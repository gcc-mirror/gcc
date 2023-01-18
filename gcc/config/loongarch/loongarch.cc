/* Subroutines used for LoongArch code generation.
   Copyright (C) 2021-2023 Free Software Foundation, Inc.
   Contributed by Loongson Ltd.
   Based on MIPS and RISC-V target for GNU compiler.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "gimple.h"
#include "cfghooks.h"
#include "df.h"
#include "tm_p.h"
#include "stringpool.h"
#include "attribs.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "insn-attr.h"
#include "output.h"
#include "alias.h"
#include "fold-const.h"
#include "varasm.h"
#include "stor-layout.h"
#include "calls.h"
#include "explow.h"
#include "expr.h"
#include "libfuncs.h"
#include "reload.h"
#include "common/common-target.h"
#include "langhooks.h"
#include "cfgrtl.h"
#include "cfganal.h"
#include "sched-int.h"
#include "gimplify.h"
#include "target-globals.h"
#include "tree-pass.h"
#include "context.h"
#include "builtins.h"
#include "rtl-iter.h"
#include "opts.h"

/* This file should be included last.  */
#include "target-def.h"

/* True if X is an UNSPEC wrapper around a SYMBOL_REF or LABEL_REF.  */
#define UNSPEC_ADDRESS_P(X)					\
  (GET_CODE (X) == UNSPEC					\
   && XINT (X, 1) >= UNSPEC_ADDRESS_FIRST			\
   && XINT (X, 1) < UNSPEC_ADDRESS_FIRST + NUM_SYMBOL_TYPES)

/* Extract the symbol or label from UNSPEC wrapper X.  */
#define UNSPEC_ADDRESS(X) XVECEXP (X, 0, 0)

/* Extract the symbol type from UNSPEC wrapper X.  */
#define UNSPEC_ADDRESS_TYPE(X) \
  ((enum loongarch_symbol_type) (XINT (X, 1) - UNSPEC_ADDRESS_FIRST))

/* True if INSN is a loongarch.md pattern or asm statement.  */
/* ???	This test exists through the compiler, perhaps it should be
   moved to rtl.h.  */
#define USEFUL_INSN_P(INSN)						\
  (NONDEBUG_INSN_P (INSN)						\
   && GET_CODE (PATTERN (INSN)) != USE					\
   && GET_CODE (PATTERN (INSN)) != CLOBBER)

/* True if bit BIT is set in VALUE.  */
#define BITSET_P(VALUE, BIT) (((VALUE) & (1 << (BIT))) != 0)

/* Classifies an address.

   ADDRESS_REG
       A natural register + offset address.  The register satisfies
       loongarch_valid_base_register_p and the offset is a const_arith_operand.

   ADDRESS_REG_REG
       A base register indexed by (optionally scaled) register.

   ADDRESS_LO_SUM
       A LO_SUM rtx.  The first operand is a valid base register and the second
       operand is a symbolic address.

   ADDRESS_CONST_INT
       A signed 16-bit constant address.

   ADDRESS_SYMBOLIC:
       A constant symbolic address.  */
enum loongarch_address_type
{
  ADDRESS_REG,
  ADDRESS_REG_REG,
  ADDRESS_LO_SUM,
  ADDRESS_CONST_INT,
  ADDRESS_SYMBOLIC
};


/* Information about an address described by loongarch_address_type.  */
struct loongarch_address_info
{
  enum loongarch_address_type type;
  rtx reg;
  rtx offset;
  enum loongarch_symbol_type symbol_type;
};

/* Method of loading instant numbers:

   METHOD_NORMAL:
     Load 0-31 bit of the immediate number.

   METHOD_LU32I:
     Load 32-51 bit of the immediate number.

   METHOD_LU52I:
     Load 52-63 bit of the immediate number.
*/
enum loongarch_load_imm_method
{
  METHOD_NORMAL,
  METHOD_LU32I,
  METHOD_LU52I
};

struct loongarch_integer_op
{
  enum rtx_code code;
  HOST_WIDE_INT value;
  /* Represent the result of the immediate count of the load instruction at
     each step.  */
  HOST_WIDE_INT curr_value;
  enum loongarch_load_imm_method method;
};

/* The largest number of operations needed to load an integer constant.
   The worst accepted case for 64-bit constants is LU12I.W,LU32I.D,LU52I.D,ORI
   or LU12I.W,LU32I.D,LU52I.D,ADDI.D DECL_ASSEMBLER_NAME.  */
#define LARCH_MAX_INTEGER_OPS 4

/* Arrays that map GCC register numbers to debugger register numbers.  */
int loongarch_dwarf_regno[FIRST_PSEUDO_REGISTER];

/* Index [M][R] is true if register R is allowed to hold a value of mode M.  */
static bool loongarch_hard_regno_mode_ok_p[MAX_MACHINE_MODE]
					  [FIRST_PSEUDO_REGISTER];

/* Index C is true if character C is a valid PRINT_OPERAND punctation
   character.  */
static bool loongarch_print_operand_punct[256];

/* Cached value of can_issue_more.  This is cached in loongarch_variable_issue
   hook and returned from loongarch_sched_reorder2.  */
static int cached_can_issue_more;

/* Index R is the smallest register class that contains register R.  */
const enum reg_class loongarch_regno_to_class[FIRST_PSEUDO_REGISTER] = {
    GR_REGS,	     GR_REGS,	      GR_REGS,	       GR_REGS,
    JIRL_REGS,       JIRL_REGS,       JIRL_REGS,       JIRL_REGS,
    JIRL_REGS,       JIRL_REGS,       JIRL_REGS,       JIRL_REGS,
    SIBCALL_REGS,    JIRL_REGS,       SIBCALL_REGS,    SIBCALL_REGS,
    SIBCALL_REGS,    SIBCALL_REGS,    SIBCALL_REGS,    SIBCALL_REGS,
    SIBCALL_REGS,    GR_REGS,	      GR_REGS,	       JIRL_REGS,
    JIRL_REGS,       JIRL_REGS,       JIRL_REGS,       JIRL_REGS,
    JIRL_REGS,       JIRL_REGS,       JIRL_REGS,       JIRL_REGS,

    FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
    FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
    FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
    FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
    FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
    FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
    FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
    FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
    FCC_REGS,	FCC_REGS,	FCC_REGS,	FCC_REGS,
    FCC_REGS,	FCC_REGS,	FCC_REGS,	FCC_REGS,
    FRAME_REGS,	FRAME_REGS
};

/* Which cost information to use.  */
static const struct loongarch_rtx_cost_data *loongarch_cost;

/* Information about a single argument.  */
struct loongarch_arg_info
{
  /* True if the argument is at least partially passed on the stack.  */
  bool stack_p;

  /* The number of integer registers allocated to this argument.  */
  unsigned int num_gprs;

  /* The offset of the first register used, provided num_gprs is nonzero.
     If passed entirely on the stack, the value is MAX_ARGS_IN_REGISTERS.  */
  unsigned int gpr_offset;

  /* The number of floating-point registers allocated to this argument.  */
  unsigned int num_fprs;

  /* The offset of the first register used, provided num_fprs is nonzero.  */
  unsigned int fpr_offset;
};

/* Invoke MACRO (COND) for each fcmp.cond.{s/d} condition.  */
#define LARCH_FP_CONDITIONS(MACRO) \
  MACRO (f),	\
  MACRO (un),	\
  MACRO (eq),	\
  MACRO (ueq),	\
  MACRO (olt),	\
  MACRO (ult),	\
  MACRO (ole),	\
  MACRO (ule),	\
  MACRO (sf),	\
  MACRO (ngle),	\
  MACRO (seq),	\
  MACRO (ngl),	\
  MACRO (lt),	\
  MACRO (nge),	\
  MACRO (le),	\
  MACRO (ngt)

/* Enumerates the codes above as LARCH_FP_COND_<X>.  */
#define DECLARE_LARCH_COND(X) LARCH_FP_COND_##X
enum loongarch_fp_condition
{
  LARCH_FP_CONDITIONS (DECLARE_LARCH_COND)
};
#undef DECLARE_LARCH_COND

/* Index X provides the string representation of LARCH_FP_COND_<X>.  */
#define STRINGIFY(X) #X
const char *const
loongarch_fp_conditions[16]= {LARCH_FP_CONDITIONS (STRINGIFY)};
#undef STRINGIFY

/* Size of guard page.  */
#define STACK_CLASH_PROTECTION_GUARD_SIZE \
  (1 << param_stack_clash_protection_guard_size)

/* Implement TARGET_FUNCTION_ARG_BOUNDARY.  Every parameter gets at
   least PARM_BOUNDARY bits of alignment, but will be given anything up
   to PREFERRED_STACK_BOUNDARY bits if the type requires it.  */

static unsigned int
loongarch_function_arg_boundary (machine_mode mode, const_tree type)
{
  unsigned int alignment;

  /* Use natural alignment if the type is not aggregate data.  */
  if (type && !AGGREGATE_TYPE_P (type))
    alignment = TYPE_ALIGN (TYPE_MAIN_VARIANT (type));
  else
    alignment = type ? TYPE_ALIGN (type) : GET_MODE_ALIGNMENT (mode);

  return MIN (PREFERRED_STACK_BOUNDARY, MAX (PARM_BOUNDARY, alignment));
}

/* If MODE represents an argument that can be passed or returned in
   floating-point registers, return the number of registers, else 0.  */

static unsigned
loongarch_pass_mode_in_fpr_p (machine_mode mode)
{
  if (GET_MODE_UNIT_SIZE (mode) <= UNITS_PER_FP_ARG)
    {
      if (GET_MODE_CLASS (mode) == MODE_FLOAT)
	return 1;

      if (GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT)
	return 2;
    }

  return 0;
}

typedef struct
{
  const_tree type;
  HOST_WIDE_INT offset;
} loongarch_aggregate_field;

/* Identify subfields of aggregates that are candidates for passing in
   floating-point registers.  */

static int
loongarch_flatten_aggregate_field (const_tree type,
				   loongarch_aggregate_field fields[2], int n,
				   HOST_WIDE_INT offset)
{
  switch (TREE_CODE (type))
    {
    case RECORD_TYPE:
      /* Can't handle incomplete types nor sizes that are not fixed.  */
      if (!COMPLETE_TYPE_P (type)
	  || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST
	  || !tree_fits_uhwi_p (TYPE_SIZE (type)))
	return -1;

      for (tree f = TYPE_FIELDS (type); f; f = DECL_CHAIN (f))
	if (TREE_CODE (f) == FIELD_DECL)
	  {
	    if (!TYPE_P (TREE_TYPE (f)))
	      return -1;

	    if (DECL_SIZE (f) && integer_zerop (DECL_SIZE (f)))
	      continue;

	    HOST_WIDE_INT pos = offset + int_byte_position (f);
	    n = loongarch_flatten_aggregate_field (TREE_TYPE (f), fields, n,
						   pos);
	    if (n < 0)
	      return -1;
	  }
      return n;

    case ARRAY_TYPE:
      {
	HOST_WIDE_INT n_elts;
	loongarch_aggregate_field subfields[2];
	tree index = TYPE_DOMAIN (type);
	tree elt_size = TYPE_SIZE_UNIT (TREE_TYPE (type));
	int n_subfields = loongarch_flatten_aggregate_field (TREE_TYPE (type),
							     subfields, 0,
							     offset);

	/* Can't handle incomplete types nor sizes that are not fixed.  */
	if (n_subfields <= 0
	    || !COMPLETE_TYPE_P (type)
	    || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST
	    || !index
	    || !TYPE_MAX_VALUE (index)
	    || !tree_fits_uhwi_p (TYPE_MAX_VALUE (index))
	    || !TYPE_MIN_VALUE (index)
	    || !tree_fits_uhwi_p (TYPE_MIN_VALUE (index))
	    || !tree_fits_uhwi_p (elt_size))
	  return -1;

	n_elts = 1 + tree_to_uhwi (TYPE_MAX_VALUE (index))
		 - tree_to_uhwi (TYPE_MIN_VALUE (index));
	gcc_assert (n_elts >= 0);

	for (HOST_WIDE_INT i = 0; i < n_elts; i++)
	  for (int j = 0; j < n_subfields; j++)
	    {
	      if (n >= 2)
		return -1;

	      fields[n] = subfields[j];
	      fields[n++].offset += i * tree_to_uhwi (elt_size);
	    }

	return n;
      }

    case COMPLEX_TYPE:
      {
	/* Complex type need consume 2 field, so n must be 0.  */
	if (n != 0)
	  return -1;

	HOST_WIDE_INT elt_size = GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (type)));

	if (elt_size <= UNITS_PER_FP_ARG)
	  {
	    fields[0].type = TREE_TYPE (type);
	    fields[0].offset = offset;
	    fields[1].type = TREE_TYPE (type);
	    fields[1].offset = offset + elt_size;

	    return 2;
	  }

	return -1;
      }

    default:
      if (n < 2
	  && ((SCALAR_FLOAT_TYPE_P (type)
	       && GET_MODE_SIZE (TYPE_MODE (type)) <= UNITS_PER_FP_ARG)
	      || (INTEGRAL_TYPE_P (type)
		  && GET_MODE_SIZE (TYPE_MODE (type)) <= UNITS_PER_WORD)))
	{
	  fields[n].type = type;
	  fields[n].offset = offset;
	  return n + 1;
	}
      else
	return -1;
    }
}

/* Identify candidate aggregates for passing in floating-point registers.
   Candidates have at most two fields after flattening.  */

static int
loongarch_flatten_aggregate_argument (const_tree type,
				      loongarch_aggregate_field fields[2])
{
  if (!type || TREE_CODE (type) != RECORD_TYPE)
    return -1;

  return loongarch_flatten_aggregate_field (type, fields, 0, 0);
}

/* See whether TYPE is a record whose fields should be returned in one or
   two floating-point registers.  If so, populate FIELDS accordingly.  */

static unsigned
loongarch_pass_aggregate_num_fpr (const_tree type,
					loongarch_aggregate_field fields[2])
{
  int n = loongarch_flatten_aggregate_argument (type, fields);

  for (int i = 0; i < n; i++)
    if (!SCALAR_FLOAT_TYPE_P (fields[i].type))
      return 0;

  return n > 0 ? n : 0;
}

/* See whether TYPE is a record whose fields should be returned in one
   floating-point register and one integer register.  If so, populate
   FIELDS accordingly.  */

static bool
loongarch_pass_aggregate_in_fpr_and_gpr_p (const_tree type,
					   loongarch_aggregate_field fields[2])
{
  unsigned num_int = 0, num_float = 0;
  int n = loongarch_flatten_aggregate_argument (type, fields);

  for (int i = 0; i < n; i++)
    {
      num_float += SCALAR_FLOAT_TYPE_P (fields[i].type);
      num_int += INTEGRAL_TYPE_P (fields[i].type);
    }

  return num_int == 1 && num_float == 1;
}

/* Return the representation of an argument passed or returned in an FPR
   when the value has mode VALUE_MODE and the type has TYPE_MODE.  The
   two modes may be different for structures like:

   struct __attribute__((packed)) foo { float f; }

   where the SFmode value "f" is passed in REGNO but the struct itself
   has mode BLKmode.  */

static rtx
loongarch_pass_fpr_single (machine_mode type_mode, unsigned regno,
			   machine_mode value_mode,
			   HOST_WIDE_INT offset)
{
  rtx x = gen_rtx_REG (value_mode, regno);

  if (type_mode != value_mode)
    {
      x = gen_rtx_EXPR_LIST (VOIDmode, x, GEN_INT (offset));
      x = gen_rtx_PARALLEL (type_mode, gen_rtvec (1, x));
    }
  return x;
}

/* Pass or return a composite value in the FPR pair REGNO and REGNO + 1.
   MODE is the mode of the composite.  MODE1 and OFFSET1 are the mode and
   byte offset for the first value, likewise MODE2 and OFFSET2 for the
   second value.  */

static rtx
loongarch_pass_fpr_pair (machine_mode mode, unsigned regno1,
			 machine_mode mode1, HOST_WIDE_INT offset1,
			 unsigned regno2, machine_mode mode2,
			 HOST_WIDE_INT offset2)
{
  return gen_rtx_PARALLEL (
    mode, gen_rtvec (2,
		     gen_rtx_EXPR_LIST (VOIDmode, gen_rtx_REG (mode1, regno1),
					GEN_INT (offset1)),
		     gen_rtx_EXPR_LIST (VOIDmode, gen_rtx_REG (mode2, regno2),
					GEN_INT (offset2))));
}

/* Fill INFO with information about a single argument, and return an
   RTL pattern to pass or return the argument.  CUM is the cumulative
   state for earlier arguments.  MODE is the mode of this argument and
   TYPE is its type (if known).  NAMED is true if this is a named
   (fixed) argument rather than a variable one.  RETURN_P is true if
   returning the argument, or false if passing the argument.  */

static rtx
loongarch_get_arg_info (struct loongarch_arg_info *info,
			const CUMULATIVE_ARGS *cum, machine_mode mode,
			const_tree type, bool named, bool return_p)
{
  unsigned num_bytes, num_words;
  unsigned fpr_base = return_p ? FP_RETURN : FP_ARG_FIRST;
  unsigned gpr_base = return_p ? GP_RETURN : GP_ARG_FIRST;
  unsigned alignment = loongarch_function_arg_boundary (mode, type);

  memset (info, 0, sizeof (*info));
  info->gpr_offset = cum->num_gprs;
  info->fpr_offset = cum->num_fprs;

  if (named)
    {
      loongarch_aggregate_field fields[2];
      unsigned fregno = fpr_base + info->fpr_offset;
      unsigned gregno = gpr_base + info->gpr_offset;

      /* Pass one- or two-element floating-point aggregates in FPRs.  */
      if ((info->num_fprs
	   = loongarch_pass_aggregate_num_fpr (type, fields))
	  && info->fpr_offset + info->num_fprs <= MAX_ARGS_IN_REGISTERS)
	switch (info->num_fprs)
	  {
	  case 1:
	    return loongarch_pass_fpr_single (mode, fregno,
					      TYPE_MODE (fields[0].type),
					      fields[0].offset);

	  case 2:
	    return loongarch_pass_fpr_pair (mode, fregno,
					    TYPE_MODE (fields[0].type),
					    fields[0].offset,
					    fregno + 1,
					    TYPE_MODE (fields[1].type),
					    fields[1].offset);

	  default:
	    gcc_unreachable ();
	  }

      /* Pass real and complex floating-point numbers in FPRs.  */
      if ((info->num_fprs = loongarch_pass_mode_in_fpr_p (mode))
	  && info->fpr_offset + info->num_fprs <= MAX_ARGS_IN_REGISTERS)
	switch (GET_MODE_CLASS (mode))
	  {
	  case MODE_FLOAT:
	    return gen_rtx_REG (mode, fregno);

	  case MODE_COMPLEX_FLOAT:
	    return loongarch_pass_fpr_pair (mode, fregno,
					    GET_MODE_INNER (mode), 0,
					    fregno + 1, GET_MODE_INNER (mode),
					    GET_MODE_UNIT_SIZE (mode));

	  default:
	    gcc_unreachable ();
	  }

      /* Pass structs with one float and one integer in an FPR and a GPR.  */
      if (loongarch_pass_aggregate_in_fpr_and_gpr_p (type, fields)
	  && info->gpr_offset < MAX_ARGS_IN_REGISTERS
	  && info->fpr_offset < MAX_ARGS_IN_REGISTERS)
	{
	  info->num_gprs = 1;
	  info->num_fprs = 1;

	  if (!SCALAR_FLOAT_TYPE_P (fields[0].type))
	    std::swap (fregno, gregno);

	  return loongarch_pass_fpr_pair (mode, fregno,
					  TYPE_MODE (fields[0].type),
					  fields[0].offset, gregno,
					  TYPE_MODE (fields[1].type),
					  fields[1].offset);
	}
    }

  /* Work out the size of the argument.  */
  num_bytes = type ? int_size_in_bytes (type) : GET_MODE_SIZE (mode);
  num_words = (num_bytes + UNITS_PER_WORD - 1) / UNITS_PER_WORD;

  /* Doubleword-aligned varargs start on an even register boundary.  */
  if (!named && num_bytes != 0 && alignment > BITS_PER_WORD)
    info->gpr_offset += info->gpr_offset & 1;

  /* Partition the argument between registers and stack.  */
  info->num_fprs = 0;
  info->num_gprs = MIN (num_words, MAX_ARGS_IN_REGISTERS - info->gpr_offset);
  info->stack_p = (num_words - info->num_gprs) != 0;

  if (info->num_gprs || return_p)
    return gen_rtx_REG (mode, gpr_base + info->gpr_offset);

  return NULL_RTX;
}

/* Implement TARGET_FUNCTION_ARG.  */

static rtx
loongarch_function_arg (cumulative_args_t cum_v, const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  struct loongarch_arg_info info;

  if (arg.end_marker_p ())
    return NULL;

  return loongarch_get_arg_info (&info, cum, arg.mode, arg.type, arg.named,
				 false);
}

/* Implement TARGET_FUNCTION_ARG_ADVANCE.  */

static void
loongarch_function_arg_advance (cumulative_args_t cum_v,
				const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  struct loongarch_arg_info info;

  loongarch_get_arg_info (&info, cum, arg.mode, arg.type, arg.named, false);

  /* Advance the register count.  This has the effect of setting
     num_gprs to MAX_ARGS_IN_REGISTERS if a doubleword-aligned
     argument required us to skip the final GPR and pass the whole
     argument on the stack.  */
  cum->num_fprs = info.fpr_offset + info.num_fprs;
  cum->num_gprs = info.gpr_offset + info.num_gprs;
}

/* Implement TARGET_ARG_PARTIAL_BYTES.  */

static int
loongarch_arg_partial_bytes (cumulative_args_t cum,
			     const function_arg_info &generic_arg)
{
  struct loongarch_arg_info arg;

  loongarch_get_arg_info (&arg, get_cumulative_args (cum), generic_arg.mode,
			  generic_arg.type, generic_arg.named, false);
  return arg.stack_p ? arg.num_gprs * UNITS_PER_WORD : 0;
}

/* Implement FUNCTION_VALUE and LIBCALL_VALUE.  For normal calls,
   VALTYPE is the return type and MODE is VOIDmode.  For libcalls,
   VALTYPE is null and MODE is the mode of the return value.  */

static rtx
loongarch_function_value_1 (const_tree type, const_tree func,
			    machine_mode mode)
{
  struct loongarch_arg_info info;
  CUMULATIVE_ARGS args;

  if (type)
    {
      int unsigned_p = TYPE_UNSIGNED (type);

      mode = TYPE_MODE (type);

      /* Since TARGET_PROMOTE_FUNCTION_MODE unconditionally promotes,
	 return values, promote the mode here too.  */
      mode = promote_function_mode (type, mode, &unsigned_p, func, 1);
    }

  memset (&args, 0, sizeof (args));
  return loongarch_get_arg_info (&info, &args, mode, type, true, true);
}


/* Implement TARGET_FUNCTION_VALUE.  */

static rtx
loongarch_function_value (const_tree valtype, const_tree fn_decl_or_type,
			  bool outgoing ATTRIBUTE_UNUSED)
{
  return loongarch_function_value_1 (valtype, fn_decl_or_type, VOIDmode);
}

/* Implement TARGET_LIBCALL_VALUE.  */

static rtx
loongarch_libcall_value (machine_mode mode, const_rtx fun ATTRIBUTE_UNUSED)
{
  return loongarch_function_value_1 (NULL_TREE, NULL_TREE, mode);
}


/* Implement TARGET_PASS_BY_REFERENCE.  */

static bool
loongarch_pass_by_reference (cumulative_args_t cum_v,
			     const function_arg_info &arg)
{
  HOST_WIDE_INT size = arg.type_size_in_bytes ();
  struct loongarch_arg_info info;
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  /* ??? std_gimplify_va_arg_expr passes NULL for cum.  Fortunately, we
     never pass variadic arguments in floating-point registers, so we can
     avoid the call to loongarch_get_arg_info in this case.  */
  if (cum != NULL)
    {
      /* Don't pass by reference if we can use a floating-point register.  */
      loongarch_get_arg_info (&info, cum, arg.mode, arg.type, arg.named,
			      false);
      if (info.num_fprs)
	return false;
    }

  /* Pass by reference if the data do not fit in two integer registers.  */
  return !IN_RANGE (size, 0, 2 * UNITS_PER_WORD);
}

/* Implement TARGET_RETURN_IN_MEMORY.  */

static bool
loongarch_return_in_memory (const_tree type,
			    const_tree fndecl ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS args;
  cumulative_args_t cum = pack_cumulative_args (&args);

  /* The rules for returning in memory are the same as for passing the
     first named argument by reference.  */
  memset (&args, 0, sizeof (args));
  function_arg_info arg (const_cast<tree> (type), /*named=*/true);
  return loongarch_pass_by_reference (cum, arg);
}

/* Implement TARGET_SETUP_INCOMING_VARARGS.  */

static void
loongarch_setup_incoming_varargs (cumulative_args_t cum,
				  const function_arg_info &arg,
				  int *pretend_size ATTRIBUTE_UNUSED,
				  int no_rtl)
{
  CUMULATIVE_ARGS local_cum;
  int gp_saved;

  /* The caller has advanced CUM up to, but not beyond, the last named
     argument.  Advance a local copy of CUM past the last "real" named
     argument, to find out how many registers are left over.  */
  local_cum = *get_cumulative_args (cum);
  if (!TYPE_NO_NAMED_ARGS_STDARG_P (TREE_TYPE (current_function_decl)))
    loongarch_function_arg_advance (pack_cumulative_args (&local_cum), arg);

  /* Found out how many registers we need to save.  */
  gp_saved = MAX_ARGS_IN_REGISTERS - local_cum.num_gprs;

  if (!no_rtl && gp_saved > 0)
    {
      rtx ptr = plus_constant (Pmode, virtual_incoming_args_rtx,
			       REG_PARM_STACK_SPACE (cfun->decl)
				 - gp_saved * UNITS_PER_WORD);
      rtx mem = gen_frame_mem (BLKmode, ptr);
      set_mem_alias_set (mem, get_varargs_alias_set ());

      move_block_from_reg (local_cum.num_gprs + GP_ARG_FIRST, mem, gp_saved);
    }
  if (REG_PARM_STACK_SPACE (cfun->decl) == 0)
    cfun->machine->varargs_size = gp_saved * UNITS_PER_WORD;
}

/* Make the last instruction frame-related and note that it performs
   the operation described by FRAME_PATTERN.  */

static void
loongarch_set_frame_expr (rtx frame_pattern)
{
  rtx insn;

  insn = get_last_insn ();
  RTX_FRAME_RELATED_P (insn) = 1;
  REG_NOTES (insn) = alloc_EXPR_LIST (REG_FRAME_RELATED_EXPR, frame_pattern,
				      REG_NOTES (insn));
}

/* Return a frame-related rtx that stores REG at MEM.
   REG must be a single register.  */

static rtx
loongarch_frame_set (rtx mem, rtx reg)
{
  rtx set = gen_rtx_SET (mem, reg);
  RTX_FRAME_RELATED_P (set) = 1;
  return set;
}

/* Return true if the current function must save register REGNO.  */

static bool
loongarch_save_reg_p (unsigned int regno)
{
  bool call_saved = !global_regs[regno] && !call_used_regs[regno];
  bool might_clobber
    = crtl->saves_all_registers || df_regs_ever_live_p (regno);

  if (call_saved && might_clobber)
    return true;

  if (regno == HARD_FRAME_POINTER_REGNUM && frame_pointer_needed)
    return true;

  if (regno == RETURN_ADDR_REGNUM && crtl->calls_eh_return)
    return true;

  return false;
}

/* Determine which GPR save/restore routine to call.  */

static unsigned
loongarch_save_libcall_count (unsigned mask)
{
  for (unsigned n = GP_REG_LAST; n > GP_REG_FIRST; n--)
    if (BITSET_P (mask, n))
      return CALLEE_SAVED_REG_NUMBER (n) + 1;
  abort ();
}

/* Populate the current function's loongarch_frame_info structure.

   LoongArch stack frames grown downward.  High addresses are at the top.

     +-------------------------------+
     |				     |
     |  incoming stack arguments     |
     |				     |
     +-------------------------------+ <-- incoming stack pointer
     |				     |
     |  callee-allocated save area   |
     |  for arguments that are       |
     |  split between registers and  |
     |  the stack		     |
     |				     |
     +-------------------------------+ <-- arg_pointer_rtx (virtual)
     |				     |
     |  callee-allocated save area   |
     |  for register varargs	     |
     |				     |
     +-------------------------------+ <-- hard_frame_pointer_rtx;
     |				     |     stack_pointer_rtx + gp_sp_offset
     |  GPR save area		     |       + UNITS_PER_WORD
     |				     |
     +-------------------------------+ <-- stack_pointer_rtx + fp_sp_offset
     |				     |       + UNITS_PER_HWVALUE
     |  FPR save area		     |
     |				     |
     +-------------------------------+ <-- frame_pointer_rtx (virtual)
     |				     |
     |  local variables		     |
     |				     |
   P +-------------------------------+
     |				     |
     |  outgoing stack arguments     |
     |				     |
     +-------------------------------+ <-- stack_pointer_rtx

   Dynamic stack allocations such as alloca insert data at point P.
   They decrease stack_pointer_rtx but leave frame_pointer_rtx and
   hard_frame_pointer_rtx unchanged.  */

static void
loongarch_compute_frame_info (void)
{
  struct loongarch_frame_info *frame;
  HOST_WIDE_INT offset;
  unsigned int regno, i, num_x_saved = 0, num_f_saved = 0;

  frame = &cfun->machine->frame;
  memset (frame, 0, sizeof (*frame));

  /* Find out which GPRs we need to save.  */
  for (regno = GP_REG_FIRST; regno <= GP_REG_LAST; regno++)
    if (loongarch_save_reg_p (regno))
      frame->mask |= 1 << (regno - GP_REG_FIRST), num_x_saved++;

  /* If this function calls eh_return, we must also save and restore the
     EH data registers.  */
  if (crtl->calls_eh_return)
    for (i = 0; (regno = EH_RETURN_DATA_REGNO (i)) != INVALID_REGNUM; i++)
      frame->mask |= 1 << (regno - GP_REG_FIRST), num_x_saved++;

  /* Find out which FPRs we need to save.  This loop must iterate over
     the same space as its companion in loongarch_for_each_saved_reg.  */
  if (TARGET_HARD_FLOAT)
    for (regno = FP_REG_FIRST; regno <= FP_REG_LAST; regno++)
      if (loongarch_save_reg_p (regno))
	frame->fmask |= 1 << (regno - FP_REG_FIRST), num_f_saved++;

  /* At the bottom of the frame are any outgoing stack arguments.  */
  offset = LARCH_STACK_ALIGN (crtl->outgoing_args_size);
  /* Next are local stack variables.  */
  offset += LARCH_STACK_ALIGN (get_frame_size ());
  /* The virtual frame pointer points above the local variables.  */
  frame->frame_pointer_offset = offset;
  /* Next are the callee-saved FPRs.  */
  if (frame->fmask)
    {
      offset += LARCH_STACK_ALIGN (num_f_saved * UNITS_PER_FP_REG);
      frame->fp_sp_offset = offset - UNITS_PER_FP_REG;
    }
  else
    frame->fp_sp_offset = offset;
  /* Next are the callee-saved GPRs.  */
  if (frame->mask)
    {
      unsigned x_save_size = LARCH_STACK_ALIGN (num_x_saved * UNITS_PER_WORD);
      unsigned num_save_restore
	= 1 + loongarch_save_libcall_count (frame->mask);

      /* Only use save/restore routines if they don't alter the stack size.  */
      if (LARCH_STACK_ALIGN (num_save_restore * UNITS_PER_WORD) == x_save_size)
	frame->save_libcall_adjustment = x_save_size;

      offset += x_save_size;
      frame->gp_sp_offset = offset - UNITS_PER_WORD;
    }
  else
    frame->gp_sp_offset = offset;
  /* The hard frame pointer points above the callee-saved GPRs.  */
  frame->hard_frame_pointer_offset = offset;
  /* Above the hard frame pointer is the callee-allocated varags save area.  */
  offset += LARCH_STACK_ALIGN (cfun->machine->varargs_size);
  /* Next is the callee-allocated area for pretend stack arguments.  */
  offset += LARCH_STACK_ALIGN (crtl->args.pretend_args_size);
  /* Arg pointer must be below pretend args, but must be above alignment
     padding.  */
  frame->arg_pointer_offset = offset - crtl->args.pretend_args_size;
  frame->total_size = offset;
  /* Next points the incoming stack pointer and any incoming arguments.  */

  /* Only use save/restore routines when the GPRs are atop the frame.  */
  if (frame->hard_frame_pointer_offset != frame->total_size)
    frame->save_libcall_adjustment = 0;
}

/* Implement INITIAL_ELIMINATION_OFFSET.  FROM is either the frame pointer
   or argument pointer.  TO is either the stack pointer or hard frame
   pointer.  */

HOST_WIDE_INT
loongarch_initial_elimination_offset (int from, int to)
{
  HOST_WIDE_INT src, dest;

  loongarch_compute_frame_info ();

  if (to == HARD_FRAME_POINTER_REGNUM)
    dest = cfun->machine->frame.hard_frame_pointer_offset;
  else if (to == STACK_POINTER_REGNUM)
    dest = 0; /* The stack pointer is the base of all offsets, hence 0.  */
  else
    gcc_unreachable ();

  if (from == FRAME_POINTER_REGNUM)
    src = cfun->machine->frame.frame_pointer_offset;
  else if (from == ARG_POINTER_REGNUM)
    src = cfun->machine->frame.arg_pointer_offset;
  else
    gcc_unreachable ();

  return src - dest;
}

/* A function to save or store a register.  The first argument is the
   register and the second is the stack slot.  */
typedef void (*loongarch_save_restore_fn) (rtx, rtx);

/* Use FN to save or restore register REGNO.  MODE is the register's
   mode and OFFSET is the offset of its save slot from the current
   stack pointer.  */

static void
loongarch_save_restore_reg (machine_mode mode, int regno, HOST_WIDE_INT offset,
			    loongarch_save_restore_fn fn)
{
  rtx mem;

  mem = gen_frame_mem (mode, plus_constant (Pmode, stack_pointer_rtx, offset));
  fn (gen_rtx_REG (mode, regno), mem);
}

/* Call FN for each register that is saved by the current function.
   SP_OFFSET is the offset of the current stack pointer from the start
   of the frame.  */

static void
loongarch_for_each_saved_reg (HOST_WIDE_INT sp_offset,
			      loongarch_save_restore_fn fn)
{
  HOST_WIDE_INT offset;

  /* Save the link register and s-registers.  */
  offset = cfun->machine->frame.gp_sp_offset - sp_offset;
  for (int regno = GP_REG_FIRST; regno <= GP_REG_LAST; regno++)
    if (BITSET_P (cfun->machine->frame.mask, regno - GP_REG_FIRST))
      {
	loongarch_save_restore_reg (word_mode, regno, offset, fn);
	offset -= UNITS_PER_WORD;
      }

  /* This loop must iterate over the same space as its companion in
     loongarch_compute_frame_info.  */
  offset = cfun->machine->frame.fp_sp_offset - sp_offset;
  for (int regno = FP_REG_FIRST; regno <= FP_REG_LAST; regno++)
    if (BITSET_P (cfun->machine->frame.fmask, regno - FP_REG_FIRST))
      {
	machine_mode mode = TARGET_DOUBLE_FLOAT ? DFmode : SFmode;

	loongarch_save_restore_reg (mode, regno, offset, fn);
	offset -= GET_MODE_SIZE (mode);
      }
}

/* Emit a move from SRC to DEST.  Assume that the move expanders can
   handle all moves if !can_create_pseudo_p ().  The distinction is
   important because, unlike emit_move_insn, the move expanders know
   how to force Pmode objects into the constant pool even when the
   constant pool address is not itself legitimate.  */

rtx
loongarch_emit_move (rtx dest, rtx src)
{
  return (can_create_pseudo_p () ? emit_move_insn (dest, src)
				 : emit_move_insn_1 (dest, src));
}

/* Save register REG to MEM.  Make the instruction frame-related.  */

static void
loongarch_save_reg (rtx reg, rtx mem)
{
  loongarch_emit_move (mem, reg);
  loongarch_set_frame_expr (loongarch_frame_set (mem, reg));
}

/* Restore register REG from MEM.  */

static void
loongarch_restore_reg (rtx reg, rtx mem)
{
  rtx insn = loongarch_emit_move (reg, mem);
  rtx dwarf = NULL_RTX;
  dwarf = alloc_reg_note (REG_CFA_RESTORE, reg, dwarf);
  REG_NOTES (insn) = dwarf;

  RTX_FRAME_RELATED_P (insn) = 1;
}

/* For stack frames that can't be allocated with a single ADDI instruction,
   compute the best value to initially allocate.  It must at a minimum
   allocate enough space to spill the callee-saved registers.  */

static HOST_WIDE_INT
loongarch_first_stack_step (struct loongarch_frame_info *frame)
{
  HOST_WIDE_INT min_first_step
    = LARCH_STACK_ALIGN (frame->total_size - frame->fp_sp_offset);

  /* When stack checking is required, if the sum of frame->total_size
     and stack_check_protect is greater than stack clash protection guard
     size, then return min_first_step.  */
  if (flag_stack_check == STATIC_BUILTIN_STACK_CHECK
      || (flag_stack_clash_protection
	  && frame->total_size > STACK_CLASH_PROTECTION_GUARD_SIZE))
    return min_first_step;

  if (IMM12_OPERAND (frame->total_size))
    return frame->total_size;

  HOST_WIDE_INT max_first_step = IMM_REACH / 2 - PREFERRED_STACK_BOUNDARY / 8;
  HOST_WIDE_INT min_second_step = frame->total_size - max_first_step;
  gcc_assert (min_first_step <= max_first_step);

  /* As an optimization, use the least-significant bits of the total frame
     size, so that the second adjustment step is just LU12I + ADD.  */
  if (!IMM12_OPERAND (min_second_step)
      && frame->total_size % IMM_REACH < IMM_REACH / 2
      && frame->total_size % IMM_REACH >= min_first_step)
    return frame->total_size % IMM_REACH;

  return max_first_step;
}

static void
loongarch_emit_stack_tie (void)
{
  emit_insn (gen_stack_tie (Pmode, stack_pointer_rtx, hard_frame_pointer_rtx));
}

#define PROBE_INTERVAL (1 << STACK_CHECK_PROBE_INTERVAL_EXP)

#if PROBE_INTERVAL > 16384
#error Cannot use indexed addressing mode for stack probing
#endif

/* Emit code to probe a range of stack addresses from FIRST to FIRST+SIZE,
   inclusive.  These are offsets from the current stack pointer.  */

static void
loongarch_emit_probe_stack_range (HOST_WIDE_INT first, HOST_WIDE_INT size)
{
  HOST_WIDE_INT rounded_size;
  HOST_WIDE_INT interval;

  if (flag_stack_clash_protection)
    interval = STACK_CLASH_PROTECTION_GUARD_SIZE;
  else
    interval = PROBE_INTERVAL;

  rtx r12 = LARCH_PROLOGUE_TEMP2 (Pmode);
  rtx r14 = LARCH_PROLOGUE_TEMP3 (Pmode);

  size = size + first;

  /* Sanity check for the addressing mode we're going to use.  */
  gcc_assert (first <= 16384);

  /* Step 1: round SIZE to the previous multiple of the interval.  */

  rounded_size = ROUND_DOWN (size, interval);

  /* Step 2: compute initial and final value of the loop counter.  */

  emit_move_insn (r14, GEN_INT (interval));

  /* If rounded_size is zero, it means that the space requested by
     the local variable is less than the interval, and there is no
     need to display and detect the allocated space.  */
  if (rounded_size != 0)
    {
      /* Step 3: the loop

	 do
	 {
	 TEST_ADDR = TEST_ADDR + PROBE_INTERVAL
	 probe at TEST_ADDR
	 }
	 while (TEST_ADDR != LAST_ADDR)

	 probes at FIRST + N * PROBE_INTERVAL for values of N from 1
	 until it is equal to ROUNDED_SIZE.  */

      if (rounded_size <= STACK_CLASH_MAX_UNROLL_PAGES * interval)
	{
	  for (HOST_WIDE_INT i = 0; i < rounded_size; i += interval)
	    {
	      emit_insn (gen_rtx_SET (stack_pointer_rtx,
				      gen_rtx_MINUS (Pmode,
						     stack_pointer_rtx,
						     r14)));
	      emit_move_insn (gen_rtx_MEM (Pmode,
					   gen_rtx_PLUS (Pmode,
							 stack_pointer_rtx,
							 const0_rtx)),
			      const0_rtx);
	      emit_insn (gen_blockage ());
	    }
	  dump_stack_clash_frame_info (PROBE_INLINE, size != rounded_size);
	}
      else
	{
	  emit_move_insn (r12, GEN_INT (rounded_size));
	  emit_insn (gen_rtx_SET (r12,
				  gen_rtx_MINUS (Pmode,
						 stack_pointer_rtx,
						 r12)));

	  emit_insn (gen_probe_stack_range (Pmode, stack_pointer_rtx,
					    stack_pointer_rtx, r12, r14));
	  emit_insn (gen_blockage ());
	  dump_stack_clash_frame_info (PROBE_LOOP, size != rounded_size);
	}
    }
  else
    dump_stack_clash_frame_info (NO_PROBE_SMALL_FRAME, true);


  /* Step 4: probe at FIRST + SIZE if we cannot assert at compile-time
     that SIZE is equal to ROUNDED_SIZE.  */

  if (size != rounded_size)
    {
      if (size - rounded_size >= 2048)
	{
	  emit_move_insn (r14, GEN_INT (size - rounded_size));
	  emit_insn (gen_rtx_SET (stack_pointer_rtx,
				  gen_rtx_MINUS (Pmode,
						 stack_pointer_rtx,
						 r14)));
	}
      else
	emit_insn (gen_rtx_SET (stack_pointer_rtx,
				gen_rtx_PLUS (Pmode,
					      stack_pointer_rtx,
					      GEN_INT (rounded_size - size))));
    }

  if (first)
    {
      emit_move_insn (r12, GEN_INT (first));
      emit_insn (gen_rtx_SET (stack_pointer_rtx,
			      gen_rtx_PLUS (Pmode,
					    stack_pointer_rtx, r12)));
    }
  /* Make sure nothing is scheduled before we are done.  */
  emit_insn (gen_blockage ());
}

/* Probe a range of stack addresses from REG1 to REG2 inclusive.  These are
   absolute addresses.  */
const char *
loongarch_output_probe_stack_range (rtx reg1, rtx reg2, rtx reg3)
{
  static int labelno = 0;
  char loop_lab[32], tmp[64];
  rtx xops[3];

  ASM_GENERATE_INTERNAL_LABEL (loop_lab, "LPSRL", labelno++);

  /* Loop.  */
  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, loop_lab);

  /* TEST_ADDR = TEST_ADDR + PROBE_INTERVAL.  */
  xops[0] = reg1;
  xops[2] = reg3;
  if (TARGET_64BIT)
    output_asm_insn ("sub.d\t%0,%0,%2", xops);
  else
    output_asm_insn ("sub.w\t%0,%0,%2", xops);

  /* Probe at TEST_ADDR, test if TEST_ADDR == LAST_ADDR and branch.  */
  xops[1] = reg2;
  strcpy (tmp, "bne\t%0,%1,");
  if (TARGET_64BIT)
    output_asm_insn ("st.d\t$r0,%0,0", xops);
  else
    output_asm_insn ("st.w\t$r0,%0,0", xops);
  output_asm_insn (strcat (tmp, &loop_lab[1]), xops);

  return "";
}

/* Expand the "prologue" pattern.  */

void
loongarch_expand_prologue (void)
{
  struct loongarch_frame_info *frame = &cfun->machine->frame;
  HOST_WIDE_INT size = frame->total_size;
  rtx insn;

  if (flag_stack_usage_info)
    current_function_static_stack_size = size;

  /* Save the registers.  */
  if ((frame->mask | frame->fmask) != 0)
    {
      HOST_WIDE_INT step1 = MIN (size, loongarch_first_stack_step (frame));

      insn = gen_add3_insn (stack_pointer_rtx, stack_pointer_rtx,
			    GEN_INT (-step1));
      RTX_FRAME_RELATED_P (emit_insn (insn)) = 1;
      size -= step1;
      loongarch_for_each_saved_reg (size, loongarch_save_reg);
    }

  /* Set up the frame pointer, if we're using one.  */
  if (frame_pointer_needed)
    {
      insn = gen_add3_insn (hard_frame_pointer_rtx, stack_pointer_rtx,
			    GEN_INT (frame->hard_frame_pointer_offset - size));
      RTX_FRAME_RELATED_P (emit_insn (insn)) = 1;

      loongarch_emit_stack_tie ();
    }

  if (flag_stack_check == STATIC_BUILTIN_STACK_CHECK
       || flag_stack_clash_protection)
    {
      HOST_WIDE_INT first = get_stack_check_protect ();

      if (frame->total_size == 0)
	{
	  /* do nothing.  */
	  dump_stack_clash_frame_info (NO_PROBE_NO_FRAME, false);
	  return;
	}

      if (crtl->is_leaf && !cfun->calls_alloca)
	{
	  HOST_WIDE_INT interval;

	  if (flag_stack_clash_protection)
	    interval = STACK_CLASH_PROTECTION_GUARD_SIZE;
	  else
	    interval = PROBE_INTERVAL;

	  if (size > interval && size > first)
	    loongarch_emit_probe_stack_range (first, size - first);
	  else
	    loongarch_emit_probe_stack_range (first, size);
	}
      else
	loongarch_emit_probe_stack_range (first, size);

      if (size > 0)
	{
	  /* Describe the effect of the previous instructions.  */
	  insn = plus_constant (Pmode, stack_pointer_rtx, -size);
	  insn = gen_rtx_SET (stack_pointer_rtx, insn);
	  loongarch_set_frame_expr (insn);
	}
      return;
    }

  if (size > 0)
    {
      if (IMM12_OPERAND (-size))
	{
	  insn = gen_add3_insn (stack_pointer_rtx, stack_pointer_rtx,
				GEN_INT (-size));
	  RTX_FRAME_RELATED_P (emit_insn (insn)) = 1;
	}
      else
	{
	  loongarch_emit_move (LARCH_PROLOGUE_TEMP (Pmode),
			       GEN_INT (-size));
	  emit_insn (gen_add3_insn (stack_pointer_rtx, stack_pointer_rtx,
				    LARCH_PROLOGUE_TEMP (Pmode)));

	  /* Describe the effect of the previous instructions.  */
	  insn = plus_constant (Pmode, stack_pointer_rtx, -size);
	  insn = gen_rtx_SET (stack_pointer_rtx, insn);
	  loongarch_set_frame_expr (insn);
	}
    }
}

/* Return nonzero if this function is known to have a null epilogue.
   This allows the optimizer to omit jumps to jumps if no stack
   was created.  */

bool
loongarch_can_use_return_insn (void)
{
  return reload_completed && cfun->machine->frame.total_size == 0;
}

/* Expand an "epilogue" or "sibcall_epilogue" pattern; SIBCALL_P
   says which.  */

void
loongarch_expand_epilogue (bool sibcall_p)
{
  /* Split the frame into two.  STEP1 is the amount of stack we should
     deallocate before restoring the registers.  STEP2 is the amount we
     should deallocate afterwards.

     Start off by assuming that no registers need to be restored.  */
  struct loongarch_frame_info *frame = &cfun->machine->frame;
  HOST_WIDE_INT step1 = frame->total_size;
  HOST_WIDE_INT step2 = 0;
  rtx ra = gen_rtx_REG (Pmode, RETURN_ADDR_REGNUM);
  rtx insn;

  /* We need to add memory barrier to prevent read from deallocated stack.  */
  bool need_barrier_p
    = (get_frame_size () + cfun->machine->frame.arg_pointer_offset) != 0;

  if (!sibcall_p && loongarch_can_use_return_insn ())
    {
      emit_jump_insn (gen_return ());
      return;
    }

  /* Move past any dynamic stack allocations.  */
  if (cfun->calls_alloca)
    {
      /* Emit a barrier to prevent loads from a deallocated stack.  */
      loongarch_emit_stack_tie ();
      need_barrier_p = false;

      rtx adjust = GEN_INT (-frame->hard_frame_pointer_offset);
      if (!IMM12_OPERAND (INTVAL (adjust)))
	{
	  loongarch_emit_move (LARCH_PROLOGUE_TEMP (Pmode), adjust);
	  adjust = LARCH_PROLOGUE_TEMP (Pmode);
	}

      insn = emit_insn (gen_add3_insn (stack_pointer_rtx,
				       hard_frame_pointer_rtx,
				       adjust));

      rtx dwarf = NULL_RTX;
      rtx minus_offset = GEN_INT (-frame->hard_frame_pointer_offset);
      rtx cfa_adjust_value = gen_rtx_PLUS (Pmode,
					   hard_frame_pointer_rtx,
					   minus_offset);

      rtx cfa_adjust_rtx = gen_rtx_SET (stack_pointer_rtx, cfa_adjust_value);
      dwarf = alloc_reg_note (REG_CFA_ADJUST_CFA, cfa_adjust_rtx, dwarf);
      RTX_FRAME_RELATED_P (insn) = 1;

      REG_NOTES (insn) = dwarf;
    }

  /* If we need to restore registers, deallocate as much stack as
     possible in the second step without going out of range.  */
  if ((frame->mask | frame->fmask) != 0)
    {
      step2 = loongarch_first_stack_step (frame);
      step1 -= step2;
    }

  /* Set TARGET to BASE + STEP1.  */
  if (step1 > 0)
    {
      /* Emit a barrier to prevent loads from a deallocated stack.  */
      loongarch_emit_stack_tie ();
      need_barrier_p = false;

      /* Get an rtx for STEP1 that we can add to BASE.  */
      rtx adjust = GEN_INT (step1);
      if (!IMM12_OPERAND (step1))
	{
	  loongarch_emit_move (LARCH_PROLOGUE_TEMP (Pmode), adjust);
	  adjust = LARCH_PROLOGUE_TEMP (Pmode);
	}

      insn = emit_insn (gen_add3_insn (stack_pointer_rtx,
				       stack_pointer_rtx,
				       adjust));

      rtx dwarf = NULL_RTX;
      rtx cfa_adjust_rtx = gen_rtx_PLUS (Pmode, stack_pointer_rtx,
					 GEN_INT (step2));

      dwarf = alloc_reg_note (REG_CFA_DEF_CFA, cfa_adjust_rtx, dwarf);
      RTX_FRAME_RELATED_P (insn) = 1;

      REG_NOTES (insn) = dwarf;
    }

  /* Restore the registers.  */
  loongarch_for_each_saved_reg (frame->total_size - step2,
				loongarch_restore_reg);

  if (need_barrier_p)
    loongarch_emit_stack_tie ();

  /* Deallocate the final bit of the frame.  */
  if (step2 > 0)
    {
      insn = emit_insn (gen_add3_insn (stack_pointer_rtx,
				       stack_pointer_rtx,
				       GEN_INT (step2)));

      rtx dwarf = NULL_RTX;
      rtx cfa_adjust_rtx = gen_rtx_PLUS (Pmode, stack_pointer_rtx, const0_rtx);
      dwarf = alloc_reg_note (REG_CFA_DEF_CFA, cfa_adjust_rtx, dwarf);
      RTX_FRAME_RELATED_P (insn) = 1;

      REG_NOTES (insn) = dwarf;
    }

  /* Add in the __builtin_eh_return stack adjustment.  */
  if (crtl->calls_eh_return)
    emit_insn (gen_add3_insn (stack_pointer_rtx, stack_pointer_rtx,
			      EH_RETURN_STACKADJ_RTX));

  if (!sibcall_p)
    emit_jump_insn (gen_simple_return_internal (ra));
}

#define LU32I_B (0xfffffULL << 32)
#define LU52I_B (0xfffULL << 52)

/* Fill CODES with a sequence of rtl operations to load VALUE.
   Return the number of operations needed.  */

static unsigned int
loongarch_build_integer (struct loongarch_integer_op *codes,
			 HOST_WIDE_INT value)

{
  unsigned int cost = 0;

  /* Get the lower 32 bits of the value.  */
  HOST_WIDE_INT low_part = (int32_t)value;

  if (IMM12_OPERAND (low_part) || IMM12_OPERAND_UNSIGNED (low_part))
    {
      /* The value of the lower 32 bit be loaded with one instruction.
	 lu12i.w.  */
      codes[cost].code = UNKNOWN;
      codes[cost].method = METHOD_NORMAL;
      codes[cost].value = low_part;
      codes[cost].curr_value = low_part;
      cost++;
    }
  else
    {
      /* lu12i.w + ior.  */
      codes[cost].code = UNKNOWN;
      codes[cost].method = METHOD_NORMAL;
      codes[cost].value = low_part & ~(IMM_REACH - 1);
      codes[cost].curr_value = codes[cost].value;
      cost++;
      HOST_WIDE_INT iorv = low_part & (IMM_REACH - 1);
      if (iorv != 0)
	{
	  codes[cost].code = IOR;
	  codes[cost].method = METHOD_NORMAL;
	  codes[cost].value = iorv;
	  codes[cost].curr_value = low_part;
	  cost++;
	}
    }

  if (TARGET_64BIT)
    {
      bool lu32i[2] = {(value & LU32I_B) == 0, (value & LU32I_B) == LU32I_B};
      bool lu52i[2] = {(value & LU52I_B) == 0, (value & LU52I_B) == LU52I_B};

      int sign31 = (value & (HOST_WIDE_INT_1U << 31)) >> 31;
      int sign51 = (value & (HOST_WIDE_INT_1U << 51)) >> 51;
      /* Determine whether the upper 32 bits are sign-extended from the lower
	 32 bits. If it is, the instructions to load the high order can be
	 ommitted.  */
      if (lu32i[sign31] && lu52i[sign31])
	return cost;
      /* Determine whether bits 32-51 are sign-extended from the lower 32
	 bits. If so, directly load 52-63 bits.  */
      else if (lu32i[sign31])
	{
	  codes[cost].method = METHOD_LU52I;
	  codes[cost].value = value & LU52I_B;
	  codes[cost].curr_value = value;
	  return cost + 1;
	}

      codes[cost].method = METHOD_LU32I;
      codes[cost].value = (value & LU32I_B) | (sign51 ? LU52I_B : 0);
      codes[cost].curr_value = (value & 0xfffffffffffff)
	| (sign51 ? LU52I_B : 0);
      cost++;

      /* Determine whether the 52-61 bits are sign-extended from the low order,
	 and if not, load the 52-61 bits.  */
      if (!lu52i[(value & (HOST_WIDE_INT_1U << 51)) >> 51])
	{
	  codes[cost].method = METHOD_LU52I;
	  codes[cost].value = value & LU52I_B;
	  codes[cost].curr_value = value;
	  cost++;
	}
    }

  gcc_assert (cost <= LARCH_MAX_INTEGER_OPS);

  return cost;
}

/* Fill CODES with a sequence of rtl operations to load VALUE.
   Return the number of operations needed.
   Split interger in loongarch_output_move.  */

static unsigned int
loongarch_integer_cost (HOST_WIDE_INT value)
{
  struct loongarch_integer_op codes[LARCH_MAX_INTEGER_OPS];
  return loongarch_build_integer (codes, value);
}

/* Implement TARGET_LEGITIMATE_CONSTANT_P.  */

static bool
loongarch_legitimate_constant_p (machine_mode mode ATTRIBUTE_UNUSED, rtx x)
{
  return loongarch_const_insns (x) > 0;
}

/* Return true if X is a thread-local symbol.  */

static bool
loongarch_tls_symbol_p (rtx x)
{
  return SYMBOL_REF_P (x) && SYMBOL_REF_TLS_MODEL (x) != 0;
}

/* Return true if SYMBOL_REF X is associated with a global symbol
   (in the STB_GLOBAL sense).  */

bool
loongarch_global_symbol_p (const_rtx x)
{
  if (LABEL_REF_P (x))
    return false;

  const_tree decl = SYMBOL_REF_DECL (x);

  if (!decl)
    return !SYMBOL_REF_LOCAL_P (x) || SYMBOL_REF_EXTERNAL_P (x);

  /* Weakref symbols are not TREE_PUBLIC, but their targets are global
     or weak symbols.  Relocations in the object file will be against
     the target symbol, so it's that symbol's binding that matters here.  */
  return DECL_P (decl) && (TREE_PUBLIC (decl) || DECL_WEAK (decl));
}

bool
loongarch_global_symbol_noweak_p (const_rtx x)
{
  if (LABEL_REF_P (x))
    return false;

  const_tree decl = SYMBOL_REF_DECL (x);

  if (!decl)
    return !SYMBOL_REF_LOCAL_P (x) || SYMBOL_REF_EXTERNAL_P (x);

  return DECL_P (decl) && TREE_PUBLIC (decl);
}

bool
loongarch_weak_symbol_p (const_rtx x)
{
  const_tree decl;
  if (LABEL_REF_P (x) || !(decl = SYMBOL_REF_DECL (x)))
    return false;
  return DECL_P (decl) && DECL_WEAK (decl);
}

/* Return true if SYMBOL_REF X binds locally.  */

bool
loongarch_symbol_binds_local_p (const_rtx x)
{
  if (TARGET_DIRECT_EXTERN_ACCESS)
    return true;

  if (SYMBOL_REF_P (x))
    return (SYMBOL_REF_DECL (x)
	    ? targetm.binds_local_p (SYMBOL_REF_DECL (x))
	    : SYMBOL_REF_LOCAL_P (x));
  else
    return false;
}

/* Return true if rtx constants of mode MODE should be put into a small
   data section.  */

static bool
loongarch_rtx_constant_in_small_data_p (machine_mode mode)
{
  return (GET_MODE_SIZE (mode) <= g_switch_value);
}

/* Return the method that should be used to access SYMBOL_REF or
   LABEL_REF X.  */

static enum loongarch_symbol_type
loongarch_classify_symbol (const_rtx x)
{
  enum loongarch_symbol_type pcrel =
    TARGET_CMODEL_EXTREME ? SYMBOL_PCREL64 : SYMBOL_PCREL;

  if (!SYMBOL_REF_P (x))
    return pcrel;

  if (SYMBOL_REF_TLS_MODEL (x))
    return SYMBOL_TLS;

  if (!loongarch_symbol_binds_local_p (x))
    return SYMBOL_GOT_DISP;

  tree t = SYMBOL_REF_DECL (x);
  if (!t)
    return pcrel;

  t = lookup_attribute ("model", DECL_ATTRIBUTES (t));
  if (!t)
    return pcrel;

  t = TREE_VALUE (TREE_VALUE (t));

  /* loongarch_handle_model_attribute should reject other values.  */
  gcc_assert (TREE_CODE (t) == STRING_CST);

  const char *model = TREE_STRING_POINTER (t);
  if (strcmp (model, "normal") == 0)
    return SYMBOL_PCREL;
  if (strcmp (model, "extreme") == 0)
    return SYMBOL_PCREL64;

  /* loongarch_handle_model_attribute should reject unknown model
     name.  */
  gcc_unreachable ();
}

/* Classify the base of symbolic expression X, given that X appears in
   context CONTEXT.  */

static enum loongarch_symbol_type
loongarch_classify_symbolic_expression (rtx x)
{
  rtx offset;

  split_const (x, &x, &offset);
  if (UNSPEC_ADDRESS_P (x))
    return UNSPEC_ADDRESS_TYPE (x);

  return loongarch_classify_symbol (x);
}

/* Return true if X is a symbolic constant.  If it is,
   store the type of the symbol in *SYMBOL_TYPE.  */

bool
loongarch_symbolic_constant_p (rtx x, enum loongarch_symbol_type *symbol_type)
{
  rtx offset;

  split_const (x, &x, &offset);
  if (UNSPEC_ADDRESS_P (x))
    {
      *symbol_type = UNSPEC_ADDRESS_TYPE (x);
      x = UNSPEC_ADDRESS (x);
    }
  else if (SYMBOL_REF_P (x) || LABEL_REF_P (x))
    {
      *symbol_type = loongarch_classify_symbol (x);
      if (*symbol_type == SYMBOL_TLS)
	return true;
    }
  else
    return false;

  if (offset == const0_rtx)
    return true;

  /* Check whether a nonzero offset is valid for the underlying
     relocations.  */
  switch (*symbol_type)
    {
    case SYMBOL_TLS_IE:
    case SYMBOL_TLS_LE:
    case SYMBOL_TLSGD:
    case SYMBOL_TLSLDM:
    case SYMBOL_PCREL:
    case SYMBOL_PCREL64:
      /* GAS rejects offsets outside the range [-2^31, 2^31-1].  */
      return sext_hwi (INTVAL (offset), 32) == INTVAL (offset);

    case SYMBOL_GOT_DISP:
    case SYMBOL_TLS:
      return false;
    }
  gcc_unreachable ();
}

/* Returns the number of instructions necessary to reference a symbol.  */

static int
loongarch_symbol_insns (enum loongarch_symbol_type type, machine_mode mode)
{
  switch (type)
    {
    case SYMBOL_GOT_DISP:
      /* The constant will have to be loaded from the GOT before it
	 is used in an address.  */
      if (!TARGET_EXPLICIT_RELOCS && mode != MAX_MACHINE_MODE)
	return 0;

      return 3;

    case SYMBOL_PCREL:
    case SYMBOL_TLS_IE:
    case SYMBOL_TLS_LE:
      return 2;

    case SYMBOL_TLSGD:
    case SYMBOL_TLSLDM:
      return 3;

    case SYMBOL_PCREL64:
      return 5;

    case SYMBOL_TLS:
      /* We don't treat a bare TLS symbol as a constant.  */
      return 0;
    }
  gcc_unreachable ();
}

/* Implement TARGET_CANNOT_FORCE_CONST_MEM.  */

static bool
loongarch_cannot_force_const_mem (machine_mode mode, rtx x)
{
  enum loongarch_symbol_type type;
  rtx base, offset;

  /* As an optimization, reject constants that loongarch_legitimize_move
     can expand inline.

     Suppose we have a multi-instruction sequence that loads constant C
     into register R.  If R does not get allocated a hard register, and
     R is used in an operand that allows both registers and memory
     references, reload will consider forcing C into memory and using
     one of the instruction's memory alternatives.  Returning false
     here will force it to use an input reload instead.  */
  if (CONST_INT_P (x) && loongarch_legitimate_constant_p (mode, x))
    return true;

  split_const (x, &base, &offset);
  if (loongarch_symbolic_constant_p (base, &type))
    {
      /* The same optimization as for CONST_INT.  */
      if (IMM12_INT (offset)
	  && loongarch_symbol_insns (type, MAX_MACHINE_MODE) > 0)
	return true;
    }

  /* TLS symbols must be computed by loongarch_legitimize_move.  */
  if (tls_referenced_p (x))
    return true;

  return false;
}

/* Return true if register REGNO is a valid base register for mode MODE.
   STRICT_P is true if REG_OK_STRICT is in effect.  */

int
loongarch_regno_mode_ok_for_base_p (int regno,
				    machine_mode mode ATTRIBUTE_UNUSED,
				    bool strict_p)
{
  if (!HARD_REGISTER_NUM_P (regno))
    {
      if (!strict_p)
	return true;
      regno = reg_renumber[regno];
    }

  /* These fake registers will be eliminated to either the stack or
     hard frame pointer, both of which are usually valid base registers.
     Reload deals with the cases where the eliminated form isn't valid.  */
  if (regno == ARG_POINTER_REGNUM || regno == FRAME_POINTER_REGNUM)
    return true;

  return GP_REG_P (regno);
}

/* Return true if X is a valid base register for mode MODE.
   STRICT_P is true if REG_OK_STRICT is in effect.  */

static bool
loongarch_valid_base_register_p (rtx x, machine_mode mode, bool strict_p)
{
  if (!strict_p && SUBREG_P (x))
    x = SUBREG_REG (x);

  return (REG_P (x)
	  && loongarch_regno_mode_ok_for_base_p (REGNO (x), mode, strict_p));
}

/* Return true if, for every base register BASE_REG, (plus BASE_REG X)
   can address a value of mode MODE.  */

static bool
loongarch_valid_offset_p (rtx x, machine_mode mode)
{
  /* Check that X is a signed 12-bit number,
     or check that X is a signed 16-bit number
     and offset 4 byte aligned.  */
  if (!(const_arith_operand (x, Pmode)
	|| ((mode == E_SImode || mode == E_DImode)
	    && const_imm16_operand (x, Pmode)
	    && (loongarch_signed_immediate_p (INTVAL (x), 14, 2)))))
    return false;

  /* We may need to split multiword moves, so make sure that every word
     is accessible.  */
  if (GET_MODE_SIZE (mode) > UNITS_PER_WORD
      && !IMM12_OPERAND (INTVAL (x) + GET_MODE_SIZE (mode) - UNITS_PER_WORD))
    return false;

  return true;
}

/* Should a symbol of type SYMBOL_TYPE should be split in two or more?  */

bool
loongarch_split_symbol_type (enum loongarch_symbol_type symbol_type)
{
  switch (symbol_type)
    {
    case SYMBOL_PCREL:
    case SYMBOL_PCREL64:
    case SYMBOL_GOT_DISP:
    case SYMBOL_TLS_IE:
    case SYMBOL_TLS_LE:
    case SYMBOL_TLSGD:
    case SYMBOL_TLSLDM:
      return true;

    case SYMBOL_TLS:
      return false;

    default:
      gcc_unreachable ();
    }
}

/* Return true if a LO_SUM can address a value of mode MODE when the
   LO_SUM symbol has type SYMBOL_TYPE.  */

static bool
loongarch_valid_lo_sum_p (enum loongarch_symbol_type symbol_type,
			  machine_mode mode, rtx x)
{
  int align, size;

  /* Check that symbols of type SYMBOL_TYPE can be used to access values
     of mode MODE.  */
  if (loongarch_symbol_insns (symbol_type, mode) == 0)
    return false;

  /* Check that there is a known low-part relocation.  */
  if (!loongarch_split_symbol_type (symbol_type))
    return false;

  /* We can't tell size or alignment when we have BLKmode, so try extracing a
     decl from the symbol if possible.  */
  if (mode == BLKmode)
    {
      rtx offset;

      /* Extract the symbol from the LO_SUM operand, if any.  */
      split_const (x, &x, &offset);

      /* Might be a CODE_LABEL.  We can compute align but not size for that,
	 so don't bother trying to handle it.  */
      if (!SYMBOL_REF_P (x))
	return false;

      /* Use worst case assumptions if we don't have a SYMBOL_REF_DECL.  */
      align = (SYMBOL_REF_DECL (x)
	       ? DECL_ALIGN (SYMBOL_REF_DECL (x))
	       : 1);
      size = (SYMBOL_REF_DECL (x) && DECL_SIZE (SYMBOL_REF_DECL (x))
	      ? tree_to_uhwi (DECL_SIZE (SYMBOL_REF_DECL (x)))
	      : 2*BITS_PER_WORD);
    }
  else
    {
      align = GET_MODE_ALIGNMENT (mode);
      size = GET_MODE_BITSIZE (mode);
    }

  /* We may need to split multiword moves, so make sure that each word
     can be accessed without inducing a carry.  */
  if (size > BITS_PER_WORD
      && (!TARGET_STRICT_ALIGN || size > align))
    return false;

  return true;
}

static bool
loongarch_valid_index_p (struct loongarch_address_info *info, rtx x,
			  machine_mode mode, bool strict_p)
{
  rtx index;

  if ((REG_P (x) || SUBREG_P (x))
      && GET_MODE (x) == Pmode)
    {
      index = x;
    }
  else
    return false;

  if (!strict_p
      && SUBREG_P (index)
      && contains_reg_of_mode[GENERAL_REGS][GET_MODE (SUBREG_REG (index))])
    index = SUBREG_REG (index);

  if (loongarch_valid_base_register_p (index, mode, strict_p))
    {
      info->type = ADDRESS_REG_REG;
      info->offset = index;
      return true;
    }

  return false;
}

/* Return true if X is a valid address for machine mode MODE.  If it is,
   fill in INFO appropriately.  STRICT_P is true if REG_OK_STRICT is in
   effect.  */

static bool
loongarch_classify_address (struct loongarch_address_info *info, rtx x,
			    machine_mode mode, bool strict_p)
{
  switch (GET_CODE (x))
    {
    case REG:
    case SUBREG:
      info->type = ADDRESS_REG;
      info->reg = x;
      info->offset = const0_rtx;
      return loongarch_valid_base_register_p (info->reg, mode, strict_p);

    case PLUS:
      if (loongarch_valid_base_register_p (XEXP (x, 0), mode, strict_p)
	  && loongarch_valid_index_p (info, XEXP (x, 1), mode, strict_p))
	{
	  info->reg = XEXP (x, 0);
	  return true;
	}

      if (loongarch_valid_base_register_p (XEXP (x, 1), mode, strict_p)
	 && loongarch_valid_index_p (info, XEXP (x, 0), mode, strict_p))
	{
	  info->reg = XEXP (x, 1);
	  return true;
	}

      info->type = ADDRESS_REG;
      info->reg = XEXP (x, 0);
      info->offset = XEXP (x, 1);
      return (loongarch_valid_base_register_p (info->reg, mode, strict_p)
	      && loongarch_valid_offset_p (info->offset, mode));

    case LO_SUM:
      info->type = ADDRESS_LO_SUM;
      info->reg = XEXP (x, 0);
      info->offset = XEXP (x, 1);
      /* We have to trust the creator of the LO_SUM to do something vaguely
	 sane.  Target-independent code that creates a LO_SUM should also
	 create and verify the matching HIGH.  Target-independent code that
	 adds an offset to a LO_SUM must prove that the offset will not
	 induce a carry.  Failure to do either of these things would be
	 a bug, and we are not required to check for it here.  The MIPS
	 backend itself should only create LO_SUMs for valid symbolic
	 constants, with the high part being either a HIGH or a copy
	 of _gp. */
      info->symbol_type
	= loongarch_classify_symbolic_expression (info->offset);
      return (loongarch_valid_base_register_p (info->reg, mode, strict_p)
	      && loongarch_valid_lo_sum_p (info->symbol_type, mode,
					   info->offset));
    case CONST_INT:
      /* Small-integer addresses don't occur very often, but they
	 are legitimate if $r0 is a valid base register.  */
      info->type = ADDRESS_CONST_INT;
      return IMM12_OPERAND (INTVAL (x));

    default:
      return false;
    }
}

/* Implement TARGET_LEGITIMATE_ADDRESS_P.  */

static bool
loongarch_legitimate_address_p (machine_mode mode, rtx x, bool strict_p)
{
  struct loongarch_address_info addr;

  return loongarch_classify_address (&addr, x, mode, strict_p);
}

/* Return true if ADDR matches the pattern for the indexed address
   instruction.  */

static bool
loongarch_index_address_p (rtx addr, machine_mode mode ATTRIBUTE_UNUSED)
{
  if (GET_CODE (addr) != PLUS
      || !REG_P (XEXP (addr, 0))
      || !REG_P (XEXP (addr, 1)))
    return false;
  return true;
}

/* Return the number of instructions needed to load or store a value
   of mode MODE at address X.  Return 0 if X isn't valid for MODE.
   Assume that multiword moves may need to be split into word moves
   if MIGHT_SPLIT_P, otherwise assume that a single load or store is
   enough.  */

int
loongarch_address_insns (rtx x, machine_mode mode, bool might_split_p)
{
  struct loongarch_address_info addr;
  int factor;

  if (!loongarch_classify_address (&addr, x, mode, false))
    return 0;

  /* BLKmode is used for single unaligned loads and stores and should
     not count as a multiword mode.  (GET_MODE_SIZE (BLKmode) is pretty
     meaningless, so we have to single it out as a special case one way
     or the other.)  */
  if (mode != BLKmode && might_split_p)
    factor = (GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
  else
    factor = 1;

  if (loongarch_classify_address (&addr, x, mode, false))
    switch (addr.type)
      {
      case ADDRESS_REG:
      case ADDRESS_REG_REG:
      case ADDRESS_CONST_INT:
	return factor;

      case ADDRESS_LO_SUM:
	return factor + 1;

      case ADDRESS_SYMBOLIC:
	return factor * loongarch_symbol_insns (addr.symbol_type, mode);
      }
  return 0;
}

/* Return true if X fits within an unsigned field of BITS bits that is
   shifted left SHIFT bits before being used.  */

bool
loongarch_unsigned_immediate_p (unsigned HOST_WIDE_INT x, int bits,
				int shift = 0)
{
  return (x & ((1 << shift) - 1)) == 0 && x < ((unsigned) 1 << (shift + bits));
}

/* Return true if X fits within a signed field of BITS bits that is
   shifted left SHIFT bits before being used.  */

bool
loongarch_signed_immediate_p (unsigned HOST_WIDE_INT x, int bits,
			      int shift = 0)
{
  x += 1 << (bits + shift - 1);
  return loongarch_unsigned_immediate_p (x, bits, shift);
}

/* Return true if X is a legitimate address with a 12-bit offset
   or addr.type is ADDRESS_LO_SUM.
   MODE is the mode of the value being accessed.  */

bool
loongarch_12bit_offset_address_p (rtx x, machine_mode mode)
{
  struct loongarch_address_info addr;

  return (loongarch_classify_address (&addr, x, mode, false)
	  && ((addr.type == ADDRESS_REG
	       && CONST_INT_P (addr.offset)
	       && LARCH_12BIT_OFFSET_P (INTVAL (addr.offset)))
	      || addr.type == ADDRESS_LO_SUM));
}

/* Return true if X is a legitimate address with a 14-bit offset shifted 2.
   MODE is the mode of the value being accessed.  */

bool
loongarch_14bit_shifted_offset_address_p (rtx x, machine_mode mode)
{
  struct loongarch_address_info addr;

  return (loongarch_classify_address (&addr, x, mode, false)
	  && addr.type == ADDRESS_REG
	  && CONST_INT_P (addr.offset)
	  && LARCH_16BIT_OFFSET_P (INTVAL (addr.offset))
	  && LARCH_SHIFT_2_OFFSET_P (INTVAL (addr.offset)));
}

/* Return true if X is a legitimate address with base and index.
   MODE is the mode of the value being accessed.  */

bool
loongarch_base_index_address_p (rtx x, machine_mode mode)
{
  struct loongarch_address_info addr;

  return (loongarch_classify_address (&addr, x, mode, false)
	  && addr.type == ADDRESS_REG_REG
	  && REG_P (addr.offset));
}

/* Return the number of instructions needed to load constant X,
   Return 0 if X isn't a valid constant.  */

int
loongarch_const_insns (rtx x)
{
  enum loongarch_symbol_type symbol_type;
  rtx offset;

  switch (GET_CODE (x))
    {
    case HIGH:
      if (!loongarch_symbolic_constant_p (XEXP (x, 0), &symbol_type)
	  || !loongarch_split_symbol_type (symbol_type))
	return 0;

      /* This is simply a PCALAU12I.  */
      return 1;

    case CONST_INT:
      return loongarch_integer_cost (INTVAL (x));

    case CONST_VECTOR:
      /* Fall through.  */
    case CONST_DOUBLE:
      return x == CONST0_RTX (GET_MODE (x)) ? 1 : 0;

    case CONST:
      /* See if we can refer to X directly.  */
      if (loongarch_symbolic_constant_p (x, &symbol_type))
	return loongarch_symbol_insns (symbol_type, MAX_MACHINE_MODE);

      /* Otherwise try splitting the constant into a base and offset.
	 If the offset is a 12-bit value, we can load the base address
	 into a register and then use ADDI.{W/D} to add in the offset.
	 If the offset is larger, we can load the base and offset
	 into separate registers and add them together with ADD.{W/D}.
	 However, the latter is only possible before reload; during
	 and after reload, we must have the option of forcing the
	 constant into the pool instead.  */
      split_const (x, &x, &offset);
      if (offset != 0)
	{
	  int n = loongarch_const_insns (x);
	  if (n != 0)
	    {
	      if (IMM12_INT (offset))
		return n + 1;
	      else if (!targetm.cannot_force_const_mem (GET_MODE (x), x))
		return n + 1 + loongarch_integer_cost (INTVAL (offset));
	    }
	}
      return 0;

    case SYMBOL_REF:
    case LABEL_REF:
      return loongarch_symbol_insns (
	loongarch_classify_symbol (x), MAX_MACHINE_MODE);

    default:
      return 0;
    }
}

/* X is a doubleword constant that can be handled by splitting it into
   two words and loading each word separately.  Return the number of
   instructions required to do this.  */

int
loongarch_split_const_insns (rtx x)
{
  unsigned int low, high;

  low = loongarch_const_insns (loongarch_subword (x, false));
  high = loongarch_const_insns (loongarch_subword (x, true));
  gcc_assert (low > 0 && high > 0);
  return low + high;
}

static bool loongarch_split_move_insn_p (rtx dest, rtx src);

/* Return the number of instructions needed to implement INSN,
   given that it loads from or stores to MEM.  */

int
loongarch_load_store_insns (rtx mem, rtx_insn *insn)
{
  machine_mode mode;
  bool might_split_p;
  rtx set;

  gcc_assert (MEM_P (mem));
  mode = GET_MODE (mem);

  /* Try to prove that INSN does not need to be split.  */
  might_split_p = GET_MODE_SIZE (mode) > UNITS_PER_WORD;
  if (might_split_p)
    {
      set = single_set (insn);
      if (set
	  && !loongarch_split_move_insn_p (SET_DEST (set), SET_SRC (set)))
	might_split_p = false;
    }

  return loongarch_address_insns (XEXP (mem, 0), mode, might_split_p);
}

/* Return true if we need to trap on division by zero.  */

bool
loongarch_check_zero_div_p (void)
{
  /* if -m[no-]check-zero-division is given explicitly.  */
  if (target_flags_explicit & MASK_CHECK_ZERO_DIV)
    return TARGET_CHECK_ZERO_DIV;

  /* if not, don't trap for optimized code except -Og.  */
  return !optimize || optimize_debug;
}

/* Return the number of instructions needed for an integer division.  */

int
loongarch_idiv_insns (machine_mode mode ATTRIBUTE_UNUSED)
{
  int count;

  count = 1;
  if (loongarch_check_zero_div_p ())
    count += 2;

  return count;
}

/* Emit an instruction of the form (set TARGET (CODE OP0 OP1)).  */

void
loongarch_emit_binary (enum rtx_code code, rtx target, rtx op0, rtx op1)
{
  emit_insn (gen_rtx_SET (target, gen_rtx_fmt_ee (code, GET_MODE (target),
						  op0, op1)));
}

/* Compute (CODE OP0 OP1) and store the result in a new register
   of mode MODE.  Return that new register.  */

static rtx
loongarch_force_binary (machine_mode mode, enum rtx_code code, rtx op0,
			rtx op1)
{
  rtx reg;

  reg = gen_reg_rtx (mode);
  loongarch_emit_binary (code, reg, op0, op1);
  return reg;
}

/* Copy VALUE to a register and return that register.  If new pseudos
   are allowed, copy it into a new register, otherwise use DEST.  */

static rtx
loongarch_force_temporary (rtx dest, rtx value)
{
  if (can_create_pseudo_p ())
    return force_reg (Pmode, value);
  else
    {
      loongarch_emit_move (dest, value);
      return dest;
    }
}

/* Wrap symbol or label BASE in an UNSPEC address of type SYMBOL_TYPE,
   then add CONST_INT OFFSET to the result.  */

static rtx
loongarch_unspec_address_offset (rtx base, rtx offset,
				 enum loongarch_symbol_type symbol_type)
{
  base = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, base),
			 UNSPEC_ADDRESS_FIRST + symbol_type);
  if (offset != const0_rtx)
    base = gen_rtx_PLUS (Pmode, base, offset);
  return gen_rtx_CONST (Pmode, base);
}

/* Return an UNSPEC address with underlying address ADDRESS and symbol
   type SYMBOL_TYPE.  */

rtx
loongarch_unspec_address (rtx address, enum loongarch_symbol_type symbol_type)
{
  rtx base, offset;

  split_const (address, &base, &offset);
  return loongarch_unspec_address_offset (base, offset, symbol_type);
}

/* Emit an instruction of the form (set TARGET SRC).  */

static rtx
loongarch_emit_set (rtx target, rtx src)
{
  emit_insn (gen_rtx_SET (target, src));
  return target;
}

/* If OP is an UNSPEC address, return the address to which it refers,
   otherwise return OP itself.  */

rtx
loongarch_strip_unspec_address (rtx op)
{
  rtx base, offset;

  split_const (op, &base, &offset);
  if (UNSPEC_ADDRESS_P (base))
    op = plus_constant (Pmode, UNSPEC_ADDRESS (base), INTVAL (offset));
  return op;
}

/* Return a legitimate address for REG + OFFSET.  TEMP is as for
   loongarch_force_temporary; it is only needed when OFFSET is not a
   IMM12_OPERAND.  */

static rtx
loongarch_add_offset (rtx temp, rtx reg, HOST_WIDE_INT offset)
{
  if (!IMM12_OPERAND (offset))
    {
      rtx high;

      /* Leave OFFSET as a 12-bit offset and put the excess in HIGH.
	 The addition inside the macro CONST_HIGH_PART may cause an
	 overflow, so we need to force a sign-extension check.  */
      high = gen_int_mode (CONST_HIGH_PART (offset), Pmode);
      offset = CONST_LOW_PART (offset);
      high = loongarch_force_temporary (temp, high);
      reg = loongarch_force_temporary (temp, gen_rtx_PLUS (Pmode, high, reg));
    }
  return plus_constant (Pmode, reg, offset);
}

/* The __tls_get_attr symbol.  */
static GTY (()) rtx loongarch_tls_symbol;

/* Load an entry from the GOT for a TLS GD access.  */

static rtx
loongarch_got_load_tls_gd (rtx dest, rtx sym)
{
  return gen_got_load_tls_gd (Pmode, dest, sym);
}

/* Load an entry from the GOT for a TLS LD access.  */

static rtx
loongarch_got_load_tls_ld (rtx dest, rtx sym)
{
  return gen_got_load_tls_ld (Pmode, dest, sym);
}

/* Load an entry from the GOT for a TLS IE access.  */

static rtx
loongarch_got_load_tls_ie (rtx dest, rtx sym)
{
  return gen_got_load_tls_ie (Pmode, dest, sym);
}

/* Add in the thread pointer for a TLS LE access.  */

static rtx
loongarch_got_load_tls_le (rtx dest, rtx sym)
{
  return gen_got_load_tls_le (Pmode, dest, sym);
}

/* Return an instruction sequence that calls __tls_get_addr.  SYM is
   the TLS symbol we are referencing and TYPE is the symbol type to use
   (either global dynamic or local dynamic).  V0 is an RTX for the
   return value location.  */

static rtx_insn *
loongarch_call_tls_get_addr (rtx sym, enum loongarch_symbol_type type, rtx v0)
{
  rtx loc, a0;
  rtx_insn *insn;
  rtx tmp = gen_reg_rtx (Pmode);

  a0 = gen_rtx_REG (Pmode, GP_ARG_FIRST);

  if (!loongarch_tls_symbol)
    loongarch_tls_symbol = init_one_libfunc ("__tls_get_addr");

  loc = loongarch_unspec_address (sym, type);

  start_sequence ();

  if (TARGET_EXPLICIT_RELOCS)
    {
      /* Split tls symbol to high and low.  */
      rtx high = gen_rtx_HIGH (Pmode, copy_rtx (loc));
      high = loongarch_force_temporary (tmp, high);

      if (TARGET_CMODEL_EXTREME)
	{
	  gcc_assert (TARGET_EXPLICIT_RELOCS);

	  rtx tmp1 = gen_reg_rtx (Pmode);
	  emit_insn (gen_tls_low (Pmode, tmp1, gen_rtx_REG (Pmode, 0), loc));
	  emit_insn (gen_lui_h_lo20 (tmp1, tmp1, loc));
	  emit_insn (gen_lui_h_hi12 (tmp1, tmp1, loc));
	  emit_move_insn (a0, gen_rtx_PLUS (Pmode, high, tmp1));
	}
      else
	emit_insn (gen_tls_low (Pmode, a0, high, loc));
    }
  else
    {
      if (type == SYMBOL_TLSLDM)
	emit_insn (loongarch_got_load_tls_ld (a0, loc));
      else if (type == SYMBOL_TLSGD)
	emit_insn (loongarch_got_load_tls_gd (a0, loc));
      else
	gcc_unreachable ();
    }

  if (flag_plt)
    {
      switch (la_opt_cmodel)
	{
	case CMODEL_NORMAL:
	  insn = emit_call_insn (gen_call_value_internal (v0,
							  loongarch_tls_symbol,
							  const0_rtx));
	  break;

	case CMODEL_MEDIUM:
	    {
	      rtx reg = gen_reg_rtx (Pmode);
	      if (TARGET_EXPLICIT_RELOCS)
		{
		  emit_insn (gen_pcalau12i (Pmode, reg, loongarch_tls_symbol));
		  rtx call = gen_call_value_internal_1 (Pmode, v0, reg,
							loongarch_tls_symbol,
							const0_rtx);
		  insn = emit_call_insn (call);
		}
	      else
		{
		  emit_move_insn (reg, loongarch_tls_symbol);
		  insn = emit_call_insn (gen_call_value_internal (v0,
								  reg,
								  const0_rtx));
		}
	      break;
	    }

	/* code model extreme not support plt.  */
	case CMODEL_EXTREME:
	case CMODEL_LARGE:
	case CMODEL_TINY:
	case CMODEL_TINY_STATIC:
	default:
	  gcc_unreachable ();
	}
    }
  else
    {
      rtx dest = gen_reg_rtx (Pmode);

      switch (la_opt_cmodel)
	{
	case CMODEL_NORMAL:
	case CMODEL_MEDIUM:
	    {
	      if (TARGET_EXPLICIT_RELOCS)
		{
		  rtx high = gen_reg_rtx (Pmode);
		  loongarch_emit_move (high,
				       gen_rtx_HIGH (Pmode,
						     loongarch_tls_symbol));
		  emit_insn (gen_ld_from_got (Pmode, dest, high,
					      loongarch_tls_symbol));
		}
	      else
		loongarch_emit_move (dest, loongarch_tls_symbol);
	      break;
	    }

	case CMODEL_EXTREME:
	    {
	      gcc_assert (TARGET_EXPLICIT_RELOCS);

	      rtx tmp1 = gen_reg_rtx (Pmode);
	      rtx high = gen_reg_rtx (Pmode);

	      loongarch_emit_move (high,
				   gen_rtx_HIGH (Pmode, loongarch_tls_symbol));
	      loongarch_emit_move (tmp1, gen_rtx_LO_SUM (Pmode,
							 gen_rtx_REG (Pmode, 0),
							 loongarch_tls_symbol));
	      emit_insn (gen_lui_h_lo20 (tmp1, tmp1, loongarch_tls_symbol));
	      emit_insn (gen_lui_h_hi12 (tmp1, tmp1, loongarch_tls_symbol));
	      loongarch_emit_move (dest,
				   gen_rtx_MEM (Pmode,
						gen_rtx_PLUS (Pmode,
							      high, tmp1)));
	    }
	  break;

	case CMODEL_LARGE:
	case CMODEL_TINY:
	case CMODEL_TINY_STATIC:
	default:
	  gcc_unreachable ();
	}

      insn = emit_call_insn (gen_call_value_internal (v0, dest, const0_rtx));
    }

  RTL_CONST_CALL_P (insn) = 1;
  use_reg (&CALL_INSN_FUNCTION_USAGE (insn), a0);
  insn = get_insns ();

  end_sequence ();

  return insn;
}

/* Generate the code to access LOC, a thread-local SYMBOL_REF, and return
   its address.  The return value will be both a valid address and a valid
   SET_SRC (either a REG or a LO_SUM).  */

static rtx
loongarch_legitimize_tls_address (rtx loc)
{
  rtx dest, tp, tmp, tmp1, tmp2, tmp3;
  enum tls_model model = SYMBOL_REF_TLS_MODEL (loc);
  rtx_insn *insn;

  switch (model)
    {
    case TLS_MODEL_LOCAL_DYNAMIC:
      tmp = gen_rtx_REG (Pmode, GP_RETURN);
      dest = gen_reg_rtx (Pmode);
      insn = loongarch_call_tls_get_addr (loc, SYMBOL_TLSLDM, tmp);
      emit_libcall_block (insn, dest, tmp, loc);
      break;

    case TLS_MODEL_GLOBAL_DYNAMIC:
      tmp = gen_rtx_REG (Pmode, GP_RETURN);
      dest = gen_reg_rtx (Pmode);
      insn = loongarch_call_tls_get_addr (loc, SYMBOL_TLSGD, tmp);
      emit_libcall_block (insn, dest, tmp, loc);
      break;

    case TLS_MODEL_INITIAL_EXEC:
	{
	  /* la.tls.ie; tp-relative add.  */
	  tp = gen_rtx_REG (Pmode, THREAD_POINTER_REGNUM);
	  tmp1 = gen_reg_rtx (Pmode);
	  dest = gen_reg_rtx (Pmode);
	  if (TARGET_EXPLICIT_RELOCS)
	    {
	      tmp2 = loongarch_unspec_address (loc, SYMBOL_TLS_IE);
	      tmp3 = gen_reg_rtx (Pmode);
	      rtx high = gen_rtx_HIGH (Pmode, copy_rtx (tmp2));
	      high = loongarch_force_temporary (tmp3, high);

	      if (TARGET_CMODEL_EXTREME)
		{
		  gcc_assert (TARGET_EXPLICIT_RELOCS);

		  rtx tmp3 = gen_reg_rtx (Pmode);
		  emit_insn (gen_tls_low (Pmode, tmp3,
					  gen_rtx_REG (Pmode, 0), tmp2));
		  emit_insn (gen_lui_h_lo20 (tmp3, tmp3, tmp2));
		  emit_insn (gen_lui_h_hi12 (tmp3, tmp3, tmp2));
		  emit_move_insn (tmp1,
				  gen_rtx_MEM (Pmode,
					       gen_rtx_PLUS (Pmode,
							     high, tmp3)));
		}
	      else
		emit_insn (gen_ld_from_got (Pmode, tmp1, high, tmp2));
	    }
	  else
	    emit_insn (loongarch_got_load_tls_ie (tmp1, loc));
	  emit_insn (gen_add3_insn (dest, tmp1, tp));
	}
      break;

    case TLS_MODEL_LOCAL_EXEC:
	{
	  /* la.tls.le; tp-relative add.  */
	  tp = gen_rtx_REG (Pmode, THREAD_POINTER_REGNUM);
	  tmp1 = gen_reg_rtx (Pmode);
	  dest = gen_reg_rtx (Pmode);

	  if (TARGET_EXPLICIT_RELOCS)
	    {
	      tmp2 = loongarch_unspec_address (loc, SYMBOL_TLS_LE);
	      tmp3 = gen_reg_rtx (Pmode);
	      rtx high = gen_rtx_HIGH (Pmode, copy_rtx (tmp2));
	      high = loongarch_force_temporary (tmp3, high);
	      emit_insn (gen_ori_l_lo12 (Pmode, tmp1, high, tmp2));

	      if (TARGET_CMODEL_EXTREME)
		{
		  gcc_assert (TARGET_EXPLICIT_RELOCS);

		  emit_insn (gen_lui_h_lo20 (tmp1, tmp1, tmp2));
		  emit_insn (gen_lui_h_hi12 (tmp1, tmp1, tmp2));
		}
	    }
	  else
	    emit_insn (loongarch_got_load_tls_le (tmp1, loc));
	  emit_insn (gen_add3_insn (dest, tmp1, tp));
	}
      break;

    default:
      gcc_unreachable ();
    }
  return dest;
}

rtx
loongarch_legitimize_call_address (rtx addr)
{
  if (!call_insn_operand (addr, VOIDmode))
    {
      rtx reg = gen_reg_rtx (Pmode);
      loongarch_emit_move (reg, addr);
      return reg;
    }

  enum loongarch_symbol_type symbol_type = loongarch_classify_symbol (addr);

  /* Split function call insn 'bl sym' or 'bl %plt(sym)' to :
     pcalau12i $rd, %pc_hi20(sym)
     jr $rd, %pc_lo12(sym).  */

  if (TARGET_CMODEL_MEDIUM
      && TARGET_EXPLICIT_RELOCS
      && (SYMBOL_REF_P (addr) || LABEL_REF_P (addr))
      && (symbol_type == SYMBOL_PCREL
	  || (symbol_type == SYMBOL_GOT_DISP && flag_plt)))
    {
      rtx reg = gen_reg_rtx (Pmode);
      emit_insn (gen_pcalau12i (Pmode, reg, addr));
      return gen_rtx_LO_SUM (Pmode, reg, addr);
    }

  return addr;
}

/* If X is a PLUS of a CONST_INT, return the two terms in *BASE_PTR
   and *OFFSET_PTR.  Return X in *BASE_PTR and 0 in *OFFSET_PTR otherwise.  */

static void
loongarch_split_plus (rtx x, rtx *base_ptr, HOST_WIDE_INT *offset_ptr)
{
  if (GET_CODE (x) == PLUS && CONST_INT_P (XEXP (x, 1)))
    {
      *base_ptr = XEXP (x, 0);
      *offset_ptr = INTVAL (XEXP (x, 1));
    }
  else
    {
      *base_ptr = x;
      *offset_ptr = 0;
    }
}

/* If X is not a valid address for mode MODE, force it into a register.  */

static rtx
loongarch_force_address (rtx x, machine_mode mode)
{
  if (!loongarch_legitimate_address_p (mode, x, false))
    x = force_reg (Pmode, x);
  return x;
}

static bool
loongarch_symbol_extreme_p (enum loongarch_symbol_type type)
{
  switch (type)
    {
      case SYMBOL_PCREL:
	return false;
      case SYMBOL_PCREL64:
	return true;
      default:
	return TARGET_CMODEL_EXTREME;
    }
}

/* If MODE is MAX_MACHINE_MODE, ADDR appears as a move operand, otherwise
   it appears in a MEM of that mode.  Return true if ADDR is a legitimate
   constant in that context and can be split into high and low parts.
   If so, and if LOW_OUT is nonnull, emit the high part and store the
   low part in *LOW_OUT.  Leave *LOW_OUT unchanged otherwise.

   Return false if build with '-mno-explicit-relocs'.

   TEMP is as for loongarch_force_temporary and is used to load the high
   part into a register.

   When MODE is MAX_MACHINE_MODE, the low part is guaranteed to be
   a legitimize SET_SRC for an .md pattern, otherwise the low part
   is guaranteed to be a legitimate address for mode MODE.  */

bool
loongarch_split_symbol (rtx temp, rtx addr, machine_mode mode, rtx *low_out)
{
  enum loongarch_symbol_type symbol_type;

  /* If build with '-mno-explicit-relocs', don't split symbol.  */
  if (!TARGET_EXPLICIT_RELOCS)
    return false;

  if ((GET_CODE (addr) == HIGH && mode == MAX_MACHINE_MODE)
      || !loongarch_symbolic_constant_p (addr, &symbol_type)
      || loongarch_symbol_insns (symbol_type, mode) == 0
      || !loongarch_split_symbol_type (symbol_type))
    return false;

  rtx high, temp1 = NULL;

  if (temp == NULL)
    temp = gen_reg_rtx (Pmode);

  /* Get the 12-31 bits of the address.  */
  high = gen_rtx_HIGH (Pmode, copy_rtx (addr));
  high = loongarch_force_temporary (temp, high);

  if (loongarch_symbol_extreme_p (symbol_type) && can_create_pseudo_p ())
    {
      gcc_assert (TARGET_EXPLICIT_RELOCS);

      temp1 = gen_reg_rtx (Pmode);
      emit_move_insn (temp1, gen_rtx_LO_SUM (Pmode, gen_rtx_REG (Pmode, 0),
					     addr));
      emit_insn (gen_lui_h_lo20 (temp1, temp1, addr));
      emit_insn (gen_lui_h_hi12 (temp1, temp1, addr));
    }

  if (low_out)
    switch (symbol_type)
      {
      case SYMBOL_PCREL64:
	if (can_create_pseudo_p ())
	  {
	    *low_out = gen_rtx_PLUS (Pmode, high, temp1);
	    break;
	  }
	/* fall through */
      case SYMBOL_PCREL:
	*low_out = gen_rtx_LO_SUM (Pmode, high, addr);
	break;

      case SYMBOL_GOT_DISP:
	/* SYMBOL_GOT_DISP symbols are loaded from the GOT.  */
	{
	  if (TARGET_CMODEL_EXTREME && can_create_pseudo_p ())
	    *low_out = gen_rtx_MEM (Pmode, gen_rtx_PLUS (Pmode, high, temp1));
	  else
	    {
	      rtx low = gen_rtx_LO_SUM (Pmode, high, addr);
	      rtx mem = gen_rtx_MEM (Pmode, low);
	      *low_out = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, mem),
					 UNSPEC_LOAD_FROM_GOT);
	    }

	  break;
	}

      default:
	gcc_unreachable ();
      }

  return true;
}

/* This function is used to implement LEGITIMIZE_ADDRESS.  If X can
   be legitimized in a way that the generic machinery might not expect,
   return a new address, otherwise return NULL.  MODE is the mode of
   the memory being accessed.  */

static rtx
loongarch_legitimize_address (rtx x, rtx oldx ATTRIBUTE_UNUSED,
			      machine_mode mode)
{
  rtx base, addr;
  HOST_WIDE_INT offset;

  if (loongarch_tls_symbol_p (x))
    return loongarch_legitimize_tls_address (x);

  /* See if the address can split into a high part and a LO_SUM.  */
  if (loongarch_split_symbol (NULL, x, mode, &addr))
    return loongarch_force_address (addr, mode);

  /* Handle BASE + OFFSET using loongarch_add_offset.  */
  loongarch_split_plus (x, &base, &offset);
  if (offset != 0)
    {
      if (!loongarch_valid_base_register_p (base, mode, false))
	base = copy_to_mode_reg (Pmode, base);
      addr = loongarch_add_offset (NULL, base, offset);
      return loongarch_force_address (addr, mode);
    }

  return x;
}

/* Load VALUE into DEST.  TEMP is as for loongarch_force_temporary.  */

void
loongarch_move_integer (rtx temp, rtx dest, unsigned HOST_WIDE_INT value)
{
  struct loongarch_integer_op codes[LARCH_MAX_INTEGER_OPS];
  machine_mode mode;
  unsigned int i, num_ops;
  rtx x;

  mode = GET_MODE (dest);
  num_ops = loongarch_build_integer (codes, value);

  /* Apply each binary operation to X.  Invariant: X is a legitimate
     source operand for a SET pattern.  */
  x = GEN_INT (codes[0].value);
  for (i = 1; i < num_ops; i++)
    {
      if (!can_create_pseudo_p ())
	{
	  emit_insn (gen_rtx_SET (temp, x));
	  x = temp;
	}
      else
	x = force_reg (mode, x);

      set_unique_reg_note (get_last_insn (), REG_EQUAL,
			   GEN_INT (codes[i-1].curr_value));

      switch (codes[i].method)
	{
	case METHOD_NORMAL:
	  x = gen_rtx_fmt_ee (codes[i].code, mode, x,
			      GEN_INT (codes[i].value));
	  break;
	case METHOD_LU32I:
	  gcc_assert (mode == DImode);
	  x = gen_rtx_IOR (DImode,
			   gen_rtx_ZERO_EXTEND (DImode,
						gen_rtx_SUBREG (SImode, x, 0)),
			   GEN_INT (codes[i].value));
	  break;
	case METHOD_LU52I:
	  gcc_assert (mode == DImode);
	  x = gen_rtx_IOR (DImode,
			   gen_rtx_AND (DImode, x, GEN_INT (0xfffffffffffff)),
			   GEN_INT (codes[i].value));
	  break;
	default:
	  gcc_unreachable ();
	}
    }

  emit_insn (gen_rtx_SET (dest, x));
}

/* Subroutine of loongarch_legitimize_move.  Move constant SRC into register
   DEST given that SRC satisfies immediate_operand but doesn't satisfy
   move_operand.  */

static void
loongarch_legitimize_const_move (machine_mode mode, rtx dest, rtx src)
{
  rtx base, offset;

  /* Split moves of big integers into smaller pieces.  */
  if (splittable_const_int_operand (src, mode))
    {
      loongarch_move_integer (dest, dest, INTVAL (src));
      return;
    }

  /* Split moves of symbolic constants into high and low.  */
  if (loongarch_split_symbol (dest, src, MAX_MACHINE_MODE, &src))
    {
      loongarch_emit_set (dest, src);
      return;
    }

  /* Generate the appropriate access sequences for TLS symbols.  */
  if (loongarch_tls_symbol_p (src))
    {
      loongarch_emit_move (dest, loongarch_legitimize_tls_address (src));
      return;
    }

  /* If we have (const (plus symbol offset)), and that expression cannot
     be forced into memory, load the symbol first and add in the offset.
     prefer to do this even if the constant _can_ be forced into memory,
     as it usually produces better code.  */
  split_const (src, &base, &offset);
  if (offset != const0_rtx
      && (targetm.cannot_force_const_mem (mode, src)
	  || (can_create_pseudo_p ())))
    {
      base = loongarch_force_temporary (dest, base);
      loongarch_emit_move (dest,
			   loongarch_add_offset (NULL, base, INTVAL (offset)));
      return;
    }

  src = force_const_mem (mode, src);

  loongarch_emit_move (dest, src);
}

/* If (set DEST SRC) is not a valid move instruction, emit an equivalent
   sequence that is valid.  */

bool
loongarch_legitimize_move (machine_mode mode, rtx dest, rtx src)
{
  if (!register_operand (dest, mode) && !reg_or_0_operand (src, mode))
    {
      loongarch_emit_move (dest, force_reg (mode, src));
      return true;
    }

  /* Both src and dest are non-registers;  one special case is supported where
     the source is (const_int 0) and the store can source the zero register.
     */
  if (!register_operand (dest, mode) && !register_operand (src, mode)
      && !const_0_operand (src, mode))
    {
      loongarch_emit_move (dest, force_reg (mode, src));
      return true;
    }

  /* We need to deal with constants that would be legitimate
     immediate_operands but aren't legitimate move_operands.  */
  if (CONSTANT_P (src) && !move_operand (src, mode))
    {
      loongarch_legitimize_const_move (mode, dest, src);
      set_unique_reg_note (get_last_insn (), REG_EQUAL, copy_rtx (src));
      return true;
    }

  return false;
}

/* Return true if OP refers to small data symbols directly.  */

static int
loongarch_small_data_pattern_1 (rtx x)
{
  subrtx_var_iterator::array_type array;
  FOR_EACH_SUBRTX_VAR (iter, array, x, ALL)
    {
      rtx x = *iter;

      /* We make no particular guarantee about which symbolic constants are
	 acceptable as asm operands versus which must be forced into a GPR.  */
      if (GET_CODE (x) == ASM_OPERANDS)
	iter.skip_subrtxes ();
      else if (MEM_P (x))
	{
	  if (loongarch_small_data_pattern_1 (XEXP (x, 0)))
	    return true;
	  iter.skip_subrtxes ();
	}
    }
  return false;
}

/* Return true if OP refers to small data symbols directly.  */

bool
loongarch_small_data_pattern_p (rtx op)
{
  return loongarch_small_data_pattern_1 (op);
}

/* Rewrite *LOC so that it refers to small data using explicit
   relocations.  */

static void
loongarch_rewrite_small_data_1 (rtx *loc)
{
  subrtx_ptr_iterator::array_type array;
  FOR_EACH_SUBRTX_PTR (iter, array, loc, ALL)
    {
      rtx *loc = *iter;
      if (MEM_P (*loc))
	{
	  loongarch_rewrite_small_data_1 (&XEXP (*loc, 0));
	  iter.skip_subrtxes ();
	}
    }
}

/* Rewrite instruction pattern PATTERN so that it refers to small data
   using explicit relocations.  */

rtx
loongarch_rewrite_small_data (rtx pattern)
{
  pattern = copy_insn (pattern);
  loongarch_rewrite_small_data_1 (&pattern);
  return pattern;
}

/* The cost of loading values from the constant pool.  It should be
   larger than the cost of any constant we want to synthesize inline.  */
#define CONSTANT_POOL_COST COSTS_N_INSNS (8)

/* Return true if there is a instruction that implements CODE
   and if that instruction accepts X as an immediate operand.  */

static int
loongarch_immediate_operand_p (int code, HOST_WIDE_INT x)
{
  switch (code)
    {
    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
      /* All shift counts are truncated to a valid constant.  */
      return true;

    case ROTATE:
    case ROTATERT:
      return true;

    case AND:
    case IOR:
    case XOR:
      /* These instructions take 12-bit unsigned immediates.  */
      return IMM12_OPERAND_UNSIGNED (x);

    case PLUS:
    case LT:
    case LTU:
      /* These instructions take 12-bit signed immediates.  */
      return IMM12_OPERAND (x);

    case EQ:
    case NE:
    case GT:
    case GTU:
      /* The "immediate" forms of these instructions are really
	 implemented as comparisons with register 0.  */
      return x == 0;

    case GE:
    case GEU:
      /* Likewise, meaning that the only valid immediate operand is 1.  */
      return x == 1;

    case LE:
      /* We add 1 to the immediate and use SLT.  */
      return IMM12_OPERAND (x + 1);

    case LEU:
      /* Likewise SLTU, but reject the always-true case.  */
      return IMM12_OPERAND (x + 1) && x + 1 != 0;

    case SIGN_EXTRACT:
    case ZERO_EXTRACT:
      /* The bit position and size are immediate operands.  */
      return 1;

    default:
      /* By default assume that $0 can be used for 0.  */
      return x == 0;
    }
}

/* Return the cost of binary operation X, given that the instruction
   sequence for a word-sized or smaller operation has cost SINGLE_COST
   and that the sequence of a double-word operation has cost DOUBLE_COST.
   If SPEED is true, optimize for speed otherwise optimize for size.  */

static int
loongarch_binary_cost (rtx x, int single_cost, int double_cost, bool speed)
{
  int cost;

  if (GET_MODE_SIZE (GET_MODE (x)) == UNITS_PER_WORD * 2)
    cost = double_cost;
  else
    cost = single_cost;
  return (cost
	  + set_src_cost (XEXP (x, 0), GET_MODE (x), speed)
	  + rtx_cost (XEXP (x, 1), GET_MODE (x), GET_CODE (x), 1, speed));
}

/* Return the cost of floating-point multiplications of mode MODE.  */

static int
loongarch_fp_mult_cost (machine_mode mode)
{
  return mode == DFmode ? loongarch_cost->fp_mult_df
			: loongarch_cost->fp_mult_sf;
}

/* Return the cost of floating-point divisions of mode MODE.  */

static int
loongarch_fp_div_cost (machine_mode mode)
{
  return mode == DFmode ? loongarch_cost->fp_div_df
			: loongarch_cost->fp_div_sf;
}

/* Return the cost of sign-extending OP to mode MODE, not including the
   cost of OP itself.  */

static int
loongarch_sign_extend_cost (rtx op)
{
  if (MEM_P (op))
    /* Extended loads are as cheap as unextended ones.  */
    return 0;

  return COSTS_N_INSNS (1);
}

/* Return the cost of zero-extending OP to mode MODE, not including the
   cost of OP itself.  */

static int
loongarch_zero_extend_cost (rtx op)
{
  if (MEM_P (op))
    /* Extended loads are as cheap as unextended ones.  */
    return 0;

  /* We can use ANDI.  */
  return COSTS_N_INSNS (1);
}

/* Return the cost of moving between two registers of mode MODE,
   assuming that the move will be in pieces of at most UNITS bytes.  */

static int
loongarch_set_reg_reg_piece_cost (machine_mode mode, unsigned int units)
{
  return COSTS_N_INSNS ((GET_MODE_SIZE (mode) + units - 1) / units);
}

/* Return the cost of moving between two registers of mode MODE.  */

static int
loongarch_set_reg_reg_cost (machine_mode mode)
{
  switch (GET_MODE_CLASS (mode))
    {
    case MODE_CC:
      return loongarch_set_reg_reg_piece_cost (mode, GET_MODE_SIZE (CCmode));

    case MODE_FLOAT:
    case MODE_COMPLEX_FLOAT:
    case MODE_VECTOR_FLOAT:
      if (TARGET_HARD_FLOAT)
	return loongarch_set_reg_reg_piece_cost (mode, UNITS_PER_HWFPVALUE);
      /* Fall through.  */

    default:
      return loongarch_set_reg_reg_piece_cost (mode, UNITS_PER_WORD);
    }
}

/* Implement TARGET_RTX_COSTS.  */

static bool
loongarch_rtx_costs (rtx x, machine_mode mode, int outer_code,
		     int opno ATTRIBUTE_UNUSED, int *total, bool speed)
{
  int code = GET_CODE (x);
  bool float_mode_p = FLOAT_MODE_P (mode);
  int cost;
  rtx addr;

  if (outer_code == COMPARE)
    {
      gcc_assert (CONSTANT_P (x));
      *total = 0;
      return true;
    }

  switch (code)
    {
    case CONST_INT:
      if (TARGET_64BIT && outer_code == AND && UINTVAL (x) == 0xffffffff)
	{
	  *total = 0;
	  return true;
	}

      /* When not optimizing for size, we care more about the cost
	 of hot code, and hot code is often in a loop.  If a constant
	 operand needs to be forced into a register, we will often be
	 able to hoist the constant load out of the loop, so the load
	 should not contribute to the cost.  */
      if (speed || loongarch_immediate_operand_p (outer_code, INTVAL (x)))
	{
	  *total = 0;
	  return true;
	}
      /* Fall through.  */

    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
    case CONST_DOUBLE:
      cost = loongarch_const_insns (x);
      if (cost > 0)
	{
	  if (cost == 1 && outer_code == SET
	      && !(float_mode_p && TARGET_HARD_FLOAT))
	    cost = 0;
	  else if ((outer_code == SET || GET_MODE (x) == VOIDmode))
	    cost = 1;
	  *total = COSTS_N_INSNS (cost);
	  return true;
	}
      /* The value will need to be fetched from the constant pool.  */
      *total = CONSTANT_POOL_COST;
      return true;

    case MEM:
      /* If the address is legitimate, return the number of
	 instructions it needs.  */
      addr = XEXP (x, 0);
      /* Check for a scaled indexed address.  */
      if (loongarch_index_address_p (addr, mode))
	{
	  *total = COSTS_N_INSNS (2);
	  return true;
	}
      cost = loongarch_address_insns (addr, mode, true);
      if (cost > 0)
	{
	  *total = COSTS_N_INSNS (cost + 1);
	  return true;
	}
      /* Otherwise use the default handling.  */
      return false;

    case FFS:
      *total = COSTS_N_INSNS (6);
      return false;

    case NOT:
      *total = COSTS_N_INSNS (GET_MODE_SIZE (mode) > UNITS_PER_WORD ? 2 : 1);
      return false;

    case AND:
      /* Check for a *clear_upper32 pattern and treat it like a zero
	 extension.  See the pattern's comment for details.  */
      if (TARGET_64BIT && mode == DImode && CONST_INT_P (XEXP (x, 1))
	  && UINTVAL (XEXP (x, 1)) == 0xffffffff)
	{
	  *total = (loongarch_zero_extend_cost (XEXP (x, 0))
		    + set_src_cost (XEXP (x, 0), mode, speed));
	  return true;
	}
      /* (AND (NOT op0) (NOT op1) is a nor operation that can be done in
	 a single instruction.  */
      if (GET_CODE (XEXP (x, 0)) == NOT && GET_CODE (XEXP (x, 1)) == NOT)
	{
	  cost = GET_MODE_SIZE (mode) > UNITS_PER_WORD ? 2 : 1;
	  *total = (COSTS_N_INSNS (cost)
		    + set_src_cost (XEXP (XEXP (x, 0), 0), mode, speed)
		    + set_src_cost (XEXP (XEXP (x, 1), 0), mode, speed));
	  return true;
	}

      /* Fall through.  */

    case IOR:
    case XOR:
      /* Double-word operations use two single-word operations.  */
      *total = loongarch_binary_cost (x, COSTS_N_INSNS (1), COSTS_N_INSNS (2),
				      speed);
      return true;

    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
    case ROTATE:
    case ROTATERT:
      if (CONSTANT_P (XEXP (x, 1)))
	*total = loongarch_binary_cost (x, COSTS_N_INSNS (1),
					COSTS_N_INSNS (4), speed);
      else
	*total = loongarch_binary_cost (x, COSTS_N_INSNS (1),
					COSTS_N_INSNS (12), speed);
      return true;

    case ABS:
      if (float_mode_p)
	*total = loongarch_cost->fp_add;
      else
	*total = COSTS_N_INSNS (4);
      return false;

    case LT:
    case LTU:
    case LE:
    case LEU:
    case GT:
    case GTU:
    case GE:
    case GEU:
    case EQ:
    case NE:
    case UNORDERED:
    case LTGT:
    case UNGE:
    case UNGT:
    case UNLE:
    case UNLT:
      /* Branch comparisons have VOIDmode, so use the first operand's
	 mode instead.  */
      mode = GET_MODE (XEXP (x, 0));
      if (FLOAT_MODE_P (mode))
	{
	  *total = loongarch_cost->fp_add;
	  return false;
	}
      *total = loongarch_binary_cost (x, COSTS_N_INSNS (1), COSTS_N_INSNS (4),
				      speed);
      return true;

    case MINUS:
    case PLUS:
      if (float_mode_p)
	{
	  *total = loongarch_cost->fp_add;
	  return false;
	}

      /* If it's an add + mult (which is equivalent to shift left) and
	 it's immediate operand satisfies const_immalsl_operand predicate.  */
      if ((mode == SImode || (TARGET_64BIT && mode == DImode))
	  && GET_CODE (XEXP (x, 0)) == MULT)
	{
	  rtx op2 = XEXP (XEXP (x, 0), 1);
	  if (const_immalsl_operand (op2, mode))
	    {
	      *total = (COSTS_N_INSNS (1)
			+ set_src_cost (XEXP (XEXP (x, 0), 0), mode, speed)
			+ set_src_cost (XEXP (x, 1), mode, speed));
	      return true;
	    }
	}

      /* Double-word operations require three single-word operations and
	 an SLTU.  */
      *total = loongarch_binary_cost (x, COSTS_N_INSNS (1), COSTS_N_INSNS (4),
				      speed);
      return true;

    case NEG:
      if (float_mode_p)
	*total = loongarch_cost->fp_add;
      else
	*total = COSTS_N_INSNS (GET_MODE_SIZE (mode) > UNITS_PER_WORD ? 4 : 1);
      return false;

    case FMA:
      *total = loongarch_fp_mult_cost (mode);
      return false;

    case MULT:
      if (float_mode_p)
	*total = loongarch_fp_mult_cost (mode);
      else if (mode == DImode && !TARGET_64BIT)
	*total = (speed
		  ? loongarch_cost->int_mult_si * 3 + 6
		  : COSTS_N_INSNS (7));
      else if (!speed)
	*total = COSTS_N_INSNS (1) + 1;
      else if (mode == DImode)
	*total = loongarch_cost->int_mult_di;
      else
	*total = loongarch_cost->int_mult_si;
      return false;

    case DIV:
      /* Check for a reciprocal.  */
      if (float_mode_p
	  && flag_unsafe_math_optimizations
	  && XEXP (x, 0) == CONST1_RTX (mode))
	{
	  if (outer_code == SQRT || GET_CODE (XEXP (x, 1)) == SQRT)
	    /* An rsqrt<mode>a or rsqrt<mode>b pattern.  Count the
	       division as being free.  */
	    *total = set_src_cost (XEXP (x, 1), mode, speed);
	  else
	    *total = (loongarch_fp_div_cost (mode)
		      + set_src_cost (XEXP (x, 1), mode, speed));
	  return true;
	}
      /* Fall through.  */

    case SQRT:
    case MOD:
      if (float_mode_p)
	{
	  *total = loongarch_fp_div_cost (mode);
	  return false;
	}
      /* Fall through.  */

    case UDIV:
    case UMOD:
      if (!speed)
	{
	  *total = COSTS_N_INSNS (loongarch_idiv_insns (mode));
	}
      else if (mode == DImode)
	*total = loongarch_cost->int_div_di;
      else
	*total = loongarch_cost->int_div_si;
      return false;

    case SIGN_EXTEND:
      *total = loongarch_sign_extend_cost (XEXP (x, 0));
      return false;

    case ZERO_EXTEND:
      *total = loongarch_zero_extend_cost (XEXP (x, 0));
      return false;
    case TRUNCATE:
      /* Costings for highpart multiplies.  Matching patterns of the form:

	 (lshiftrt:DI (mult:DI (sign_extend:DI (...)
			       (sign_extend:DI (...))
		      (const_int 32)
      */
      if ((GET_CODE (XEXP (x, 0)) == ASHIFTRT
	   || GET_CODE (XEXP (x, 0)) == LSHIFTRT)
	  && CONST_INT_P (XEXP (XEXP (x, 0), 1))
	  && ((INTVAL (XEXP (XEXP (x, 0), 1)) == 32
	       && GET_MODE (XEXP (x, 0)) == DImode)
	      || (TARGET_64BIT
		  && INTVAL (XEXP (XEXP (x, 0), 1)) == 64
		  && GET_MODE (XEXP (x, 0)) == TImode))
	  && GET_CODE (XEXP (XEXP (x, 0), 0)) == MULT
	  && ((GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 0)) == SIGN_EXTEND
	       && GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 1)) == SIGN_EXTEND)
	      || (GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 0)) == ZERO_EXTEND
		  && (GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 1))
		      == ZERO_EXTEND))))
	{
	  if (!speed)
	    *total = COSTS_N_INSNS (1) + 1;
	  else if (mode == DImode)
	    *total = loongarch_cost->int_mult_di;
	  else
	    *total = loongarch_cost->int_mult_si;

	  /* Sign extension is free, zero extension costs for DImode when
	     on a 64bit core / when DMUL is present.  */
	  for (int i = 0; i < 2; ++i)
	    {
	      rtx op = XEXP (XEXP (XEXP (x, 0), 0), i);
	      if (TARGET_64BIT
		  && GET_CODE (op) == ZERO_EXTEND
		  && GET_MODE (op) == DImode)
		*total += rtx_cost (op, DImode, MULT, i, speed);
	      else
		*total += rtx_cost (XEXP (op, 0), VOIDmode, GET_CODE (op), 0,
				    speed);
	    }

	  return true;
	}
      return false;

    case FLOAT:
    case UNSIGNED_FLOAT:
    case FIX:
    case FLOAT_EXTEND:
    case FLOAT_TRUNCATE:
      *total = loongarch_cost->fp_add;
      return false;

    case SET:
      if (register_operand (SET_DEST (x), VOIDmode)
	  && reg_or_0_operand (SET_SRC (x), VOIDmode))
	{
	  *total = loongarch_set_reg_reg_cost (GET_MODE (SET_DEST (x)));
	  return true;
	}
      return false;

    default:
      return false;
    }
}

/* Implement TARGET_ADDRESS_COST.  */

static int
loongarch_address_cost (rtx addr, machine_mode mode,
			addr_space_t as ATTRIBUTE_UNUSED,
			bool speed ATTRIBUTE_UNUSED)
{
  return loongarch_address_insns (addr, mode, false);
}

/* Return one word of double-word value OP, taking into account the fixed
   endianness of certain registers.  HIGH_P is true to select the high part,
   false to select the low part.  */

rtx
loongarch_subword (rtx op, bool high_p)
{
  unsigned int byte;
  machine_mode mode;

  byte = high_p ? UNITS_PER_WORD : 0;
  mode = GET_MODE (op);
  if (mode == VOIDmode)
    mode = TARGET_64BIT ? TImode : DImode;

  if (FP_REG_RTX_P (op))
    return gen_rtx_REG (word_mode, REGNO (op) + high_p);

  if (MEM_P (op))
    return loongarch_rewrite_small_data (adjust_address (op, word_mode, byte));

  return simplify_gen_subreg (word_mode, op, mode, byte);
}

/* Return true if a move from SRC to DEST should be split into two.
   SPLIT_TYPE describes the split condition.  */

bool
loongarch_split_move_p (rtx dest, rtx src)
{
  /* FPR-to-FPR moves can be done in a single instruction, if they're
     allowed at all.  */
  unsigned int size = GET_MODE_SIZE (GET_MODE (dest));
  if (size == 8 && FP_REG_RTX_P (src) && FP_REG_RTX_P (dest))
    return false;

  /* Check for floating-point loads and stores.  */
  if (size == 8)
    {
      if (FP_REG_RTX_P (dest) && MEM_P (src))
	return false;
      if (FP_REG_RTX_P (src) && MEM_P (dest))
	return false;
    }
  /* Otherwise split all multiword moves.  */
  return size > UNITS_PER_WORD;
}

/* Split a move from SRC to DEST, given that loongarch_split_move_p holds.
   SPLIT_TYPE describes the split condition.  */

void
loongarch_split_move (rtx dest, rtx src, rtx insn_)
{
  rtx low_dest;

  gcc_checking_assert (loongarch_split_move_p (dest, src));
  if (FP_REG_RTX_P (dest) || FP_REG_RTX_P (src))
    {
      if (!TARGET_64BIT && GET_MODE (dest) == DImode)
	emit_insn (gen_move_doubleword_fprdi (dest, src));
      else if (!TARGET_64BIT && GET_MODE (dest) == DFmode)
	emit_insn (gen_move_doubleword_fprdf (dest, src));
      else if (TARGET_64BIT && GET_MODE (dest) == TFmode)
	emit_insn (gen_move_doubleword_fprtf (dest, src));
      else
	gcc_unreachable ();
    }
  else
    {
      /* The operation can be split into two normal moves.  Decide in
	 which order to do them.  */
      low_dest = loongarch_subword (dest, false);
      if (REG_P (low_dest) && reg_overlap_mentioned_p (low_dest, src))
	{
	  loongarch_emit_move (loongarch_subword (dest, true),
			       loongarch_subword (src, true));
	  loongarch_emit_move (low_dest, loongarch_subword (src, false));
	}
      else
	{
	  loongarch_emit_move (low_dest, loongarch_subword (src, false));
	  loongarch_emit_move (loongarch_subword (dest, true),
			       loongarch_subword (src, true));
	}
    }

  /* This is a hack.  See if the next insn uses DEST and if so, see if we
     can forward SRC for DEST.  This is most useful if the next insn is a
     simple store.  */
  rtx_insn *insn = (rtx_insn *) insn_;
  struct loongarch_address_info addr = {};
  if (insn)
    {
      rtx_insn *next = next_nonnote_nondebug_insn_bb (insn);
      if (next)
	{
	  rtx set = single_set (next);
	  if (set && SET_SRC (set) == dest)
	    {
	      if (MEM_P (src))
		{
		  rtx tmp = XEXP (src, 0);
		  loongarch_classify_address (&addr, tmp, GET_MODE (tmp),
					      true);
		  if (addr.reg && !reg_overlap_mentioned_p (dest, addr.reg))
		    validate_change (next, &SET_SRC (set), src, false);
		}
	      else
		validate_change (next, &SET_SRC (set), src, false);
	    }
	}
    }
}

/* Return true if a move from SRC to DEST in INSN should be split.  */

static bool
loongarch_split_move_insn_p (rtx dest, rtx src)
{
  return loongarch_split_move_p (dest, src);
}

/* Implement TARGET_CONSTANT_ALIGNMENT.  */

static HOST_WIDE_INT
loongarch_constant_alignment (const_tree exp, HOST_WIDE_INT align)
{
  if (TREE_CODE (exp) == STRING_CST || TREE_CODE (exp) == CONSTRUCTOR)
    return MAX (align, BITS_PER_WORD);
  return align;
}

const char *
loongarch_output_move_index (rtx x, machine_mode mode, bool ldr)
{
  int index = exact_log2 (GET_MODE_SIZE (mode));
  if (!IN_RANGE (index, 0, 3))
    return NULL;

  struct loongarch_address_info info;
  if ((loongarch_classify_address (&info, x, mode, false)
       && !(info.type == ADDRESS_REG_REG))
      || !loongarch_legitimate_address_p (mode, x, false))
    return NULL;

  const char *const insn[][4] =
    {
      {
	"stx.b\t%z1,%0",
	"stx.h\t%z1,%0",
	"stx.w\t%z1,%0",
	"stx.d\t%z1,%0",
      },
      {
	"ldx.bu\t%0,%1",
	"ldx.hu\t%0,%1",
	"ldx.w\t%0,%1",
	"ldx.d\t%0,%1",
      }
    };

  return insn[ldr][index];
}

const char *
loongarch_output_move_index_float (rtx x, machine_mode mode, bool ldr)
{
  int index = exact_log2 (GET_MODE_SIZE (mode));
  if (!IN_RANGE (index, 2, 3))
    return NULL;

  struct loongarch_address_info info;
  if ((loongarch_classify_address (&info, x, mode, false)
       && !(info.type == ADDRESS_REG_REG))
      || !loongarch_legitimate_address_p (mode, x, false))
    return NULL;

  const char *const insn[][2] =
    {
	{
	  "fstx.s\t%1,%0",
	  "fstx.d\t%1,%0"
	},
	{
	  "fldx.s\t%0,%1",
	  "fldx.d\t%0,%1"
	},
    };

  return insn[ldr][index-2];
}

/* Return the appropriate instructions to move SRC into DEST.  Assume
   that SRC is operand 1 and DEST is operand 0.  */

const char *
loongarch_output_move (rtx dest, rtx src)
{
  enum rtx_code dest_code = GET_CODE (dest);
  enum rtx_code src_code = GET_CODE (src);
  machine_mode mode = GET_MODE (dest);
  bool dbl_p = (GET_MODE_SIZE (mode) == 8);

  if (loongarch_split_move_p (dest, src))
    return "#";

  if ((src_code == REG && GP_REG_P (REGNO (src)))
      || (src == CONST0_RTX (mode)))
    {
      if (dest_code == REG)
	{
	  if (GP_REG_P (REGNO (dest)))
	    return "or\t%0,%z1,$r0";

	  if (FP_REG_P (REGNO (dest)))
	    return dbl_p ? "movgr2fr.d\t%0,%z1" : "movgr2fr.w\t%0,%z1";
	}
      if (dest_code == MEM)
	{
	  const char *insn = NULL;
	  insn = loongarch_output_move_index (XEXP (dest, 0), GET_MODE (dest),
					      false);
	  if (insn)
	    return insn;

	  rtx offset = XEXP (dest, 0);
	  if (GET_CODE (offset) == PLUS)
	    offset = XEXP (offset, 1);
	  switch (GET_MODE_SIZE (mode))
	    {
	    case 1:
	      return "st.b\t%z1,%0";
	    case 2:
	      return "st.h\t%z1,%0";
	    case 4:
	      /* Matching address type with a 12bit offset and
		 ADDRESS_LO_SUM.  */
	      if (const_arith_operand (offset, Pmode)
		  || GET_CODE (offset) == LO_SUM)
		return "st.w\t%z1,%0";
	      else
		return "stptr.w\t%z1,%0";
	    case 8:
	      if (const_arith_operand (offset, Pmode)
		  || GET_CODE (offset) == LO_SUM)
		return "st.d\t%z1,%0";
	      else
		return "stptr.d\t%z1,%0";
	    default:
	      gcc_unreachable ();
	    }
	}
    }
  if (dest_code == REG && GP_REG_P (REGNO (dest)))
    {
      if (src_code == REG)
	if (FP_REG_P (REGNO (src)))
	  return dbl_p ? "movfr2gr.d\t%0,%1" : "movfr2gr.s\t%0,%1";

      if (src_code == MEM)
	{
	  const char *insn = NULL;
	  insn = loongarch_output_move_index (XEXP (src, 0), GET_MODE (src),
					      true);
	  if (insn)
	    return insn;

	  rtx offset = XEXP (src, 0);
	  if (GET_CODE (offset) == PLUS)
	    offset = XEXP (offset, 1);
	  switch (GET_MODE_SIZE (mode))
	    {
	    case 1:
	      return "ld.bu\t%0,%1";
	    case 2:
	      return "ld.hu\t%0,%1";
	    case 4:
	      /* Matching address type with a 12bit offset and
		 ADDRESS_LO_SUM.  */
	      if (const_arith_operand (offset, Pmode)
		  || GET_CODE (offset) == LO_SUM)
		return "ld.w\t%0,%1";
	      else
		return "ldptr.w\t%0,%1";
	    case 8:
	      if (const_arith_operand (offset, Pmode)
		  || GET_CODE (offset) == LO_SUM)
		return "ld.d\t%0,%1";
	      else
		return "ldptr.d\t%0,%1";
	    default:
	      gcc_unreachable ();
	    }
	}

      if (src_code == HIGH)
	{
	  rtx offset, x;
	  split_const (XEXP (src, 0), &x, &offset);
	  enum loongarch_symbol_type type = SYMBOL_PCREL;

	  if (UNSPEC_ADDRESS_P (x))
	     type = UNSPEC_ADDRESS_TYPE (x);

	  if (type == SYMBOL_TLS_LE)
	    return "lu12i.w\t%0,%h1";
	  else
	    return "pcalau12i\t%0,%h1";
	}

      if (src_code == CONST_INT)
	{
	  if (LU12I_INT (src))
	    return "lu12i.w\t%0,%1>>12\t\t\t# %X1";
	  else if (IMM12_INT (src))
	    return "addi.w\t%0,$r0,%1\t\t\t# %X1";
	  else if (IMM12_INT_UNSIGNED (src))
	    return "ori\t%0,$r0,%1\t\t\t# %X1";
	  else if (LU52I_INT (src))
	    return "lu52i.d\t%0,$r0,%X1>>52\t\t\t# %1";
	  else
	    gcc_unreachable ();
	}
    }

  if (!TARGET_EXPLICIT_RELOCS
      && dest_code == REG && symbolic_operand (src, VOIDmode))
    {
      if (loongarch_classify_symbol (src) == SYMBOL_PCREL)
	return "la.local\t%0,%1";
      else
	return "la.global\t%0,%1";
    }

  if (src_code == REG && FP_REG_P (REGNO (src)))
    {
      if (dest_code == REG && FP_REG_P (REGNO (dest)))
	return dbl_p ? "fmov.d\t%0,%1" : "fmov.s\t%0,%1";

      if (dest_code == MEM)
	{
	  const char *insn = NULL;
	  insn = loongarch_output_move_index_float (XEXP (dest, 0),
						    GET_MODE (dest),
						    false);
	  if (insn)
	    return insn;

	  return dbl_p ? "fst.d\t%1,%0" : "fst.s\t%1,%0";
	}
    }

  if (dest_code == REG && FP_REG_P (REGNO (dest)))
    {
      if (src_code == MEM)
	{
	  const char *insn = NULL;
	  insn = loongarch_output_move_index_float (XEXP (src, 0),
						    GET_MODE (src),
						    true);
	  if (insn)
	    return insn;

	  return dbl_p ? "fld.d\t%0,%1" : "fld.s\t%0,%1";
	}
    }

  gcc_unreachable ();
}

/* Return true if CMP1 is a suitable second operand for integer ordering
   test CODE.  */

static bool
loongarch_int_order_operand_ok_p (enum rtx_code code, rtx cmp1)
{
  switch (code)
    {
    case GT:
    case GTU:
      return reg_or_0_operand (cmp1, VOIDmode);

    case GE:
    case GEU:
      return cmp1 == const1_rtx;

    case LT:
    case LTU:
      return arith_operand (cmp1, VOIDmode);

    case LE:
      return sle_operand (cmp1, VOIDmode);

    case LEU:
      return sleu_operand (cmp1, VOIDmode);

    default:
      gcc_unreachable ();
    }
}

/* Return true if *CMP1 (of mode MODE) is a valid second operand for
   integer ordering test *CODE, or if an equivalent combination can
   be formed by adjusting *CODE and *CMP1.  When returning true, update
   *CODE and *CMP1 with the chosen code and operand, otherwise leave
   them alone.  */

static bool
loongarch_canonicalize_int_order_test (enum rtx_code *code, rtx *cmp1,
				       machine_mode mode)
{
  HOST_WIDE_INT plus_one;

  if (loongarch_int_order_operand_ok_p (*code, *cmp1))
    return true;

  if (CONST_INT_P (*cmp1))
    switch (*code)
      {
      case LE:
	plus_one = trunc_int_for_mode (UINTVAL (*cmp1) + 1, mode);
	if (INTVAL (*cmp1) < plus_one)
	  {
	    *code = LT;
	    *cmp1 = force_reg (mode, GEN_INT (plus_one));
	    return true;
	  }
	break;

      case LEU:
	plus_one = trunc_int_for_mode (UINTVAL (*cmp1) + 1, mode);
	if (plus_one != 0)
	  {
	    *code = LTU;
	    *cmp1 = force_reg (mode, GEN_INT (plus_one));
	    return true;
	  }
	break;

      default:
	break;
      }
  return false;
}

/* Compare CMP0 and CMP1 using ordering test CODE and store the result
   in TARGET.  CMP0 and TARGET are register_operands.  If INVERT_PTR
   is nonnull, it's OK to set TARGET to the inverse of the result and
   flip *INVERT_PTR instead.  */

static void
loongarch_emit_int_order_test (enum rtx_code code, bool *invert_ptr,
			       rtx target, rtx cmp0, rtx cmp1)
{
  machine_mode mode;

  /* First see if there is a LoongArch instruction that can do this operation.
     If not, try doing the same for the inverse operation.  If that also
     fails, force CMP1 into a register and try again.  */
  mode = GET_MODE (cmp0);
  if (loongarch_canonicalize_int_order_test (&code, &cmp1, mode))
    loongarch_emit_binary (code, target, cmp0, cmp1);
  else
    {
      enum rtx_code inv_code = reverse_condition (code);
      if (!loongarch_canonicalize_int_order_test (&inv_code, &cmp1, mode))
	{
	  cmp1 = force_reg (mode, cmp1);
	  loongarch_emit_int_order_test (code, invert_ptr, target, cmp0, cmp1);
	}
      else if (invert_ptr == 0)
	{
	  rtx inv_target;

	  inv_target = loongarch_force_binary (GET_MODE (target),
					       inv_code, cmp0, cmp1);
	  loongarch_emit_binary (XOR, target, inv_target, const1_rtx);
	}
      else
	{
	  *invert_ptr = !*invert_ptr;
	  loongarch_emit_binary (inv_code, target, cmp0, cmp1);
	}
    }
}

/* Return a register that is zero if CMP0 and CMP1 are equal.
   The register will have the same mode as CMP0.  */

static rtx
loongarch_zero_if_equal (rtx cmp0, rtx cmp1)
{
  if (cmp1 == const0_rtx)
    return cmp0;

  if (uns_arith_operand (cmp1, VOIDmode))
    return expand_binop (GET_MODE (cmp0), xor_optab, cmp0, cmp1, 0, 0,
			 OPTAB_DIRECT);

  return expand_binop (GET_MODE (cmp0), sub_optab, cmp0, cmp1, 0, 0,
		       OPTAB_DIRECT);
}

/* Allocate a floating-point condition-code register of mode MODE.  */

static rtx
loongarch_allocate_fcc (machine_mode mode)
{
  unsigned int regno, count;

  gcc_assert (TARGET_HARD_FLOAT);

  if (mode == FCCmode)
    count = 1;
  else
    gcc_unreachable ();

  cfun->machine->next_fcc += -cfun->machine->next_fcc & (count - 1);
  if (cfun->machine->next_fcc > FCC_REG_LAST - FCC_REG_FIRST)
    cfun->machine->next_fcc = 0;

  regno = FCC_REG_FIRST + cfun->machine->next_fcc;
  cfun->machine->next_fcc += count;
  return gen_rtx_REG (mode, regno);
}

/* Sign- or zero-extend OP0 and OP1 for integer comparisons.  */

static void
loongarch_extend_comparands (rtx_code code, rtx *op0, rtx *op1)
{
  /* Comparisons consider all XLEN bits, so extend sub-XLEN values.  */
  if (GET_MODE_SIZE (word_mode) > GET_MODE_SIZE (GET_MODE (*op0)))
    {
      /* TODO: checkout It is more profitable to zero-extend QImode values.  */
      if (unsigned_condition (code) == code && GET_MODE (*op0) == QImode)
	{
	  *op0 = gen_rtx_ZERO_EXTEND (word_mode, *op0);
	  if (CONST_INT_P (*op1))
	    *op1 = GEN_INT ((uint8_t) INTVAL (*op1));
	  else
	    *op1 = gen_rtx_ZERO_EXTEND (word_mode, *op1);
	}
      else
	{
	  *op0 = gen_rtx_SIGN_EXTEND (word_mode, *op0);
	  if (*op1 != const0_rtx)
	    *op1 = gen_rtx_SIGN_EXTEND (word_mode, *op1);
	}
    }
}

/* Convert a comparison into something that can be used in a branch.  On
   entry, *OP0 and *OP1 are the values being compared and *CODE is the code
   used to compare them.  Update them to describe the final comparison.  */

static void
loongarch_emit_int_compare (enum rtx_code *code, rtx *op0, rtx *op1)
{
  static const enum rtx_code
  mag_comparisons[][2] = {{LEU, LTU}, {GTU, GEU}, {LE, LT}, {GT, GE}};

  if (splittable_const_int_operand (*op1, VOIDmode))
    {
      HOST_WIDE_INT rhs = INTVAL (*op1);

      if (*code == EQ || *code == NE)
	{
	  /* Convert e.g. OP0 == 2048 into OP0 - 2048 == 0.  */
	  if (IMM12_OPERAND (-rhs))
	    {
	      *op0 = loongarch_force_binary (GET_MODE (*op0), PLUS, *op0,
					     GEN_INT (-rhs));
	      *op1 = const0_rtx;
	    }
	}
      else
	{
	  /* Convert e.g. (OP0 <= 0xFFF) into (OP0 < 0x1000).  */
	  for (size_t i = 0; i < ARRAY_SIZE (mag_comparisons); i++)
	    {
	      HOST_WIDE_INT new_rhs;
	      bool increment = *code == mag_comparisons[i][0];
	      bool decrement = *code == mag_comparisons[i][1];
	      if (!increment && !decrement)
		continue;

	      if ((increment && rhs == HOST_WIDE_INT_MAX)
		  || (decrement && rhs == HOST_WIDE_INT_MIN))
		break;

	      new_rhs = rhs + (increment ? 1 : -1);
	      if (loongarch_integer_cost (new_rhs)
		    < loongarch_integer_cost (rhs))
		{
		  *op1 = GEN_INT (new_rhs);
		  *code = mag_comparisons[i][increment];
		}
	      break;
	    }
	}
    }

  loongarch_extend_comparands (*code, op0, op1);

  *op0 = force_reg (word_mode, *op0);
  if (*op1 != const0_rtx)
    *op1 = force_reg (word_mode, *op1);
}

/* Like loongarch_emit_int_compare, but for floating-point comparisons.  */

static void
loongarch_emit_float_compare (enum rtx_code *code, rtx *op0, rtx *op1)
{
  rtx cmp_op0 = *op0;
  rtx cmp_op1 = *op1;

  /* Floating-point tests use a separate FCMP.cond.fmt
     comparison to set a register.  The branch or conditional move will
     then compare that register against zero.

     Set CMP_CODE to the code of the comparison instruction and
     *CODE to the code that the branch or move should use.  */
  enum rtx_code cmp_code = *code;
  /* Three FP conditions cannot be implemented by reversing the
     operands for FCMP.cond.fmt, instead a reversed condition code is
     required and a test for false.  */
  *code = NE;
  *op0 = loongarch_allocate_fcc (FCCmode);

  *op1 = const0_rtx;
  loongarch_emit_binary (cmp_code, *op0, cmp_op0, cmp_op1);
}

/* Try performing the comparison in OPERANDS[1], whose arms are OPERANDS[2]
   and OPERAND[3].  Store the result in OPERANDS[0].

   On 64-bit targets, the mode of the comparison and target will always be
   SImode, thus possibly narrower than that of the comparison's operands.  */

void
loongarch_expand_scc (rtx operands[])
{
  rtx target = operands[0];
  enum rtx_code code = GET_CODE (operands[1]);
  rtx op0 = operands[2];
  rtx op1 = operands[3];

  loongarch_extend_comparands (code, &op0, &op1);
  op0 = force_reg (word_mode, op0);

  gcc_assert (GET_MODE_CLASS (GET_MODE (op0)) == MODE_INT);

  if (code == EQ || code == NE)
    {
      rtx zie = loongarch_zero_if_equal (op0, op1);
      loongarch_emit_binary (code, target, zie, const0_rtx);
    }
  else
    loongarch_emit_int_order_test (code, 0, target, op0, op1);
}

/* Compare OPERANDS[1] with OPERANDS[2] using comparison code
   CODE and jump to OPERANDS[3] if the condition holds.  */

void
loongarch_expand_conditional_branch (rtx *operands)
{
  enum rtx_code code = GET_CODE (operands[0]);
  rtx op0 = operands[1];
  rtx op1 = operands[2];
  rtx condition;

  if (FLOAT_MODE_P (GET_MODE (op1)))
    loongarch_emit_float_compare (&code, &op0, &op1);
  else
    loongarch_emit_int_compare (&code, &op0, &op1);

  condition = gen_rtx_fmt_ee (code, VOIDmode, op0, op1);
  emit_jump_insn (gen_condjump (condition, operands[3]));
}

/* Perform the comparison in OPERANDS[1].  Move OPERANDS[2] into OPERANDS[0]
   if the condition holds, otherwise move OPERANDS[3] into OPERANDS[0].  */

void
loongarch_expand_conditional_move (rtx *operands)
{
  enum rtx_code code = GET_CODE (operands[1]);
  rtx op0 = XEXP (operands[1], 0);
  rtx op1 = XEXP (operands[1], 1);

  if (FLOAT_MODE_P (GET_MODE (op1)))
    loongarch_emit_float_compare (&code, &op0, &op1);
  else
    {
      loongarch_extend_comparands (code, &op0, &op1);

      op0 = force_reg (word_mode, op0);

      if (code == EQ || code == NE)
	{
	  op0 = loongarch_zero_if_equal (op0, op1);
	  op1 = const0_rtx;
	}
      else
	{
	  /* The comparison needs a separate scc instruction.  Store the
	     result of the scc in *OP0 and compare it against zero.  */
	  bool invert = false;
	  rtx target = gen_reg_rtx (GET_MODE (op0));
	  loongarch_emit_int_order_test (code, &invert, target, op0, op1);
	  code = invert ? EQ : NE;
	  op0 = target;
	  op1 = const0_rtx;
	}
    }

  rtx cond = gen_rtx_fmt_ee (code, GET_MODE (op0), op0, op1);
  /* There is no direct support for general conditional GP move involving
     two registers using SEL.  */
  if (INTEGRAL_MODE_P (GET_MODE (operands[2]))
      && register_operand (operands[2], VOIDmode)
      && register_operand (operands[3], VOIDmode))
    {
      machine_mode mode = GET_MODE (operands[0]);
      rtx temp = gen_reg_rtx (mode);
      rtx temp2 = gen_reg_rtx (mode);

      emit_insn (gen_rtx_SET (temp,
			      gen_rtx_IF_THEN_ELSE (mode, cond,
						    operands[2], const0_rtx)));

      /* Flip the test for the second operand.  */
      cond = gen_rtx_fmt_ee ((code == EQ) ? NE : EQ, GET_MODE (op0), op0, op1);

      emit_insn (gen_rtx_SET (temp2,
			      gen_rtx_IF_THEN_ELSE (mode, cond,
						    operands[3], const0_rtx)));

      /* Merge the two results, at least one is guaranteed to be zero.  */
      emit_insn (gen_rtx_SET (operands[0], gen_rtx_IOR (mode, temp, temp2)));
    }
  else
    emit_insn (gen_rtx_SET (operands[0],
			    gen_rtx_IF_THEN_ELSE (GET_MODE (operands[0]), cond,
						  operands[2], operands[3])));
}

/* Implement TARGET_EXPAND_BUILTIN_VA_START.  */

static void
loongarch_va_start (tree valist, rtx nextarg)
{
  nextarg = plus_constant (Pmode, nextarg, -cfun->machine->varargs_size);
  std_expand_builtin_va_start (valist, nextarg);
}

/* Implement TARGET_FUNCTION_OK_FOR_SIBCALL.  */

static bool
loongarch_function_ok_for_sibcall (tree decl ATTRIBUTE_UNUSED,
				   tree exp ATTRIBUTE_UNUSED)
{
  /* Always OK.  */
  return true;
}

/* Emit straight-line code to move LENGTH bytes from SRC to DEST.
   Assume that the areas do not overlap.  */

static void
loongarch_block_move_straight (rtx dest, rtx src, HOST_WIDE_INT length)
{
  HOST_WIDE_INT offset, delta;
  unsigned HOST_WIDE_INT bits;
  int i;
  machine_mode mode;
  rtx *regs;

  bits = MIN (BITS_PER_WORD, MIN (MEM_ALIGN (src), MEM_ALIGN (dest)));

  mode = int_mode_for_size (bits, 0).require ();
  delta = bits / BITS_PER_UNIT;

  /* Allocate a buffer for the temporary registers.  */
  regs = XALLOCAVEC (rtx, length / delta);

  /* Load as many BITS-sized chunks as possible.  Use a normal load if
     the source has enough alignment, otherwise use left/right pairs.  */
  for (offset = 0, i = 0; offset + delta <= length; offset += delta, i++)
    {
      regs[i] = gen_reg_rtx (mode);
      loongarch_emit_move (regs[i], adjust_address (src, mode, offset));
    }

  for (offset = 0, i = 0; offset + delta <= length; offset += delta, i++)
    loongarch_emit_move (adjust_address (dest, mode, offset), regs[i]);

  /* Mop up any left-over bytes.  */
  if (offset < length)
    {
      src = adjust_address (src, BLKmode, offset);
      dest = adjust_address (dest, BLKmode, offset);
      move_by_pieces (dest, src, length - offset,
		      MIN (MEM_ALIGN (src), MEM_ALIGN (dest)),
		      (enum memop_ret) 0);
    }
}

/* Helper function for doing a loop-based block operation on memory
   reference MEM.  Each iteration of the loop will operate on LENGTH
   bytes of MEM.

   Create a new base register for use within the loop and point it to
   the start of MEM.  Create a new memory reference that uses this
   register.  Store them in *LOOP_REG and *LOOP_MEM respectively.  */

static void
loongarch_adjust_block_mem (rtx mem, HOST_WIDE_INT length, rtx *loop_reg,
			    rtx *loop_mem)
{
  *loop_reg = copy_addr_to_reg (XEXP (mem, 0));

  /* Although the new mem does not refer to a known location,
     it does keep up to LENGTH bytes of alignment.  */
  *loop_mem = change_address (mem, BLKmode, *loop_reg);
  set_mem_align (*loop_mem, MIN (MEM_ALIGN (mem), length * BITS_PER_UNIT));
}

/* Move LENGTH bytes from SRC to DEST using a loop that moves BYTES_PER_ITER
   bytes at a time.  LENGTH must be at least BYTES_PER_ITER.  Assume that
   the memory regions do not overlap.  */

static void
loongarch_block_move_loop (rtx dest, rtx src, HOST_WIDE_INT length,
			   HOST_WIDE_INT bytes_per_iter)
{
  rtx_code_label *label;
  rtx src_reg, dest_reg, final_src, test;
  HOST_WIDE_INT leftover;

  leftover = length % bytes_per_iter;
  length -= leftover;

  /* Create registers and memory references for use within the loop.  */
  loongarch_adjust_block_mem (src, bytes_per_iter, &src_reg, &src);
  loongarch_adjust_block_mem (dest, bytes_per_iter, &dest_reg, &dest);

  /* Calculate the value that SRC_REG should have after the last iteration
     of the loop.  */
  final_src = expand_simple_binop (Pmode, PLUS, src_reg, GEN_INT (length), 0,
				   0, OPTAB_WIDEN);

  /* Emit the start of the loop.  */
  label = gen_label_rtx ();
  emit_label (label);

  /* Emit the loop body.  */
  loongarch_block_move_straight (dest, src, bytes_per_iter);

  /* Move on to the next block.  */
  loongarch_emit_move (src_reg,
		       plus_constant (Pmode, src_reg, bytes_per_iter));
  loongarch_emit_move (dest_reg,
		       plus_constant (Pmode, dest_reg, bytes_per_iter));

  /* Emit the loop condition.  */
  test = gen_rtx_NE (VOIDmode, src_reg, final_src);
  if (Pmode == DImode)
    emit_jump_insn (gen_cbranchdi4 (test, src_reg, final_src, label));
  else
    emit_jump_insn (gen_cbranchsi4 (test, src_reg, final_src, label));

  /* Mop up any left-over bytes.  */
  if (leftover)
    loongarch_block_move_straight (dest, src, leftover);
  else
    /* Temporary fix for PR79150.  */
    emit_insn (gen_nop ());
}

/* Expand a cpymemsi instruction, which copies LENGTH bytes from
   memory reference SRC to memory reference DEST.  */

bool
loongarch_expand_block_move (rtx dest, rtx src, rtx length)
{
  int max_move_bytes = LARCH_MAX_MOVE_BYTES_STRAIGHT;

  if (CONST_INT_P (length)
      && INTVAL (length) <= loongarch_max_inline_memcpy_size)
    {
      if (INTVAL (length) <= max_move_bytes)
	{
	  loongarch_block_move_straight (dest, src, INTVAL (length));
	  return true;
	}
      else if (optimize)
	{
	  loongarch_block_move_loop (dest, src, INTVAL (length),
				     LARCH_MAX_MOVE_BYTES_PER_LOOP_ITER);
	  return true;
	}
    }
  return false;
}

/* Return true if loongarch_expand_block_move is the preferred
   implementation of the 'cpymemsi' template.  */

bool
loongarch_do_optimize_block_move_p (void)
{
  /* if -m[no-]memcpy is given explicitly.  */
  if (target_flags_explicit & MASK_MEMCPY)
    return !TARGET_MEMCPY;

  /* if not, don't optimize under -Os.  */
  return !optimize_size;
}

/* Expand a QI or HI mode atomic memory operation.

   GENERATOR contains a pointer to the gen_* function that generates
   the SI mode underlying atomic operation using masks that we
   calculate.

   RESULT is the return register for the operation.  Its value is NULL
   if unused.

   MEM is the location of the atomic access.

   OLDVAL is the first operand for the operation.

   NEWVAL is the optional second operand for the operation.  Its value
   is NULL if unused.  */

void
loongarch_expand_atomic_qihi (union loongarch_gen_fn_ptrs generator,
			      rtx result, rtx mem, rtx oldval, rtx newval,
			      rtx model)
{
  rtx orig_addr, memsi_addr, memsi, shift, shiftsi, unshifted_mask;
  rtx unshifted_mask_reg, mask, inverted_mask, si_op;
  rtx res = NULL;
  machine_mode mode;

  mode = GET_MODE (mem);

  /* Compute the address of the containing SImode value.  */
  orig_addr = force_reg (Pmode, XEXP (mem, 0));
  memsi_addr = loongarch_force_binary (Pmode, AND, orig_addr,
				       force_reg (Pmode, GEN_INT (-4)));

  /* Create a memory reference for it.  */
  memsi = gen_rtx_MEM (SImode, memsi_addr);
  set_mem_alias_set (memsi, ALIAS_SET_MEMORY_BARRIER);
  MEM_VOLATILE_P (memsi) = MEM_VOLATILE_P (mem);

  /* Work out the byte offset of the QImode or HImode value,
     counting from the least significant byte.  */
  shift = loongarch_force_binary (Pmode, AND, orig_addr, GEN_INT (3));
  /* Multiply by eight to convert the shift value from bytes to bits.  */
  loongarch_emit_binary (ASHIFT, shift, shift, GEN_INT (3));

  /* Make the final shift an SImode value, so that it can be used in
     SImode operations.  */
  shiftsi = force_reg (SImode, gen_lowpart (SImode, shift));

  /* Set MASK to an inclusive mask of the QImode or HImode value.  */
  unshifted_mask = GEN_INT (GET_MODE_MASK (mode));
  unshifted_mask_reg = force_reg (SImode, unshifted_mask);
  mask = loongarch_force_binary (SImode, ASHIFT, unshifted_mask_reg, shiftsi);

  /* Compute the equivalent exclusive mask.  */
  inverted_mask = gen_reg_rtx (SImode);
  emit_insn (gen_rtx_SET (inverted_mask, gen_rtx_NOT (SImode, mask)));

  /* Shift the old value into place.  */
  if (oldval != const0_rtx)
    {
      oldval = convert_modes (SImode, mode, oldval, true);
      oldval = force_reg (SImode, oldval);
      oldval = loongarch_force_binary (SImode, ASHIFT, oldval, shiftsi);
    }

  /* Do the same for the new value.  */
  if (newval && newval != const0_rtx)
    {
      newval = convert_modes (SImode, mode, newval, true);
      newval = force_reg (SImode, newval);
      newval = loongarch_force_binary (SImode, ASHIFT, newval, shiftsi);
    }

  /* Do the SImode atomic access.  */
  if (result)
    res = gen_reg_rtx (SImode);

  if (newval)
    si_op = generator.fn_7 (res, memsi, mask, inverted_mask, oldval, newval,
			    model);
  else if (result)
    si_op = generator.fn_6 (res, memsi, mask, inverted_mask, oldval, model);
  else
    si_op = generator.fn_5 (memsi, mask, inverted_mask, oldval, model);

  emit_insn (si_op);

  if (result)
    {
      /* Shift and convert the result.  */
      loongarch_emit_binary (AND, res, res, mask);
      loongarch_emit_binary (LSHIFTRT, res, res, shiftsi);
      loongarch_emit_move (result, gen_lowpart (GET_MODE (result), res));
    }
}

/* Return true if (zero_extract OP WIDTH BITPOS) can be used as the
   source of an "ext" instruction or the destination of an "ins"
   instruction.  OP must be a register operand and the following
   conditions must hold:

   0 <= BITPOS < GET_MODE_BITSIZE (GET_MODE (op))
   0 < WIDTH <= GET_MODE_BITSIZE (GET_MODE (op))
   0 < BITPOS + WIDTH <= GET_MODE_BITSIZE (GET_MODE (op))

   Also reject lengths equal to a word as they are better handled
   by the move patterns.  */

bool
loongarch_use_ins_ext_p (rtx op, HOST_WIDE_INT width, HOST_WIDE_INT bitpos)
{
  if (!register_operand (op, VOIDmode)
      || GET_MODE_BITSIZE (GET_MODE (op)) > BITS_PER_WORD)
    return false;

  if (!IN_RANGE (width, 1, GET_MODE_BITSIZE (GET_MODE (op)) - 1))
    return false;

  if (bitpos < 0 || bitpos + width > GET_MODE_BITSIZE (GET_MODE (op)))
    return false;

  return true;
}

/* Print the text for PRINT_OPERAND punctation character CH to FILE.
   The punctuation characters are:

   '.'	Print the name of the register with a hard-wired zero (zero or $r0).
   '$'	Print the name of the stack pointer register (sp or $r3).

   See also loongarch_init_print_operand_punct.  */

static void
loongarch_print_operand_punctuation (FILE *file, int ch)
{
  switch (ch)
    {
    case '.':
      fputs (reg_names[GP_REG_FIRST + 0], file);
      break;

    case '$':
      fputs (reg_names[STACK_POINTER_REGNUM], file);
      break;

    default:
      gcc_unreachable ();
      break;
    }
}

/* Initialize loongarch_print_operand_punct.  */

static void
loongarch_init_print_operand_punct (void)
{
  const char *p;

  for (p = ".$"; *p; p++)
    loongarch_print_operand_punct[(unsigned char) *p] = true;
}

/* PRINT_OPERAND prefix LETTER refers to the integer branch instruction
   associated with condition CODE.  Print the condition part of the
   opcode to FILE.  */

static void
loongarch_print_int_branch_condition (FILE *file, enum rtx_code code,
				      int letter)
{
  switch (code)
    {
    case EQ:
    case NE:
    case GT:
    case GE:
    case LT:
    case LE:
    case GTU:
    case GEU:
    case LTU:
    case LEU:
      /* Conveniently, the LoongArch names for these conditions are the same
	 as their RTL equivalents.  */
      fputs (GET_RTX_NAME (code), file);
      break;

    default:
      output_operand_lossage ("'%%%c' is not a valid operand prefix", letter);
      break;
    }
}

/* Likewise floating-point branches.  */

static void
loongarch_print_float_branch_condition (FILE *file, enum rtx_code code,
					int letter)
{
  switch (code)
    {
    case EQ:
      fputs ("ceqz", file);
      break;

    case NE:
      fputs ("cnez", file);
      break;

    default:
      output_operand_lossage ("'%%%c' is not a valid operand prefix", letter);
      break;
    }
}

/* Implement TARGET_PRINT_OPERAND_PUNCT_VALID_P.  */

static bool
loongarch_print_operand_punct_valid_p (unsigned char code)
{
  return loongarch_print_operand_punct[code];
}

/* Return true if a FENCE should be emitted to before a memory access to
   implement the release portion of memory model MODEL.  */

static bool
loongarch_memmodel_needs_rel_acq_fence (enum memmodel model)
{
  switch (model)
    {
      case MEMMODEL_ACQ_REL:
      case MEMMODEL_SEQ_CST:
      case MEMMODEL_SYNC_SEQ_CST:
      case MEMMODEL_RELEASE:
      case MEMMODEL_SYNC_RELEASE:
      case MEMMODEL_ACQUIRE:
      case MEMMODEL_CONSUME:
      case MEMMODEL_SYNC_ACQUIRE:
	return true;

      case MEMMODEL_RELAXED:
	return false;

      default:
	gcc_unreachable ();
    }
}

/* Return true if a FENCE should be emitted to before a memory access to
   implement the release portion of memory model MODEL.  */

static bool
loongarch_memmodel_needs_release_fence (enum memmodel model)
{
  switch (model)
    {
    case MEMMODEL_ACQ_REL:
    case MEMMODEL_SEQ_CST:
    case MEMMODEL_SYNC_SEQ_CST:
    case MEMMODEL_RELEASE:
    case MEMMODEL_SYNC_RELEASE:
      return true;

    case MEMMODEL_ACQUIRE:
    case MEMMODEL_CONSUME:
    case MEMMODEL_SYNC_ACQUIRE:
    case MEMMODEL_RELAXED:
      return false;

    default:
      gcc_unreachable ();
    }
}

/* Print symbolic operand OP, which is part of a HIGH or LO_SUM
   in context CONTEXT.  HI_RELOC indicates a high-part reloc.  */

static void
loongarch_print_operand_reloc (FILE *file, rtx op, bool hi64_part,
			       bool hi_reloc)
{
  const char *reloc;
  enum loongarch_symbol_type symbol_type =
    loongarch_classify_symbolic_expression (op);

  if (loongarch_symbol_extreme_p (symbol_type))
    gcc_assert (TARGET_EXPLICIT_RELOCS);

  switch (symbol_type)
    {
    case SYMBOL_PCREL64:
      if (hi64_part)
	{
	  reloc = hi_reloc ? "%pc64_hi12" : "%pc64_lo20";
	  break;
	}
      /* fall through */
    case SYMBOL_PCREL:
      reloc = hi_reloc ? "%pc_hi20" : "%pc_lo12";
      break;

    case SYMBOL_GOT_DISP:
      if (hi64_part)
	{
	  if (TARGET_CMODEL_EXTREME)
	    reloc = hi_reloc ? "%got64_pc_hi12" : "%got64_pc_lo20";
	  else
	    gcc_unreachable ();
	}
      else
	reloc = hi_reloc ? "%got_pc_hi20" : "%got_pc_lo12";
      break;

    case SYMBOL_TLS_IE:
      if (hi64_part)
	{
	  if (TARGET_CMODEL_EXTREME)
	    reloc = hi_reloc ? "%ie64_pc_hi12" : "%ie64_pc_lo20";
	  else
	    gcc_unreachable ();
	}
      else
	reloc = hi_reloc ? "%ie_pc_hi20" : "%ie_pc_lo12";
      break;

    case SYMBOL_TLS_LE:
      if (hi64_part)
	{
	  if (TARGET_CMODEL_EXTREME)
	    reloc = hi_reloc ? "%le64_hi12" : "%le64_lo20";
	  else
	    gcc_unreachable ();
	}
      else
	reloc = hi_reloc ? "%le_hi20" : "%le_lo12";
      break;

    case SYMBOL_TLSGD:
      if (hi64_part)
	{
	  if (TARGET_CMODEL_EXTREME)
	    reloc = hi_reloc ? "%got64_pc_hi12" : "%got64_pc_lo20";
	  else
	    gcc_unreachable ();
	}
      else
	reloc = hi_reloc ? "%gd_pc_hi20" : "%got_pc_lo12";
      break;

    case SYMBOL_TLSLDM:
      if (hi64_part)
	{
	  if (TARGET_CMODEL_EXTREME)
	    reloc = hi_reloc ? "%got64_pc_hi12" : "%got64_pc_lo20";
	  else
	    gcc_unreachable ();
	}
      else
	reloc = hi_reloc ? "%ld_pc_hi20" : "%got_pc_lo12";
      break;

    default:
      gcc_unreachable ();
    }

  fprintf (file, "%s(", reloc);
  output_addr_const (file, loongarch_strip_unspec_address (op));
  fputc (')', file);
}

/* Implement TARGET_PRINT_OPERAND.  The LoongArch-specific operand codes are:

   'A'	Print a _DB suffix if the memory model requires a release.
   'b'	Print the address of a memory operand, without offset.
   'c'  Print an integer.
   'C'	Print the integer branch condition for comparison OP.
   'd'	Print CONST_INT OP in decimal.
   'F'	Print the FPU branch condition for comparison OP.
   'G'	Print a DBAR insn if the memory model requires a release.
   'H'  Print address 52-61bit relocation associated with OP.
   'h'  Print the high-part relocation associated with OP.
   'i'	Print i if the operand is not a register.
   'L'  Print the low-part relocation associated with OP.
   'm'	Print one less than CONST_INT OP in decimal.
   'N'	Print the inverse of the integer branch condition for comparison OP.
   'r'  Print address 12-31bit relocation associated with OP.
   'R'  Print address 32-51bit relocation associated with OP.
   'T'	Print 'f' for (eq:CC ...), 't' for (ne:CC ...),
	      'z' for (eq:?I ...), 'n' for (ne:?I ...).
   't'	Like 'T', but with the EQ/NE cases reversed
   'V'	Print exact log2 of CONST_INT OP element 0 of a replicated
	  CONST_VECTOR in decimal.
   'W'	Print the inverse of the FPU branch condition for comparison OP.
   'X'	Print CONST_INT OP in hexadecimal format.
   'x'	Print the low 16 bits of CONST_INT OP in hexadecimal format.
   'Y'	Print loongarch_fp_conditions[INTVAL (OP)]
   'y'	Print exact log2 of CONST_INT OP in decimal.
   'Z'	Print OP and a comma for 8CC, otherwise print nothing.
   'z'	Print $0 if OP is zero, otherwise print OP normally.  */

static void
loongarch_print_operand (FILE *file, rtx op, int letter)
{
  enum rtx_code code;

  if (loongarch_print_operand_punct_valid_p (letter))
    {
      loongarch_print_operand_punctuation (file, letter);
      return;
    }

  gcc_assert (op);
  code = GET_CODE (op);

  switch (letter)
    {
    case 'A':
      if (loongarch_memmodel_needs_rel_acq_fence ((enum memmodel) INTVAL (op)))
       fputs ("_db", file);
      break;

    case 'c':
      if (CONST_INT_P (op))
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (op));
      else
	output_operand_lossage ("unsupported operand for code '%c'", letter);

      break;

    case 'C':
      loongarch_print_int_branch_condition (file, code, letter);
      break;

    case 'd':
      if (CONST_INT_P (op))
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (op));
      else
	output_operand_lossage ("invalid use of '%%%c'", letter);
      break;

    case 'F':
      loongarch_print_float_branch_condition (file, code, letter);
      break;

    case 'G':
      if (loongarch_memmodel_needs_release_fence ((enum memmodel) INTVAL (op)))
	fputs ("dbar\t0", file);
      break;

    case 'h':
      if (code == HIGH)
	op = XEXP (op, 0);
      loongarch_print_operand_reloc (file, op, false /* hi64_part */,
				     true /* hi_reloc */);
      break;

    case 'H':
      loongarch_print_operand_reloc (file, op, true /* hi64_part */,
				     true /* hi_reloc */);
      break;

    case 'i':
      if (code != REG)
	fputs ("i", file);
      break;

    case 'L':
      loongarch_print_operand_reloc (file, op, false /* hi64_part*/,
				     false /* lo_reloc */);
      break;

    case 'm':
      if (CONST_INT_P (op))
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (op) - 1);
      else
	output_operand_lossage ("invalid use of '%%%c'", letter);
      break;

    case 'N':
      loongarch_print_int_branch_condition (file, reverse_condition (code),
					    letter);
      break;

    case 'r':
      loongarch_print_operand_reloc (file, op, false /* hi64_part */,
				     true /* lo_reloc */);
      break;

    case 'R':
      loongarch_print_operand_reloc (file, op, true /* hi64_part */,
				     false /* lo_reloc */);
      break;

    case 't':
    case 'T':
      {
	int truth = (code == NE) == (letter == 'T');
	fputc ("zfnt"[truth * 2 + FCC_REG_P (REGNO (XEXP (op, 0)))], file);
      }
      break;

    case 'V':
      if (CONST_VECTOR_P (op))
	{
	  machine_mode mode = GET_MODE_INNER (GET_MODE (op));
	  unsigned HOST_WIDE_INT val = UINTVAL (CONST_VECTOR_ELT (op, 0));
	  int vlog2 = exact_log2 (val & GET_MODE_MASK (mode));
	  if (vlog2 != -1)
	    fprintf (file, "%d", vlog2);
	  else
	    output_operand_lossage ("invalid use of '%%%c'", letter);
	}
      else
	output_operand_lossage ("invalid use of '%%%c'", letter);
      break;

    case 'W':
      loongarch_print_float_branch_condition (file, reverse_condition (code),
					      letter);
      break;

    case 'x':
      if (CONST_INT_P (op))
	fprintf (file, HOST_WIDE_INT_PRINT_HEX, INTVAL (op) & 0xffff);
      else
	output_operand_lossage ("invalid use of '%%%c'", letter);
      break;

    case 'X':
      if (CONST_INT_P (op))
	fprintf (file, HOST_WIDE_INT_PRINT_HEX, INTVAL (op));
      else
	output_operand_lossage ("invalid use of '%%%c'", letter);
      break;

    case 'y':
      if (CONST_INT_P (op))
	{
	  int val = exact_log2 (INTVAL (op));
	  if (val != -1)
	    fprintf (file, "%d", val);
	  else
	    output_operand_lossage ("invalid use of '%%%c'", letter);
	}
      else
	output_operand_lossage ("invalid use of '%%%c'", letter);
      break;

    case 'Y':
      if (code == CONST_INT
	  && UINTVAL (op) < ARRAY_SIZE (loongarch_fp_conditions))
	fputs (loongarch_fp_conditions[UINTVAL (op)], file);
      else
	output_operand_lossage ("'%%%c' is not a valid operand prefix",
				letter);
      break;

    case 'Z':
      loongarch_print_operand (file, op, 0);
      fputc (',', file);
      break;

    default:
      switch (code)
	{
	case REG:
	  {
	    unsigned int regno = REGNO (op);
	    if (letter && letter != 'z')
	      output_operand_lossage ("invalid use of '%%%c'", letter);
	    fprintf (file, "%s", reg_names[regno]);
	  }
	  break;

	case MEM:
	  if (letter == 'D')
	    output_address (GET_MODE (op),
			    plus_constant (Pmode, XEXP (op, 0), 4));
	  else if (letter == 'b')
	    {
	      gcc_assert (REG_P (XEXP (op, 0)));
	      loongarch_print_operand (file, XEXP (op, 0), 0);
	    }
	  else if (letter && letter != 'z')
	    output_operand_lossage ("invalid use of '%%%c'", letter);
	  else
	    output_address (GET_MODE (op), XEXP (op, 0));
	  break;

	default:
	  if (letter == 'z' && op == CONST0_RTX (GET_MODE (op)))
	    fputs (reg_names[GP_REG_FIRST], file);
	  else if (letter && letter != 'z')
	    output_operand_lossage ("invalid use of '%%%c'", letter);
	  else
	    output_addr_const (file, loongarch_strip_unspec_address (op));
	  break;
	}
    }
}

/* Implement TARGET_PRINT_OPERAND_ADDRESS.  */

static void
loongarch_print_operand_address (FILE *file, machine_mode /* mode  */, rtx x)
{
  struct loongarch_address_info addr;

  if (loongarch_classify_address (&addr, x, word_mode, true))
    switch (addr.type)
      {
      case ADDRESS_REG:
	fprintf (file, "%s,", reg_names[REGNO (addr.reg)]);
	loongarch_print_operand (file, addr.offset, 0);
	return;

      case ADDRESS_REG_REG:
	fprintf (file, "%s,%s", reg_names[REGNO (addr.reg)],
				reg_names[REGNO (addr.offset)]);
	return;

      case ADDRESS_LO_SUM:
	fprintf (file, "%s,", reg_names[REGNO (addr.reg)]);
	loongarch_print_operand_reloc (file, addr.offset, false /* hi64_part */,
				       false /* hi_reloc */);
	return;

      case ADDRESS_CONST_INT:
	fprintf (file, "%s,", reg_names[GP_REG_FIRST]);
	output_addr_const (file, x);
	return;

      case ADDRESS_SYMBOLIC:
	output_addr_const (file, loongarch_strip_unspec_address (x));
	return;
      }
  if (CONST_INT_P (x))
    output_addr_const (file, x);
  else
    gcc_unreachable ();
}

/* Implement TARGET_ASM_SELECT_RTX_SECTION.  */

static section *
loongarch_select_rtx_section (machine_mode mode, rtx x,
			      unsigned HOST_WIDE_INT align)
{
  /* ??? Consider using mergeable small data sections.  */
  if (loongarch_rtx_constant_in_small_data_p (mode))
    return get_named_section (NULL, ".sdata", 0);

  return default_elf_select_rtx_section (mode, x, align);
}

/* Implement TARGET_ASM_FUNCTION_RODATA_SECTION.

   The complication here is that jump tables will use absolute addresses,
   and should therefore not be included in the read-only part of a DSO.
   Handle such cases by selecting a normal data section instead of a
   read-only one.  The logic apes that in default_function_rodata_section.  */

static section *
loongarch_function_rodata_section (tree decl, bool)
{
  return default_function_rodata_section (decl, false);
}

/* Implement TARGET_IN_SMALL_DATA_P.  */

static bool
loongarch_in_small_data_p (const_tree decl)
{
  int size;

  if (TREE_CODE (decl) == STRING_CST || TREE_CODE (decl) == FUNCTION_DECL)
    return false;

  if (VAR_P (decl) && DECL_SECTION_NAME (decl) != 0)
    {
      const char *name;

      /* Reject anything that isn't in a known small-data section.  */
      name = DECL_SECTION_NAME (decl);
      if (strcmp (name, ".sdata") != 0 && strcmp (name, ".sbss") != 0)
	return false;

      /* If a symbol is defined externally, the assembler will use the
	 usual -G rules when deciding how to implement macros.  */
      if (!DECL_EXTERNAL (decl))
	return true;
    }

  /* We have traditionally not treated zero-sized objects as small data,
     so this is now effectively part of the ABI.  */
  size = int_size_in_bytes (TREE_TYPE (decl));
  return size > 0 && size <= g_switch_value;
}

/* The LoongArch debug format wants all automatic variables and arguments
   to be in terms of the virtual frame pointer (stack pointer before
   any adjustment in the function), while the LoongArch linker wants
   the frame pointer to be the stack pointer after the initial
   adjustment.  So, we do the adjustment here.  The arg pointer (which
   is eliminated) points to the virtual frame pointer, while the frame
   pointer (which may be eliminated) points to the stack pointer after
   the initial adjustments.  */

HOST_WIDE_INT
loongarch_debugger_offset (rtx addr, HOST_WIDE_INT offset)
{
  rtx offset2 = const0_rtx;
  rtx reg = eliminate_constant_term (addr, &offset2);

  if (offset == 0)
    offset = INTVAL (offset2);

  if (reg == stack_pointer_rtx
      || reg == frame_pointer_rtx
      || reg == hard_frame_pointer_rtx)
    {
      offset -= cfun->machine->frame.total_size;
      if (reg == hard_frame_pointer_rtx)
	offset += cfun->machine->frame.hard_frame_pointer_offset;
    }

  return offset;
}

/* Implement ASM_OUTPUT_EXTERNAL.  */

void
loongarch_output_external (FILE *file, tree decl, const char *name)
{
  default_elf_asm_output_external (file, decl, name);

  /* We output the name if and only if TREE_SYMBOL_REFERENCED is
     set in order to avoid putting out names that are never really
     used.  */
  if (TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl)))
    {
      if (loongarch_in_small_data_p (decl))
	{
	  /* When using assembler macros, emit .extern directives for
	     all small-data externs so that the assembler knows how
	     big they are.

	     In most cases it would be safe (though pointless) to emit
	     .externs for other symbols too.  One exception is when an
	     object is within the -G limit but declared by the user to
	     be in a section other than .sbss or .sdata.  */
	  fputs ("\t.extern\t", file);
	  assemble_name (file, name);
	  fprintf (file, ", " HOST_WIDE_INT_PRINT_DEC "\n",
		   int_size_in_bytes (TREE_TYPE (decl)));
	}
    }
}

/* Implement TARGET_ASM_OUTPUT_DWARF_DTPREL.  */

static void ATTRIBUTE_UNUSED
loongarch_output_dwarf_dtprel (FILE *file, int size, rtx x)
{
  switch (size)
    {
    case 4:
      fputs ("\t.dtprelword\t", file);
      break;

    case 8:
      fputs ("\t.dtpreldword\t", file);
      break;

    default:
      gcc_unreachable ();
    }
  output_addr_const (file, x);
  fputs ("+0x8000", file);
}

/* Implement ASM_OUTPUT_ASCII.  */

void
loongarch_output_ascii (FILE *stream, const char *string, size_t len)
{
  size_t i;
  int cur_pos;

  cur_pos = 17;
  fprintf (stream, "\t.ascii\t\"");
  for (i = 0; i < len; i++)
    {
      int c;

      c = (unsigned char) string[i];
      if (ISPRINT (c))
	{
	  if (c == '\\' || c == '\"')
	    {
	      putc ('\\', stream);
	      cur_pos++;
	    }
	  putc (c, stream);
	  cur_pos++;
	}
      else
	{
	  fprintf (stream, "\\%03o", c);
	  cur_pos += 4;
	}

      if (cur_pos > 72 && i + 1 < len)
	{
	  cur_pos = 17;
	  fprintf (stream, "\"\n\t.ascii\t\"");
	}
    }
  fprintf (stream, "\"\n");
}

/* Implement TARGET_FRAME_POINTER_REQUIRED.  */

static bool
loongarch_frame_pointer_required (void)
{
  /* If the function contains dynamic stack allocations, we need to
     use the frame pointer to access the static parts of the frame.  */
  if (cfun->calls_alloca)
    return true;

  return false;
}

/* Implement TARGET_CAN_ELIMINATE.  Make sure that we're not trying
   to eliminate to the wrong hard frame pointer.  */

static bool
loongarch_can_eliminate (const int from ATTRIBUTE_UNUSED, const int to)
{
  return (to == HARD_FRAME_POINTER_REGNUM || to == STACK_POINTER_REGNUM);
}

/* Implement RETURN_ADDR_RTX.  We do not support moving back to a
   previous frame.  */

rtx
loongarch_return_addr (int count, rtx frame ATTRIBUTE_UNUSED)
{
  if (count != 0)
    return const0_rtx;

  return get_hard_reg_initial_val (Pmode, RETURN_ADDR_REGNUM);
}

/* Emit code to change the current function's return address to
   ADDRESS.  SCRATCH is available as a scratch register, if needed.
   ADDRESS and SCRATCH are both word-mode GPRs.  */

void
loongarch_set_return_address (rtx address, rtx scratch)
{
  rtx slot_address;

  gcc_assert (BITSET_P (cfun->machine->frame.mask, RETURN_ADDR_REGNUM));

  if (frame_pointer_needed)
    slot_address = loongarch_add_offset (scratch, hard_frame_pointer_rtx,
					 -UNITS_PER_WORD);
  else
    slot_address = loongarch_add_offset (scratch, stack_pointer_rtx,
					 cfun->machine->frame.gp_sp_offset);

  loongarch_emit_move (gen_frame_mem (GET_MODE (address), slot_address),
		       address);
}

/* Return true if register REGNO can store a value of mode MODE.
   The result of this function is cached in loongarch_hard_regno_mode_ok.  */

static bool
loongarch_hard_regno_mode_ok_uncached (unsigned int regno, machine_mode mode)
{
  unsigned int size;
  enum mode_class mclass;

  if (mode == FCCmode)
    return FCC_REG_P (regno);

  size = GET_MODE_SIZE (mode);
  mclass = GET_MODE_CLASS (mode);

  if (GP_REG_P (regno))
    return ((regno - GP_REG_FIRST) & 1) == 0 || size <= UNITS_PER_WORD;

  if (FP_REG_P (regno))
    {
      if (mclass == MODE_FLOAT
	  || mclass == MODE_COMPLEX_FLOAT
	  || mclass == MODE_VECTOR_FLOAT)
	return size <= UNITS_PER_FPVALUE;

      /* Allow integer modes that fit into a single register.  We need
	 to put integers into FPRs when using instructions like CVT
	 and TRUNC.  There's no point allowing sizes smaller than a word,
	 because the FPU has no appropriate load/store instructions.  */
      if (mclass == MODE_INT)
	return size >= MIN_UNITS_PER_WORD && size <= UNITS_PER_FPREG;
    }

  return false;
}

/* Implement TARGET_HARD_REGNO_MODE_OK.  */

static bool
loongarch_hard_regno_mode_ok (unsigned int regno, machine_mode mode)
{
  return loongarch_hard_regno_mode_ok_p[mode][regno];
}

/* Implement TARGET_HARD_REGNO_NREGS.  */

static unsigned int
loongarch_hard_regno_nregs (unsigned int regno, machine_mode mode)
{
  if (FCC_REG_P (regno))
    /* The size of FP status registers is always 4, because they only hold
       FCCmode values, and FCCmode is always considered to be 4 bytes wide.  */
    return (GET_MODE_SIZE (mode) + 3) / 4;

  if (FP_REG_P (regno))
    return (GET_MODE_SIZE (mode) + UNITS_PER_FPREG - 1) / UNITS_PER_FPREG;

  /* All other registers are word-sized.  */
  return (GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
}

/* Implement CLASS_MAX_NREGS, taking the maximum of the cases
   in loongarch_hard_regno_nregs.  */

int
loongarch_class_max_nregs (enum reg_class rclass, machine_mode mode)
{
  int size;
  HARD_REG_SET left;

  size = 0x8000;
  left = reg_class_contents[rclass];
  if (hard_reg_set_intersect_p (left, reg_class_contents[(int) FCC_REGS]))
    {
      if (loongarch_hard_regno_mode_ok (FCC_REG_FIRST, mode))
	size = MIN (size, 4);

      left &= ~reg_class_contents[FCC_REGS];
    }
  if (hard_reg_set_intersect_p (left, reg_class_contents[(int) FP_REGS]))
    {
      if (loongarch_hard_regno_mode_ok (FP_REG_FIRST, mode))
	size = MIN (size, UNITS_PER_FPREG);

      left &= ~reg_class_contents[FP_REGS];
    }
  if (!hard_reg_set_empty_p (left))
    size = MIN (size, UNITS_PER_WORD);
  return (GET_MODE_SIZE (mode) + size - 1) / size;
}

/* Implement TARGET_CAN_CHANGE_MODE_CLASS.  */

static bool
loongarch_can_change_mode_class (machine_mode, machine_mode,
				 reg_class_t rclass)
{
  return !reg_classes_intersect_p (FP_REGS, rclass);
}

/* Return true if moves in mode MODE can use the FPU's fmov.fmt instruction,
*/

static bool
loongarch_mode_ok_for_mov_fmt_p (machine_mode mode)
{
  switch (mode)
    {
    case E_FCCmode:
    case E_SFmode:
      return TARGET_HARD_FLOAT;

    case E_DFmode:
      return TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT;

    default:
      return 0;
    }
}

/* Implement TARGET_MODES_TIEABLE_P.  */

static bool
loongarch_modes_tieable_p (machine_mode mode1, machine_mode mode2)
{
  /* FPRs allow no mode punning, so it's not worth tying modes if we'd
     prefer to put one of them in FPRs.  */
  return (mode1 == mode2
	  || (!loongarch_mode_ok_for_mov_fmt_p (mode1)
	      && !loongarch_mode_ok_for_mov_fmt_p (mode2)));
}

/* Implement TARGET_PREFERRED_RELOAD_CLASS.  */

static reg_class_t
loongarch_preferred_reload_class (rtx x, reg_class_t rclass)
{
  if (reg_class_subset_p (FP_REGS, rclass)
      && loongarch_mode_ok_for_mov_fmt_p (GET_MODE (x)))
    return FP_REGS;

  if (reg_class_subset_p (GR_REGS, rclass))
    rclass = GR_REGS;

  return rclass;
}

/* RCLASS is a class involved in a REGISTER_MOVE_COST calculation.
   Return a "canonical" class to represent it in later calculations.  */

static reg_class_t
loongarch_canonicalize_move_class (reg_class_t rclass)
{
  if (reg_class_subset_p (rclass, GENERAL_REGS))
    rclass = GENERAL_REGS;

  return rclass;
}

/* Return the cost of moving a value from a register of class FROM to a GPR.
   Return 0 for classes that are unions of other classes handled by this
   function.  */

static int
loongarch_move_to_gpr_cost (reg_class_t from)
{
  switch (from)
    {
    case GENERAL_REGS:
      /* MOVE macro.  */
      return 2;

    case FP_REGS:
      /* MOVFR2GR, etc.  */
      return 4;

    default:
      return 0;
    }
}

/* Return the cost of moving a value from a GPR to a register of class TO.
   Return 0 for classes that are unions of other classes handled by this
   function.  */

static int
loongarch_move_from_gpr_cost (reg_class_t to)
{
  switch (to)
    {
    case GENERAL_REGS:
      /*MOVE macro.  */
      return 2;

    case FP_REGS:
      /* MOVGR2FR, etc.  */
      return 4;

    default:
      return 0;
    }
}

/* Implement TARGET_REGISTER_MOVE_COST.  Return 0 for classes that are the
   maximum of the move costs for subclasses; regclass will work out
   the maximum for us.  */

static int
loongarch_register_move_cost (machine_mode mode, reg_class_t from,
			      reg_class_t to)
{
  reg_class_t dregs;
  int cost1, cost2;

  from = loongarch_canonicalize_move_class (from);
  to = loongarch_canonicalize_move_class (to);

  /* Handle moves that can be done without using general-purpose registers.  */
  if (from == FP_REGS)
    {
      if (to == FP_REGS && loongarch_mode_ok_for_mov_fmt_p (mode))
	/* FMOV.FMT.  */
	return 4;
    }

  /* Handle cases in which only one class deviates from the ideal.  */
  dregs = GENERAL_REGS;
  if (from == dregs)
    return loongarch_move_from_gpr_cost (to);
  if (to == dregs)
    return loongarch_move_to_gpr_cost (from);

  /* Handles cases that require a GPR temporary.  */
  cost1 = loongarch_move_to_gpr_cost (from);
  if (cost1 != 0)
    {
      cost2 = loongarch_move_from_gpr_cost (to);
      if (cost2 != 0)
	return cost1 + cost2;
    }

  return 0;
}

/* Implement TARGET_MEMORY_MOVE_COST.  */

static int
loongarch_memory_move_cost (machine_mode mode, reg_class_t rclass, bool in)
{
  return (loongarch_cost->memory_latency
	  + memory_move_secondary_cost (mode, rclass, in));
}

/* Return the register class required for a secondary register when
   copying between one of the registers in RCLASS and value X, which
   has mode MODE.  X is the source of the move if IN_P, otherwise it
   is the destination.  Return NO_REGS if no secondary register is
   needed.  */

static reg_class_t
loongarch_secondary_reload (bool in_p ATTRIBUTE_UNUSED, rtx x,
			    reg_class_t rclass, machine_mode mode,
			    secondary_reload_info *sri ATTRIBUTE_UNUSED)
{
  int regno;

  regno = true_regnum (x);

  if (reg_class_subset_p (rclass, FP_REGS))
    {
      if (regno < 0
	  || (MEM_P (x)
	      && (GET_MODE_SIZE (mode) == 4 || GET_MODE_SIZE (mode) == 8)))
	/* In this case we can use fld.s, fst.s, fld.d or fst.d.  */
	return NO_REGS;

      if (GP_REG_P (regno) || x == CONST0_RTX (mode))
	/* In this case we can use movgr2fr.s, movfr2gr.s, movgr2fr.d or
	 * movfr2gr.d.  */
	return NO_REGS;

      if (CONSTANT_P (x) && !targetm.cannot_force_const_mem (mode, x))
	/* We can force the constant to memory and use fld.s
	   and fld.d.  As above, we will use pairs of lwc1s if
	   ldc1 is not supported.  */
	return NO_REGS;

      if (FP_REG_P (regno) && loongarch_mode_ok_for_mov_fmt_p (mode))
	/* In this case we can use fmov.{s/d}.  */
	return NO_REGS;

      /* Otherwise, we need to reload through an integer register.  */
      return GR_REGS;
    }
  if (FP_REG_P (regno))
    return reg_class_subset_p (rclass, GR_REGS) ? NO_REGS : GR_REGS;

  return NO_REGS;
}

/* Implement TARGET_VALID_POINTER_MODE.  */

static bool
loongarch_valid_pointer_mode (scalar_int_mode mode)
{
  return mode == SImode || (TARGET_64BIT && mode == DImode);
}

/* Implement TARGET_SCALAR_MODE_SUPPORTED_P.  */

static bool
loongarch_scalar_mode_supported_p (scalar_mode mode)
{
  if (ALL_FIXED_POINT_MODE_P (mode)
      && GET_MODE_PRECISION (mode) <= 2 * BITS_PER_WORD)
    return true;

  return default_scalar_mode_supported_p (mode);
}

/* Return the assembly code for INSN, which has the operands given by
   OPERANDS, and which branches to OPERANDS[0] if some condition is true.
   BRANCH_IF_TRUE is the asm template that should be used if OPERANDS[0]
   is in range of a direct branch.  BRANCH_IF_FALSE is an inverted
   version of BRANCH_IF_TRUE.  */

const char *
loongarch_output_conditional_branch (rtx_insn *insn, rtx *operands,
				     const char *branch_if_true,
				     const char *branch_if_false)
{
  unsigned int length;
  rtx taken;

  gcc_assert (LABEL_P (operands[0]));

  length = get_attr_length (insn);
  if (length <= 4)
    {
      return branch_if_true;
    }

  /* Generate a reversed branch around a direct jump.  */
  rtx_code_label *not_taken = gen_label_rtx ();
  taken = operands[0];

  /* Generate the reversed branch to NOT_TAKEN.  */
  operands[0] = not_taken;
  output_asm_insn (branch_if_false, operands);

  output_asm_insn ("b\t%0", &taken);

  /* Output NOT_TAKEN.  */
  targetm.asm_out.internal_label (asm_out_file, "L",
				  CODE_LABEL_NUMBER (not_taken));
  return "";
}

/* Return the assembly code for INSN, which branches to OPERANDS[0]
   if some equality condition is true.  The condition is given by
   OPERANDS[1] if !INVERTED_P, otherwise it is the inverse of
   OPERANDS[1].  OPERANDS[2] is the comparison's first operand;
   OPERANDS[3] is the second operand and may be zero or a register.  */

const char *
loongarch_output_equal_conditional_branch (rtx_insn *insn, rtx *operands,
					   bool inverted_p)
{
  const char *branch[2];
  if (operands[3] == const0_rtx)
    {
      branch[!inverted_p] = LARCH_BRANCH ("b%C1z", "%2,%0");
      branch[inverted_p] = LARCH_BRANCH ("b%N1z", "%2,%0");
    }
  else
    {
      branch[!inverted_p] = LARCH_BRANCH ("b%C1", "%2,%z3,%0");
      branch[inverted_p] = LARCH_BRANCH ("b%N1", "%2,%z3,%0");
    }

  return loongarch_output_conditional_branch (insn, operands, branch[1],
					      branch[0]);
}

/* Return the assembly code for INSN, which branches to OPERANDS[0]
   if some ordering condition is true.  The condition is given by
   OPERANDS[1] if !INVERTED_P, otherwise it is the inverse of
   OPERANDS[1].  OPERANDS[2] is the comparison's first operand;
   OPERANDS[3] is the second operand and may be zero or a register.  */

const char *
loongarch_output_order_conditional_branch (rtx_insn *insn, rtx *operands,
					   bool inverted_p)
{
  const char *branch[2];

  /* Make BRANCH[1] branch to OPERANDS[0] when the condition is true.
     Make BRANCH[0] branch on the inverse condition.  */
  if (operands[3] != const0_rtx)
    {
      /* Handle degenerate cases that should not, but do, occur.  */
      if (REGNO (operands[2]) == REGNO (operands[3]))
	{
	  switch (GET_CODE (operands[1]))
	    {
	    case LT:
	    case LTU:
	    case GT:
	    case GTU:
	      inverted_p = !inverted_p;
	      /* Fall through.  */
	    case LE:
	    case LEU:
	    case GE:
	    case GEU:
	      branch[!inverted_p] = LARCH_BRANCH ("b", "%0");
	      branch[inverted_p] = "\t# branch never";
	      break;
	    default:
	      gcc_unreachable ();
	    }
	}
      else
	{
	  switch (GET_CODE (operands[1]))
	    {
	    case LE:
	    case LEU:
	    case GT:
	    case GTU:
	    case LT:
	    case LTU:
	    case GE:
	    case GEU:
	      branch[!inverted_p] = LARCH_BRANCH ("b%C1", "%2,%3,%0");
	      branch[inverted_p] = LARCH_BRANCH ("b%N1", "%2,%3,%0");
	      break;
	    default:
	      gcc_unreachable ();
	    }
	}
    }
  else
    {
      switch (GET_CODE (operands[1]))
	{
	  /* These cases are equivalent to comparisons against zero.  */
	case LEU:
	case GTU:
	case LTU:
	case GEU:
	case LE:
	case GT:
	case LT:
	case GE:
	  branch[!inverted_p] = LARCH_BRANCH ("b%C1", "%2,$r0,%0");
	  branch[inverted_p] = LARCH_BRANCH ("b%N1", "%2,$r0,%0");
	  break;
	default:
	  gcc_unreachable ();
	}
    }
  return loongarch_output_conditional_branch (insn, operands, branch[1],
					      branch[0]);
}

/* Return the assembly code for DIV.{W/D} instruction DIVISION, which has
   the operands given by OPERANDS.  Add in a divide-by-zero check if needed.
   */

const char *
loongarch_output_division (const char *division, rtx *operands)
{
  const char *s;

  s = division;
  if (loongarch_check_zero_div_p ())
    {
      output_asm_insn (s, operands);
      s = "bne\t%2,%.,1f\n\tbreak\t7\n1:";
    }
  return s;
}

/* Implement TARGET_SCHED_ADJUST_COST.  We assume that anti and output
   dependencies have no cost.  */

static int
loongarch_adjust_cost (rtx_insn *, int dep_type, rtx_insn *, int cost,
		       unsigned int)
{
  if (dep_type != 0 && (dep_type != REG_DEP_OUTPUT))
    return 0;
  return cost;
}

/* Return the number of instructions that can be issued per cycle.  */

static int
loongarch_issue_rate (void)
{
  if ((unsigned long) LARCH_ACTUAL_TUNE < N_TUNE_TYPES)
    return loongarch_cpu_issue_rate[LARCH_ACTUAL_TUNE];
  else
    return 1;
}

/* Implement TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD.  This should
   be as wide as the scheduling freedom in the DFA.  */

static int
loongarch_multipass_dfa_lookahead (void)
{
  if ((unsigned long) LARCH_ACTUAL_TUNE < N_ARCH_TYPES)
    return loongarch_cpu_multipass_dfa_lookahead[LARCH_ACTUAL_TUNE];
  else
    return 0;
}

/* Implement TARGET_SCHED_REORDER.  */

static int
loongarch_sched_reorder (FILE *file ATTRIBUTE_UNUSED,
			 int verbose ATTRIBUTE_UNUSED,
			 rtx_insn **ready ATTRIBUTE_UNUSED,
			 int *nreadyp ATTRIBUTE_UNUSED,
			 int cycle ATTRIBUTE_UNUSED)
{
  return loongarch_issue_rate ();
}

/* Implement TARGET_SCHED_REORDER2.  */

static int
loongarch_sched_reorder2 (FILE *file ATTRIBUTE_UNUSED,
			  int verbose ATTRIBUTE_UNUSED,
			  rtx_insn **ready ATTRIBUTE_UNUSED,
			  int *nreadyp ATTRIBUTE_UNUSED,
			  int cycle ATTRIBUTE_UNUSED)
{
  return cached_can_issue_more;
}

/* Implement TARGET_SCHED_INIT.  */

static void
loongarch_sched_init (FILE *file ATTRIBUTE_UNUSED,
		      int verbose ATTRIBUTE_UNUSED,
		      int max_ready ATTRIBUTE_UNUSED)
{}

/* Implement TARGET_SCHED_VARIABLE_ISSUE.  */

static int
loongarch_variable_issue (FILE *file ATTRIBUTE_UNUSED,
			  int verbose ATTRIBUTE_UNUSED, rtx_insn *insn,
			  int more)
{
  /* Ignore USEs and CLOBBERs; don't count them against the issue rate.  */
  if (USEFUL_INSN_P (insn))
    {
      if (get_attr_type (insn) != TYPE_GHOST)
	more--;
    }

  /* Instructions of type 'multi' should all be split before
     the second scheduling pass.  */
  gcc_assert (!reload_completed
	      || recog_memoized (insn) < 0
	      || get_attr_type (insn) != TYPE_MULTI);

  cached_can_issue_more = more;
  return more;
}

/* Given that we have an rtx of the form (prefetch ... WRITE LOCALITY),
   return the first operand of the associated PREF or PREFX insn.  */

rtx
loongarch_prefetch_cookie (rtx write, rtx locality)
{
  /* store_streamed / load_streamed.  */
  if (INTVAL (locality) <= 0)
    return GEN_INT (INTVAL (write) + 4);

  /* store / load.  */
  if (INTVAL (locality) <= 2)
    return write;

  /* store_retained / load_retained.  */
  return GEN_INT (INTVAL (write) + 6);
}

/* Implement TARGET_ASM_OUTPUT_MI_THUNK.  Generate rtl rather than asm text
   in order to avoid duplicating too much logic from elsewhere.  */

static void
loongarch_output_mi_thunk (FILE *file, tree thunk_fndecl ATTRIBUTE_UNUSED,
			   HOST_WIDE_INT delta, HOST_WIDE_INT vcall_offset,
			   tree function)
{
  const char *fnname = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (thunk_fndecl));
  rtx this_rtx, temp1, temp2, fnaddr;
  rtx_insn *insn;
  bool use_sibcall_p;

  /* Pretend to be a post-reload pass while generating rtl.  */
  reload_completed = 1;

  /* Mark the end of the (empty) prologue.  */
  emit_note (NOTE_INSN_PROLOGUE_END);

  /* Determine if we can use a sibcall to call FUNCTION directly.  */
  fnaddr = XEXP (DECL_RTL (function), 0);
  use_sibcall_p = const_call_insn_operand (fnaddr, Pmode);

  /* We need two temporary registers in some cases.  */
  temp1 = gen_rtx_REG (Pmode, 12);
  temp2 = gen_rtx_REG (Pmode, 13);

  /* Find out which register contains the "this" pointer.  */
  if (aggregate_value_p (TREE_TYPE (TREE_TYPE (function)), function))
    this_rtx = gen_rtx_REG (Pmode, GP_ARG_FIRST + 1);
  else
    this_rtx = gen_rtx_REG (Pmode, GP_ARG_FIRST);

  /* Add DELTA to THIS_RTX.  */
  if (delta != 0)
    {
      rtx offset = GEN_INT (delta);
      if (!IMM12_OPERAND (delta))
	{
	  loongarch_emit_move (temp1, offset);
	  offset = temp1;
	}
      emit_insn (gen_add3_insn (this_rtx, this_rtx, offset));
    }

  /* If needed, add *(*THIS_RTX + VCALL_OFFSET) to THIS_RTX.  */
  if (vcall_offset != 0)
    {
      rtx addr;

      /* Set TEMP1 to *THIS_RTX.  */
      loongarch_emit_move (temp1, gen_rtx_MEM (Pmode, this_rtx));

      /* Set ADDR to a legitimate address for *THIS_RTX + VCALL_OFFSET.  */
      addr = loongarch_add_offset (temp2, temp1, vcall_offset);

      /* Load the offset and add it to THIS_RTX.  */
      loongarch_emit_move (temp1, gen_rtx_MEM (Pmode, addr));
      emit_insn (gen_add3_insn (this_rtx, this_rtx, temp1));
    }

  /* Jump to the target function.  Use a sibcall if direct jumps are
     allowed, otherwise load the address into a register first.  */
  if (use_sibcall_p)
    {
      insn = emit_call_insn (gen_sibcall_internal (fnaddr, const0_rtx));
      SIBLING_CALL_P (insn) = 1;
    }
  else
    {
      loongarch_emit_move (temp1, fnaddr);
      emit_jump_insn (gen_indirect_jump (temp1));
    }

  /* Run just enough of rest_of_compilation.  This sequence was
     "borrowed" from alpha.c.  */
  insn = get_insns ();
  split_all_insns_noflow ();
  shorten_branches (insn);
  assemble_start_function (thunk_fndecl, fnname);
  final_start_function (insn, file, 1);
  final (insn, file, 1);
  final_end_function ();
  assemble_end_function (thunk_fndecl, fnname);

  /* Stop pretending to be a post-reload pass.  */
  reload_completed = 0;
}

/* Allocate a chunk of memory for per-function machine-dependent data.  */

static struct machine_function *
loongarch_init_machine_status (void)
{
  return ggc_cleared_alloc<machine_function> ();
}

static void
loongarch_option_override_internal (struct gcc_options *opts)
{
  int i, regno, mode;

  if (flag_pic)
    g_switch_value = 0;

  /* Handle target-specific options: compute defaults/conflicts etc.  */
  loongarch_config_target (&la_target, la_opt_switches,
			   la_opt_cpu_arch, la_opt_cpu_tune, la_opt_fpu,
			   la_opt_abi_base, la_opt_abi_ext, la_opt_cmodel, 0);

  if (TARGET_ABI_LP64)
    flag_pcc_struct_return = 0;

  /* Decide which rtx_costs structure to use.  */
  if (optimize_size)
    loongarch_cost = &loongarch_rtx_cost_optimize_size;
  else
    loongarch_cost = &loongarch_cpu_rtx_cost_data[LARCH_ACTUAL_TUNE];

  /* If the user hasn't specified a branch cost, use the processor's
     default.  */
  if (loongarch_branch_cost == 0)
    loongarch_branch_cost = loongarch_cost->branch_cost;

  /* Set up parameters to be used in prefetching algorithm.  */
  int simultaneous_prefetches
    = loongarch_cpu_cache[LARCH_ACTUAL_TUNE].simultaneous_prefetches;

  SET_OPTION_IF_UNSET (opts, &global_options_set,
		       param_simultaneous_prefetches,
		       simultaneous_prefetches);

  SET_OPTION_IF_UNSET (opts, &global_options_set,
		       param_l1_cache_line_size,
		       loongarch_cpu_cache[LARCH_ACTUAL_TUNE].l1d_line_size);

  SET_OPTION_IF_UNSET (opts, &global_options_set,
		       param_l1_cache_size,
		       loongarch_cpu_cache[LARCH_ACTUAL_TUNE].l1d_size);

  SET_OPTION_IF_UNSET (opts, &global_options_set,
		       param_l2_cache_size,
		       loongarch_cpu_cache[LARCH_ACTUAL_TUNE].l2d_size);


  /* Enable sw prefetching at -O3 and higher.  */
  if (opts->x_flag_prefetch_loop_arrays < 0
      && (opts->x_optimize >= 3 || opts->x_flag_profile_use)
      && !opts->x_optimize_size)
    opts->x_flag_prefetch_loop_arrays = 1;

  if (TARGET_DIRECT_EXTERN_ACCESS && flag_shlib)
    error ("%qs cannot be used for compiling a shared library",
	   "-mdirect-extern-access");

  switch (la_target.cmodel)
    {
      case CMODEL_EXTREME:
	if (!TARGET_EXPLICIT_RELOCS)
	  error ("code model %qs needs %s",
		 "extreme", "-mexplicit-relocs");

	if (opts->x_flag_plt)
	  {
	    if (global_options_set.x_flag_plt)
	      error ("code model %qs is not compatible with %s",
		     "extreme", "-fplt");
	    opts->x_flag_plt = 0;
	  }
	break;

      case CMODEL_TINY_STATIC:
      case CMODEL_MEDIUM:
      case CMODEL_NORMAL:
      case CMODEL_TINY:
      case CMODEL_LARGE:
	break;

      default:
	gcc_unreachable ();
    }

  /* Validate the guard size.  */
  int guard_size = param_stack_clash_protection_guard_size;

  /* Enforce that interval is the same size as size so the mid-end does the
     right thing.  */
  SET_OPTION_IF_UNSET (opts, &global_options_set,
		       param_stack_clash_protection_probe_interval,
		       guard_size);

  loongarch_init_print_operand_punct ();

  /* Set up array to map GCC register number to debug register number.
     Ignore the special purpose register numbers.  */

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (GP_REG_P (i) || FP_REG_P (i))
	loongarch_dwarf_regno[i] = i;
      else
	loongarch_dwarf_regno[i] = INVALID_REGNUM;
    }

  /* Set up loongarch_hard_regno_mode_ok.  */
  for (mode = 0; mode < MAX_MACHINE_MODE; mode++)
    for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
      loongarch_hard_regno_mode_ok_p[mode][regno]
	= loongarch_hard_regno_mode_ok_uncached (regno, (machine_mode) mode);

  /* Function to allocate machine-dependent function status.  */
  init_machine_status = &loongarch_init_machine_status;
}


/* Implement TARGET_OPTION_OVERRIDE.  */

static void
loongarch_option_override (void)
{
  loongarch_option_override_internal (&global_options);
}

/* Implement TARGET_CONDITIONAL_REGISTER_USAGE.  */

static void
loongarch_conditional_register_usage (void)
{
  if (!TARGET_HARD_FLOAT)
    accessible_reg_set &= ~(reg_class_contents[FP_REGS]
			    | reg_class_contents[FCC_REGS]);
}

/* Implement EH_USES.  */

bool
loongarch_eh_uses (unsigned int regno ATTRIBUTE_UNUSED)
{
  return false;
}

/* Implement EPILOGUE_USES.  */

bool
loongarch_epilogue_uses (unsigned int regno)
{
  /* Say that the epilogue uses the return address register.  Note that
     in the case of sibcalls, the values "used by the epilogue" are
     considered live at the start of the called function.  */
  if (regno == RETURN_ADDR_REGNUM)
    return true;

  return false;
}

bool
loongarch_load_store_bonding_p (rtx *operands, machine_mode mode, bool load_p)
{
  rtx reg1, reg2, mem1, mem2, base1, base2;
  enum reg_class rc1, rc2;
  HOST_WIDE_INT offset1, offset2;

  if (load_p)
    {
      reg1 = operands[0];
      reg2 = operands[2];
      mem1 = operands[1];
      mem2 = operands[3];
    }
  else
    {
      reg1 = operands[1];
      reg2 = operands[3];
      mem1 = operands[0];
      mem2 = operands[2];
    }

  if (loongarch_address_insns (XEXP (mem1, 0), mode, false) == 0
      || loongarch_address_insns (XEXP (mem2, 0), mode, false) == 0)
    return false;

  loongarch_split_plus (XEXP (mem1, 0), &base1, &offset1);
  loongarch_split_plus (XEXP (mem2, 0), &base2, &offset2);

  /* Base regs do not match.  */
  if (!REG_P (base1) || !rtx_equal_p (base1, base2))
    return false;

  /* Either of the loads is clobbering base register.  It is legitimate to bond
     loads if second load clobbers base register.  However, hardware does not
     support such bonding.  */
  if (load_p
      && (REGNO (reg1) == REGNO (base1) || (REGNO (reg2) == REGNO (base1))))
    return false;

  /* Loading in same registers.  */
  if (load_p && REGNO (reg1) == REGNO (reg2))
    return false;

  /* The loads/stores are not of same type.  */
  rc1 = REGNO_REG_CLASS (REGNO (reg1));
  rc2 = REGNO_REG_CLASS (REGNO (reg2));
  if (rc1 != rc2 && !reg_class_subset_p (rc1, rc2)
      && !reg_class_subset_p (rc2, rc1))
    return false;

  if (abs (offset1 - offset2) != GET_MODE_SIZE (mode))
    return false;

  return true;
}

/* Implement TARGET_TRAMPOLINE_INIT.  */

static void
loongarch_trampoline_init (rtx m_tramp, tree fndecl, rtx chain_value)
{
  rtx addr, end_addr, mem;
  rtx trampoline[8];
  unsigned int i, j;
  HOST_WIDE_INT end_addr_offset, static_chain_offset, target_function_offset;

  /* Work out the offsets of the pointers from the start of the
     trampoline code.  */
  end_addr_offset = TRAMPOLINE_CODE_SIZE;
  static_chain_offset = end_addr_offset;
  target_function_offset = static_chain_offset + GET_MODE_SIZE (ptr_mode);

  /* Get pointers to the beginning and end of the code block.  */
  addr = force_reg (Pmode, XEXP (m_tramp, 0));
  end_addr
    = loongarch_force_binary (Pmode, PLUS, addr, GEN_INT (end_addr_offset));

#define OP(X) gen_int_mode (X, SImode)

  /* Build up the code in TRAMPOLINE.  */
  i = 0;
  /*pcaddi $static_chain,0
    ld.[dw] $tmp,$static_chain,target_function_offset
    ld.[dw] $static_chain,$static_chain,static_chain_offset
    jirl $r0,$tmp,0  */
  trampoline[i++] = OP (0x18000000 | (STATIC_CHAIN_REGNUM - GP_REG_FIRST));
  trampoline[i++] = OP ((ptr_mode == DImode ? 0x28c00000 : 0x28800000)
			| 19 /* $t7  */
			| ((STATIC_CHAIN_REGNUM - GP_REG_FIRST) << 5)
			| ((target_function_offset & 0xfff) << 10));
  trampoline[i++] = OP ((ptr_mode == DImode ? 0x28c00000 : 0x28800000)
			| (STATIC_CHAIN_REGNUM - GP_REG_FIRST)
			| ((STATIC_CHAIN_REGNUM - GP_REG_FIRST) << 5)
			| ((static_chain_offset & 0xfff) << 10));
  trampoline[i++] = OP (0x4c000000 | (19 << 5));
#undef OP

  for (j = 0; j < i; j++)
   {
     mem = adjust_address (m_tramp, SImode, j * GET_MODE_SIZE (SImode));
     loongarch_emit_move (mem, trampoline[j]);
   }

  /* Set up the static chain pointer field.  */
  mem = adjust_address (m_tramp, ptr_mode, static_chain_offset);
  loongarch_emit_move (mem, chain_value);

  /* Set up the target function field.  */
  mem = adjust_address (m_tramp, ptr_mode, target_function_offset);
  loongarch_emit_move (mem, XEXP (DECL_RTL (fndecl), 0));

  /* Flush the code part of the trampoline.  */
  emit_insn (gen_add3_insn (end_addr, addr, GEN_INT (TRAMPOLINE_SIZE)));
  emit_insn (gen_clear_cache (addr, end_addr));
}

/* Implement HARD_REGNO_CALLER_SAVE_MODE.  */

machine_mode
loongarch_hard_regno_caller_save_mode (unsigned int regno, unsigned int nregs,
				       machine_mode mode)
{
  /* For performance, avoid saving/restoring upper parts of a register
     by returning MODE as save mode when the mode is known.  */
  if (mode == VOIDmode)
    return choose_hard_reg_mode (regno, nregs, NULL);
  else
    return mode;
}

/* Implement TARGET_SPILL_CLASS.  */

static reg_class_t
loongarch_spill_class (reg_class_t rclass ATTRIBUTE_UNUSED,
		       machine_mode mode ATTRIBUTE_UNUSED)
{
  return NO_REGS;
}

/* Implement TARGET_PROMOTE_FUNCTION_MODE.  */

/* This function is equivalent to default_promote_function_mode_always_promote
   except that it returns a promoted mode even if type is NULL_TREE.  This is
   needed by libcalls which have no type (only a mode) such as fixed conversion
   routines that take a signed or unsigned char/short argument and convert it
   to a fixed type.  */

static machine_mode
loongarch_promote_function_mode (const_tree type ATTRIBUTE_UNUSED,
				 machine_mode mode,
				 int *punsignedp ATTRIBUTE_UNUSED,
				 const_tree fntype ATTRIBUTE_UNUSED,
				 int for_return ATTRIBUTE_UNUSED)
{
  int unsignedp;

  if (type != NULL_TREE)
    return promote_mode (type, mode, punsignedp);

  unsignedp = *punsignedp;
  PROMOTE_MODE (mode, unsignedp, type);
  *punsignedp = unsignedp;
  return mode;
}

/* Implement TARGET_STARTING_FRAME_OFFSET.  See loongarch_compute_frame_info
   for details about the frame layout.  */

static HOST_WIDE_INT
loongarch_starting_frame_offset (void)
{
  if (FRAME_GROWS_DOWNWARD)
    return 0;
  return crtl->outgoing_args_size;
}

static tree
loongarch_handle_model_attribute (tree *node, tree name, tree arg, int,
				  bool *no_add_attrs)
{
  tree decl = *node;
  if (TREE_CODE (decl) == VAR_DECL)
    {
      if (DECL_THREAD_LOCAL_P (decl))
	{
	  error_at (DECL_SOURCE_LOCATION (decl),
		    "%qE attribute cannot be specified for thread-local "
		    "variables", name);
	  *no_add_attrs = true;
	  return NULL_TREE;
	}
      if (DECL_CONTEXT (decl)
	  && TREE_CODE (DECL_CONTEXT (decl)) == FUNCTION_DECL
	  && !TREE_STATIC (decl))
	{
	  error_at (DECL_SOURCE_LOCATION (decl),
		    "%qE attribute cannot be specified for local "
		    "variables", name);
	  *no_add_attrs = true;
	  return NULL_TREE;
	}
      if (DECL_REGISTER (decl))
	{
	  error_at (DECL_SOURCE_LOCATION (decl),
		    "%qE attribute cannot be specified for register "
		    "variables", name);
	  *no_add_attrs = true;
	  return NULL_TREE;
	}
      if (!TARGET_EXPLICIT_RELOCS)
	{
	  error_at (DECL_SOURCE_LOCATION (decl),
		    "%qE attribute requires %s", name, "-mexplicit-relocs");
	  *no_add_attrs = true;
	  return NULL_TREE;
	}

      arg = TREE_VALUE (arg);
      if (TREE_CODE (arg) != STRING_CST)
	{
	  error_at (DECL_SOURCE_LOCATION (decl),
		    "invalid argument of %qE attribute", name);
	  *no_add_attrs = true;
	  return NULL_TREE;
	}

      const char *model = TREE_STRING_POINTER (arg);
      if (strcmp (model, "normal") != 0
	  && strcmp (model, "extreme") != 0)
	{
	  error_at (DECL_SOURCE_LOCATION (decl),
		    "invalid argument of %qE attribute", name);
	  *no_add_attrs = true;
	  return NULL_TREE;
	}

      if (lookup_attribute ("model", DECL_ATTRIBUTES (decl)))
	{
	  error_at (DECL_SOURCE_LOCATION (decl),
		    "multiple %qE attribute", name);
	  *no_add_attrs = true;
	  return NULL_TREE;
	}
    }
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }
  return NULL_TREE;
}

static const struct attribute_spec loongarch_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req,
       affects_type_identity, handler, exclude } */
  { "model", 1, 1, true, false, false, false,
    loongarch_handle_model_attribute, NULL },
  /* The last attribute spec is set to be NULL.  */
  {}
};

bool
loongarch_use_anchors_for_symbol_p (const_rtx symbol)
{
  tree decl = SYMBOL_REF_DECL (symbol);

  /* The section anchor optimization may break custom address model.  */
  if (decl && lookup_attribute ("model", DECL_ATTRIBUTES (decl)))
    return false;

  return default_use_anchors_for_symbol_p (symbol);
}

/* Implement the TARGET_ASAN_SHADOW_OFFSET hook.  */

static unsigned HOST_WIDE_INT
loongarch_asan_shadow_offset (void)
{
  /* We only have libsanitizer support for LOONGARCH64 at present.
     This value is taken from the file libsanitizer/asan/asan_mapping.h.  */
  return TARGET_64BIT ? (HOST_WIDE_INT_1 << 46) : 0;
}

/* Initialize the GCC target structure.  */
#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.half\t"
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.word\t"
#undef TARGET_ASM_ALIGNED_DI_OP
#define TARGET_ASM_ALIGNED_DI_OP "\t.dword\t"

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE loongarch_option_override

#undef TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS loongarch_legitimize_address

#undef TARGET_ASM_SELECT_RTX_SECTION
#define TARGET_ASM_SELECT_RTX_SECTION loongarch_select_rtx_section
#undef TARGET_ASM_FUNCTION_RODATA_SECTION
#define TARGET_ASM_FUNCTION_RODATA_SECTION loongarch_function_rodata_section

#undef TARGET_SCHED_INIT
#define TARGET_SCHED_INIT loongarch_sched_init
#undef TARGET_SCHED_REORDER
#define TARGET_SCHED_REORDER loongarch_sched_reorder
#undef TARGET_SCHED_REORDER2
#define TARGET_SCHED_REORDER2 loongarch_sched_reorder2
#undef TARGET_SCHED_VARIABLE_ISSUE
#define TARGET_SCHED_VARIABLE_ISSUE loongarch_variable_issue
#undef TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST loongarch_adjust_cost
#undef TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE loongarch_issue_rate
#undef TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD
#define TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD \
  loongarch_multipass_dfa_lookahead

#undef TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL loongarch_function_ok_for_sibcall

#undef TARGET_VALID_POINTER_MODE
#define TARGET_VALID_POINTER_MODE loongarch_valid_pointer_mode
#undef TARGET_REGISTER_MOVE_COST
#define TARGET_REGISTER_MOVE_COST loongarch_register_move_cost
#undef TARGET_MEMORY_MOVE_COST
#define TARGET_MEMORY_MOVE_COST loongarch_memory_move_cost
#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS loongarch_rtx_costs
#undef TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST loongarch_address_cost

#undef TARGET_IN_SMALL_DATA_P
#define TARGET_IN_SMALL_DATA_P loongarch_in_small_data_p

#undef TARGET_PREFERRED_RELOAD_CLASS
#define TARGET_PREFERRED_RELOAD_CLASS loongarch_preferred_reload_class

#undef TARGET_ASM_FILE_START_FILE_DIRECTIVE
#define TARGET_ASM_FILE_START_FILE_DIRECTIVE true

#undef TARGET_EXPAND_BUILTIN_VA_START
#define TARGET_EXPAND_BUILTIN_VA_START loongarch_va_start

#undef TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE loongarch_promote_function_mode
#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY loongarch_return_in_memory

#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE loongarch_function_value
#undef TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE loongarch_libcall_value

#undef TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK loongarch_output_mi_thunk
#undef TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK \
  hook_bool_const_tree_hwi_hwi_const_tree_true

#undef TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND loongarch_print_operand
#undef TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS loongarch_print_operand_address
#undef TARGET_PRINT_OPERAND_PUNCT_VALID_P
#define TARGET_PRINT_OPERAND_PUNCT_VALID_P \
  loongarch_print_operand_punct_valid_p

#undef TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS loongarch_setup_incoming_varargs
#undef TARGET_STRICT_ARGUMENT_NAMING
#define TARGET_STRICT_ARGUMENT_NAMING hook_bool_CUMULATIVE_ARGS_true
#undef TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK must_pass_in_stack_var_size
#undef TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE loongarch_pass_by_reference
#undef TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES loongarch_arg_partial_bytes
#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG loongarch_function_arg
#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE loongarch_function_arg_advance
#undef TARGET_FUNCTION_ARG_BOUNDARY
#define TARGET_FUNCTION_ARG_BOUNDARY loongarch_function_arg_boundary

#undef TARGET_SCALAR_MODE_SUPPORTED_P
#define TARGET_SCALAR_MODE_SUPPORTED_P loongarch_scalar_mode_supported_p

#undef TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS loongarch_init_builtins
#undef TARGET_BUILTIN_DECL
#define TARGET_BUILTIN_DECL loongarch_builtin_decl
#undef TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN loongarch_expand_builtin

/* The generic ELF target does not always have TLS support.  */
#ifdef HAVE_AS_TLS
#undef TARGET_HAVE_TLS
#define TARGET_HAVE_TLS HAVE_AS_TLS
#endif

#undef TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM loongarch_cannot_force_const_mem

#undef TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P loongarch_legitimate_constant_p

#undef TARGET_USE_BLOCKS_FOR_CONSTANT_P
#define TARGET_USE_BLOCKS_FOR_CONSTANT_P hook_bool_mode_const_rtx_true

#ifdef HAVE_AS_DTPRELWORD
#undef TARGET_ASM_OUTPUT_DWARF_DTPREL
#define TARGET_ASM_OUTPUT_DWARF_DTPREL loongarch_output_dwarf_dtprel
#endif

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P loongarch_legitimate_address_p

#undef TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED loongarch_frame_pointer_required

#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE loongarch_can_eliminate

#undef TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE loongarch_conditional_register_usage

#undef TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT loongarch_trampoline_init

#undef TARGET_MIN_ANCHOR_OFFSET
#define TARGET_MIN_ANCHOR_OFFSET (-IMM_REACH/2)

#undef TARGET_MAX_ANCHOR_OFFSET
#define TARGET_MAX_ANCHOR_OFFSET (IMM_REACH/2-1)

#undef TARGET_ATOMIC_ASSIGN_EXPAND_FENV
#define TARGET_ATOMIC_ASSIGN_EXPAND_FENV loongarch_atomic_assign_expand_fenv

#undef TARGET_CALL_FUSAGE_CONTAINS_NON_CALLEE_CLOBBERS
#define TARGET_CALL_FUSAGE_CONTAINS_NON_CALLEE_CLOBBERS true

#undef TARGET_SPILL_CLASS
#define TARGET_SPILL_CLASS loongarch_spill_class

#undef TARGET_HARD_REGNO_NREGS
#define TARGET_HARD_REGNO_NREGS loongarch_hard_regno_nregs
#undef TARGET_HARD_REGNO_MODE_OK
#define TARGET_HARD_REGNO_MODE_OK loongarch_hard_regno_mode_ok

#undef TARGET_MODES_TIEABLE_P
#define TARGET_MODES_TIEABLE_P loongarch_modes_tieable_p

#undef TARGET_CUSTOM_FUNCTION_DESCRIPTORS
#define TARGET_CUSTOM_FUNCTION_DESCRIPTORS 2

#undef TARGET_CAN_CHANGE_MODE_CLASS
#define TARGET_CAN_CHANGE_MODE_CLASS loongarch_can_change_mode_class

#undef TARGET_CONSTANT_ALIGNMENT
#define TARGET_CONSTANT_ALIGNMENT loongarch_constant_alignment

#undef TARGET_STARTING_FRAME_OFFSET
#define TARGET_STARTING_FRAME_OFFSET loongarch_starting_frame_offset

#undef TARGET_SECONDARY_RELOAD
#define TARGET_SECONDARY_RELOAD loongarch_secondary_reload

#undef  TARGET_HAVE_SPECULATION_SAFE_VALUE
#define TARGET_HAVE_SPECULATION_SAFE_VALUE speculation_safe_value_not_needed

#undef  TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE loongarch_attribute_table

#undef  TARGET_USE_ANCHORS_FOR_SYMBOL_P
#define TARGET_USE_ANCHORS_FOR_SYMBOL_P loongarch_use_anchors_for_symbol_p

#undef TARGET_ASAN_SHADOW_OFFSET
#define TARGET_ASAN_SHADOW_OFFSET loongarch_asan_shadow_offset

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-loongarch.h"
