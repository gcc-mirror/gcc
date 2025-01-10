/* Subroutines used for LoongArch code generation.
   Copyright (C) 2021-2025 Free Software Foundation, Inc.
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
#include "function-abi.h"
#include "cfgloop.h"
#include "tree-vectorizer.h"

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

   METHOD_MIRROR:
     Copy 0-31 bit of the immediate number to 32-63bit.
*/
enum loongarch_load_imm_method
{
  METHOD_NORMAL,
  METHOD_LU32I,
  METHOD_LU52I,
  METHOD_MIRROR
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

  /* For a C23 variadic function w/o any named argument, and w/o an
     artifical argument for large return value, skip advancing args.
     There is such an artifical argument iff. arg.type is non-NULL
     (PR 114175).  */
  if (!TYPE_NO_NAMED_ARGS_STDARG_P (TREE_TYPE (current_function_decl))
      || arg.type != NULL_TREE)
    loongarch_function_arg_advance (pack_cumulative_args (&local_cum), arg);

  /* Found out how many registers we need to save.  */
  gp_saved = cfun->va_list_gpr_size / UNITS_PER_WORD;
  if (gp_saved > (int) (MAX_ARGS_IN_REGISTERS - local_cum.num_gprs))
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
			      loongarch_save_restore_fn fn,
			      bool skip_eh_data_regs_p)
{
  HOST_WIDE_INT offset;

  /* Save the link register and s-registers.  */
  offset = cfun->machine->frame.gp_sp_offset - sp_offset;
  for (int regno = GP_REG_FIRST; regno <= GP_REG_LAST; regno++)
    if (BITSET_P (cfun->machine->frame.mask, regno - GP_REG_FIRST))
      {
	/* Special care needs to be taken for $r4-$r7 (EH_RETURN_DATA_REGNO)
	   when returning normally from a function that calls
	   __builtin_eh_return.  In this case, these registers are saved but
	   should not be restored, or the return value may be clobbered.  */

	if (!(cfun->machine->reg_is_wrapped_separately[regno]
	      || (skip_eh_data_regs_p
	      && GP_ARG_FIRST <= regno && regno < GP_ARG_FIRST + 4)))
	  loongarch_save_restore_reg (word_mode, regno, offset, fn);

	offset -= UNITS_PER_WORD;
      }

  /* This loop must iterate over the same space as its companion in
     loongarch_compute_frame_info.  */
  offset = cfun->machine->frame.fp_sp_offset - sp_offset;
  machine_mode mode = TARGET_DOUBLE_FLOAT ? DFmode : SFmode;

  for (int regno = FP_REG_FIRST; regno <= FP_REG_LAST; regno++)
    if (BITSET_P (cfun->machine->frame.fmask, regno - FP_REG_FIRST))
      {
	if (!cfun->machine->reg_is_wrapped_separately[regno])
	  loongarch_save_restore_reg (word_mode, regno, offset, fn);

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
  emit_insn (gen_stack_tie (Pmode, stack_pointer_rtx,
			    frame_pointer_needed ? hard_frame_pointer_rtx
			    : stack_pointer_rtx));
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
      loongarch_for_each_saved_reg (size, loongarch_save_reg, false);
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

/* Expand function epilogue using the following insn patterns:
   "epilogue"	      (style == NORMAL_RETURN)
   "sibcall_epilogue" (style == SIBCALL_RETURN)
   "eh_return"	      (style == EXCEPTION_RETURN) */

void
loongarch_expand_epilogue (int style)
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

  /* Handle simple returns.  */
  if (style == NORMAL_RETURN && loongarch_can_use_return_insn ())
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
				loongarch_restore_reg,
				crtl->calls_eh_return
				&& style != EXCEPTION_RETURN);

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
  if (crtl->calls_eh_return && style == EXCEPTION_RETURN)
    emit_insn (gen_add3_insn (stack_pointer_rtx, stack_pointer_rtx,
			      EH_RETURN_STACKADJ_RTX));

  /* Emit return unless doing sibcall.  */
  if (style != SIBCALL_RETURN)
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

      uint32_t hival = (uint32_t) (value >> 32);
      uint32_t loval = (uint32_t) value;

      /* Determine whether the upper 32 bits are sign-extended from the lower
	 32 bits. If it is, the instructions to load the high order can be
	 ommitted.  */
      if (lu32i[sign31] && lu52i[sign31])
	return cost;
      /* If the lower 32 bits are the same as the upper 32 bits, just copy
	 the lower 32 bits to the upper 32 bits.  */
      else if (loval == hival)
	{
	  codes[cost].method = METHOD_MIRROR;
	  codes[cost].curr_value = value;
	  return cost + 1;
	}
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

/* Return true if OP is a constant vector with the number of units in MODE,
   and each unit has the same bit set.  */

bool
loongarch_const_vector_bitimm_set_p (rtx op, machine_mode mode)
{
  if (GET_CODE (op) == CONST_VECTOR && op != CONST0_RTX (mode))
    {
      unsigned HOST_WIDE_INT val = UINTVAL (CONST_VECTOR_ELT (op, 0));
      int vlog2 = exact_log2 (val & GET_MODE_MASK (GET_MODE_INNER (mode)));

      if (vlog2 != -1)
	{
	  gcc_assert (GET_MODE_CLASS (mode) == MODE_VECTOR_INT);
	  gcc_assert (vlog2 >= 0 && vlog2 <= GET_MODE_UNIT_BITSIZE (mode) - 1);
	  return loongarch_const_vector_same_val_p (op, mode);
	}
    }

  return false;
}

/* Return true if OP is a constant vector with the number of units in MODE,
   and each unit has the same bit clear.  */

bool
loongarch_const_vector_bitimm_clr_p (rtx op, machine_mode mode)
{
  if (GET_CODE (op) == CONST_VECTOR && op != CONSTM1_RTX (mode))
    {
      unsigned HOST_WIDE_INT val = ~UINTVAL (CONST_VECTOR_ELT (op, 0));
      int vlog2 = exact_log2 (val & GET_MODE_MASK (GET_MODE_INNER (mode)));

      if (vlog2 != -1)
	{
	  gcc_assert (GET_MODE_CLASS (mode) == MODE_VECTOR_INT);
	  gcc_assert (vlog2 >= 0 && vlog2 <= GET_MODE_UNIT_BITSIZE (mode) - 1);
	  return loongarch_const_vector_same_val_p (op, mode);
	}
    }

  return false;
}

/* Return true if OP is a constant vector with the number of units in MODE,
   and each unit has the same value.  */

bool
loongarch_const_vector_same_val_p (rtx op, machine_mode mode)
{
  int i, nunits = GET_MODE_NUNITS (mode);
  rtx first;

  if (GET_CODE (op) != CONST_VECTOR || GET_MODE (op) != mode)
    return false;

  first = CONST_VECTOR_ELT (op, 0);
  for (i = 1; i < nunits; i++)
    if (!rtx_equal_p (first, CONST_VECTOR_ELT (op, i)))
      return false;

  return true;
}

/* Return true if OP is a constant vector with the number of units in MODE,
   and each unit has the same value as well as replicated bytes in the value.
*/

bool
loongarch_const_vector_same_bytes_p (rtx op, machine_mode mode)
{
  int i, bytes;
  HOST_WIDE_INT val, first_byte;
  rtx first;

  if (!loongarch_const_vector_same_val_p (op, mode))
    return false;

  first = CONST_VECTOR_ELT (op, 0);
  bytes = GET_MODE_UNIT_SIZE (mode);
  val = INTVAL (first);
  first_byte = val & 0xff;
  for (i = 1; i < bytes; i++)
    {
      val >>= 8;
      if ((val & 0xff) != first_byte)
	return false;
    }

  return true;
}

/* Return true if OP is a constant vector with the number of units in MODE,
   and each unit has the same integer value in the range [LOW, HIGH].  */

bool
loongarch_const_vector_same_int_p (rtx op, machine_mode mode, HOST_WIDE_INT low,
				   HOST_WIDE_INT high)
{
  HOST_WIDE_INT value;
  rtx elem0;

  if (!loongarch_const_vector_same_val_p (op, mode))
    return false;

  elem0 = CONST_VECTOR_ELT (op, 0);
  if (!CONST_INT_P (elem0))
    return false;

  value = INTVAL (elem0);
  return (value >= low && value <= high);
}

/* Return true if OP is a constant vector with repeated 4-element sets
   in mode MODE.  */

bool
loongarch_const_vector_shuffle_set_p (rtx op, machine_mode mode)
{
  int nunits = GET_MODE_NUNITS (mode);
  int nsets = nunits / 4;
  int set = 0;
  int i, j;

  /* Check if we have the same 4-element sets.  */
  for (j = 0; j < nsets; j++, set = 4 * j)
    for (i = 0; i < 4; i++)
      if ((INTVAL (XVECEXP (op, 0, i))
	   != (INTVAL (XVECEXP (op, 0, set + i)) - set))
	  || !IN_RANGE (INTVAL (XVECEXP (op, 0, set + i)), 0, set + 3))
	return false;
  return true;
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
    *symbol_type = loongarch_classify_symbol (x);
  else
    return false;

  if (offset == const0_rtx)
    return true;

  /* Check whether a nonzero offset is valid for the underlying
     relocations.  */
  switch (*symbol_type)
    {
    case SYMBOL_PCREL64:
      /* When the code model is extreme, the non-zero offset situation
	 has not been handled well, so it is disabled here now.  */
      if (!loongarch_explicit_relocs_p (SYMBOL_PCREL64))
	return false;
    /* fall through */
    case SYMBOL_PCREL:
      /* GAS rejects offsets outside the range [-2^31, 2^31-1].  */
      return sext_hwi (INTVAL (offset), 32) == INTVAL (offset);

    /* The following symbol types do not allow non-zero offsets.  */
    case SYMBOL_GOT_DISP:
    case SYMBOL_TLS_IE:
    case SYMBOL_TLSGD:
    case SYMBOL_TLSLDM:
    case SYMBOL_TLS:
    /* From an implementation perspective, tls_le symbols are allowed to
       have non-zero offsets, but currently binutils has not added support,
       so the generation of non-zero offsets is prohibited here.  */
    case SYMBOL_TLS_LE:
      return false;
    }
  gcc_unreachable ();
}

/* If -mexplicit-relocs=auto, we use machine operations with reloc hints
   for cases where the linker is unable to relax so we can schedule the
   machine operations, otherwise use an assembler pseudo-op so the
   assembler will generate R_LARCH_RELAX.  */

bool
loongarch_explicit_relocs_p (enum loongarch_symbol_type type)
{
  if (la_opt_explicit_relocs != EXPLICIT_RELOCS_AUTO)
    return la_opt_explicit_relocs == EXPLICIT_RELOCS_ALWAYS;

  /* The linker don't know how to relax accesses in extreme code model.  */
  if (loongarch_symbol_extreme_p (type))
    return true;

  switch (type)
    {
      case SYMBOL_TLS_IE:
      case SYMBOL_TLS_LE:
      case SYMBOL_PCREL64:
	/* TLS IE cannot be relaxed.  TLS LE relaxation is different from
	   the normal R_LARCH_RELAX-based relaxation and it **requires**
	   using the explicit %le_{lo12,hi20,add}_r relocs.  The linker
	   does not relax 64-bit pc-relative accesses as at now.  */
	return true;
      case SYMBOL_GOT_DISP:
	/* If we are performing LTO for a final link, and we have the
	   linker plugin so we know the resolution of the symbols, then
	   all GOT references are binding to external symbols or
	   preemptable symbols.  So the linker cannot relax them.  */
	return (in_lto_p
		&& !flag_incremental_link
		&& HAVE_LTO_PLUGIN == 2
		&& (!global_options_set.x_flag_use_linker_plugin
		    || global_options.x_flag_use_linker_plugin));
      default:
	return false;
    }
}

/* Returns the number of instructions necessary to reference a symbol.  */

static int
loongarch_symbol_insns (enum loongarch_symbol_type type, machine_mode mode)
{
  /* LSX LD.* and ST.* cannot support loading symbols via an immediate
     operand.  */
  if (mode != MAX_MACHINE_MODE
      && (LSX_SUPPORTED_MODE_P (mode) || LASX_SUPPORTED_MODE_P (mode)))
    return 0;

  switch (type)
    {
    case SYMBOL_GOT_DISP:
      /* The constant will have to be loaded from the GOT before it
	 is used in an address.  */
      if (!loongarch_explicit_relocs_p (type) && mode != MAX_MACHINE_MODE)
	return 0;

      return 3;

    case SYMBOL_PCREL:
    case SYMBOL_TLS_IE:
    case SYMBOL_TLS_LE:
      return 2;

    case SYMBOL_TLSGD:
    case SYMBOL_TLSLDM:
      return TARGET_TLS_DESC ? 4 : 3;

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
  if ((CONST_INT_P (x) || GET_CODE (x) == CONST_VECTOR)
      && loongarch_legitimate_constant_p (mode, x))
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
  if (!(LSX_SUPPORTED_MODE_P (mode) || LASX_SUPPORTED_MODE_P (mode))
      && GET_MODE_SIZE (mode) > UNITS_PER_WORD
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
loongarch_legitimate_address_p (machine_mode mode, rtx x, bool strict_p,
				code_helper = ERROR_MARK)
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
  bool lsx_p = (!might_split_p
		&& (LSX_SUPPORTED_MODE_P (mode)
		    || LASX_SUPPORTED_MODE_P (mode)));

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
	if (lsx_p)
	  {
	    /* LSX LD.* and ST.* supports 12-bit signed offsets.  */
	    if (IMM12_OPERAND (INTVAL (addr.offset)))
	      return 1;
	    else
	      return 0;
	  }
	return factor;

      case ADDRESS_REG_REG:
	return factor;

      case ADDRESS_CONST_INT:
	return lsx_p ? 0 : factor;

      case ADDRESS_LO_SUM:
	return factor + 1;

      case ADDRESS_SYMBOLIC:
	return lsx_p ? 0
	  : factor * loongarch_symbol_insns (addr.symbol_type, mode);
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

/* Return the scale shift that applied to LSX LD/ST address offset.  */

int
loongarch_ldst_scaled_shift (machine_mode mode)
{
  int shift = exact_log2 (GET_MODE_UNIT_SIZE (mode));

  if (shift < 0 || shift > 8)
    gcc_unreachable ();

  return shift;
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
      if ((LSX_SUPPORTED_MODE_P (GET_MODE (x))
	   || LASX_SUPPORTED_MODE_P (GET_MODE (x)))
	  && loongarch_const_vector_same_int_p (x, GET_MODE (x), -512, 511))
	return 1;
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

/* Return one word of 128-bit value OP, taking into account the fixed
   endianness of certain registers.  BYTE selects from the byte address.  */

rtx
loongarch_subword_at_byte (rtx op, unsigned int byte)
{
  machine_mode mode;

  mode = GET_MODE (op);
  if (mode == VOIDmode)
    mode = TImode;

  gcc_assert (!FP_REG_RTX_P (op));

  if (MEM_P (op))
    return loongarch_rewrite_small_data (adjust_address (op, word_mode, byte));

  return simplify_gen_subreg (word_mode, op, mode, byte);
}

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
	  && !loongarch_split_move_p (SET_DEST (set), SET_SRC (set)))
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

/* The __tls_get_addr symbol.  */
static GTY (()) rtx loongarch_tls_symbol;

/* Load an entry for a TLS access.  */

static rtx
loongarch_load_tls (rtx dest, rtx sym, enum loongarch_symbol_type type)
{
  /* TLS LE gets a 32 or 64 bit offset here, so one register can do it.  */
  if (type == SYMBOL_TLS_LE)
    return gen_load_tls (Pmode, dest, sym);

  return loongarch_symbol_extreme_p (type)
    ? gen_movdi_symbolic_off64 (dest, sym, gen_reg_rtx (DImode))
    : gen_load_tls (Pmode, dest, sym);
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

  if (loongarch_explicit_relocs_p (type))
    {
      if (TARGET_CMODEL_EXTREME)
	{
	  rtx part1 = gen_reg_rtx (Pmode);
	  rtx part2 = gen_reg_rtx (Pmode);

	  emit_insn (gen_la_pcrel64_two_parts (part1, part2, loc));
	  emit_move_insn (a0, gen_rtx_PLUS (Pmode, part1, part2));
	}
      else
	{
	  /* Split tls symbol to high and low.  */
	  rtx high = gen_rtx_HIGH (Pmode, copy_rtx (loc));

	  high = loongarch_force_temporary (tmp, high);
	  emit_insn (gen_tls_low (Pmode, a0, high, loc));
	}
    }
  else
    emit_insn (loongarch_load_tls (a0, loc, type));

  if (flag_plt)
    {
      switch (la_target.cmodel)
	{
	case CMODEL_NORMAL:
	  insn = emit_call_insn (gen_call_value_internal (v0,
							  loongarch_tls_symbol,
							  const0_rtx));
	  break;

	case CMODEL_MEDIUM:
	    {
	      if (la_opt_explicit_relocs != EXPLICIT_RELOCS_NONE)
		{
		  rtx call;

		 if (HAVE_AS_SUPPORT_CALL36)
		   call = gen_call_value_internal (v0, loongarch_tls_symbol,
						   const0_rtx);
		 else
		   {
		     rtx reg = gen_reg_rtx (Pmode);
		     emit_insn (gen_pcalau12i (Pmode, reg,
					       loongarch_tls_symbol));
		     call = gen_call_value_internal_1 (Pmode, v0, reg,
						       loongarch_tls_symbol,
						       const0_rtx);
		   }
		 insn = emit_call_insn (call);
		}
	      else
		{
		  rtx reg = gen_reg_rtx (Pmode);
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

      switch (la_target.cmodel)
	{
	case CMODEL_NORMAL:
	case CMODEL_MEDIUM:
	    {
	      if (loongarch_explicit_relocs_p (SYMBOL_GOT_DISP))
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
	      if (loongarch_explicit_relocs_p (SYMBOL_GOT_DISP))
		{
		  gcc_assert (la_opt_explicit_relocs != EXPLICIT_RELOCS_NONE);

		  rtx part1 = gen_reg_rtx (Pmode);
		  rtx part2 = gen_reg_rtx (Pmode);

		  emit_insn (gen_la_pcrel64_two_parts (part1, part2,
						       loongarch_tls_symbol));
		  loongarch_emit_move (
		    dest,
		    gen_rtx_MEM (Pmode, gen_rtx_PLUS (Pmode,
						      part1,
						      part2)));

		  /* Put an REG_EQUAL note here to allow CSE (storing
		     part1 + part2, i.e. the address of tls_get_addr into
		     a saved register and use it for multiple TLS
		     accesses).  */
		  rtx sum = gen_rtx_UNSPEC (
		    Pmode, gen_rtvec (1, loongarch_tls_symbol),
		    UNSPEC_ADDRESS_FIRST
		    + loongarch_classify_symbol (loongarch_tls_symbol));
		  set_unique_reg_note (get_last_insn (), REG_EQUAL, sum);
		}
	      else
	       emit_insn (gen_movdi_symbolic_off64 (dest, loongarch_tls_symbol,
						    gen_reg_rtx (DImode)));
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
  rtx dest, tp, tmp, tmp1, tmp2, tmp3, a0;
  enum tls_model model = SYMBOL_REF_TLS_MODEL (loc);
  rtx_insn *insn;

  switch (model)
    {
    case TLS_MODEL_LOCAL_DYNAMIC:
      if (!TARGET_TLS_DESC)
	{
	  tmp = gen_rtx_REG (Pmode, GP_RETURN);
	  dest = gen_reg_rtx (Pmode);
	  insn = loongarch_call_tls_get_addr (loc, SYMBOL_TLSLDM, tmp);
	  emit_libcall_block (insn, dest, tmp, loc);
	  break;
	}
      /* Fall through.  */
    case TLS_MODEL_GLOBAL_DYNAMIC:
      if (TARGET_TLS_DESC)
	{
	  a0 = gen_rtx_REG (Pmode, GP_ARG_FIRST);
	  dest = gen_reg_rtx (Pmode);
	  tp = gen_rtx_REG (Pmode, THREAD_POINTER_REGNUM);

	  if (TARGET_CMODEL_EXTREME)
	    emit_insn (gen_got_load_tls_desc_off64 (loc, gen_reg_rtx (DImode)));
	  else
	    emit_insn (gen_got_load_tls_desc (Pmode, loc));

	  emit_insn (gen_add3_insn (dest, a0, tp));
	}
      else
	{
	  tmp = gen_rtx_REG (Pmode, GP_RETURN);
	  dest = gen_reg_rtx (Pmode);
	  insn = loongarch_call_tls_get_addr (loc, SYMBOL_TLSGD, tmp);
	  emit_libcall_block (insn, dest, tmp, loc);
	}
      break;

    case TLS_MODEL_INITIAL_EXEC:
	{
	  /* la.tls.ie; tp-relative add.  */
	  tp = gen_rtx_REG (Pmode, THREAD_POINTER_REGNUM);
	  tmp1 = gen_reg_rtx (Pmode);
	  tmp2 = loongarch_unspec_address (loc, SYMBOL_TLS_IE);
	  dest = gen_reg_rtx (Pmode);
	  if (loongarch_explicit_relocs_p (SYMBOL_TLS_IE))
	    {
	      if (TARGET_CMODEL_EXTREME)
		{
		  gcc_assert (la_opt_explicit_relocs
			      != EXPLICIT_RELOCS_NONE);

		  rtx part1 = gen_reg_rtx (Pmode);
		  rtx part2 = gen_reg_rtx (Pmode);

		  emit_insn (gen_la_pcrel64_two_parts (part1, part2,
						       tmp2));
		  emit_move_insn (tmp1,
				  gen_rtx_MEM (Pmode,
					       gen_rtx_PLUS (Pmode,
							     part1,
							     part2)));
		}
	      else
		{
		  tmp3 = gen_reg_rtx (Pmode);
		  rtx high = gen_rtx_HIGH (Pmode, copy_rtx (tmp2));

		  high = loongarch_force_temporary (tmp3, high);
		  emit_insn (gen_ld_from_got (Pmode, tmp1, high, tmp2));
		}
	    }
	  else
	    emit_insn (loongarch_load_tls (tmp1, tmp2, SYMBOL_TLS_IE));
	  emit_insn (gen_add3_insn (dest, tmp1, tp));
	}
      break;

    case TLS_MODEL_LOCAL_EXEC:
	{
	  /* la.tls.le; tp-relative add.

	     normal:
	      lu12i.w $rd, %le_hi20(sym)
	      ori $rd, $rd, %le_lo12(sym)
	      add.{w/d} $rd, $rd, $tp
	      (st.{w/d}/ld.{w/d} $rs, $rd, 0)

	     tls le relax:
	      lu12i.w $rd, %le_hi20_r(sym)
	      add.{w/d} $rd,$rd,$tp
	      addi.{w/d} $rd,$rd,%le_lo12_r(sym)
	      (st.{w/d}/ld.{w/d} $rs, $rd, 0)

	     extreme (When the code model is set to extreme, the TLS le Relax
	     instruction sequence is not generated):
	      lu12i.w $rd, %le_hi20(sym)
	      ori $rd, $rd, %le_lo12(sym)
	      lu32i.d $rd, %le64_lo20(sym)
	      lu52i.d $rd, $rd, %le64_hi12(sym)
	      add.d $rd, $rd, $tp
	      (st.{w/d}/ld.{w/d} $rs, $rd, 0)  */

	  tp = gen_rtx_REG (Pmode, THREAD_POINTER_REGNUM);
	  tmp1 = gen_reg_rtx (Pmode);
	  tmp2 = loongarch_unspec_address (loc, SYMBOL_TLS_LE);
	  dest = gen_reg_rtx (Pmode);

	  if (loongarch_explicit_relocs_p (SYMBOL_TLS_LE))
	    {
	      tmp3 = gen_reg_rtx (Pmode);
	      rtx high = gen_rtx_HIGH (Pmode, copy_rtx (tmp2));
	      high = loongarch_force_temporary (tmp3, high);

	      /* The assembler does not implement tls le relax support when the
		 code model is extreme, so when the code model is extreme, the
		 old symbol address acquisition method is still used.  */
	      if (HAVE_AS_TLS_LE_RELAXATION && !TARGET_CMODEL_EXTREME)
		{
		  emit_insn (gen_add_tls_le_relax (Pmode, dest, high,
						   tp, loc));
		  loongarch_emit_move (dest,
				       gen_rtx_LO_SUM (Pmode, dest, tmp2));
		  return dest;
		}
	      else
		emit_insn (gen_ori_l_lo12 (Pmode, tmp1, high, tmp2));

	      if (TARGET_CMODEL_EXTREME)
		{
		  emit_insn (gen_lui_h_lo20 (tmp1, tmp1, tmp2));
		  emit_insn (gen_lui_h_hi12 (tmp1, tmp1, tmp2));
		}
	    }
	  else
	    emit_insn (loongarch_load_tls (tmp1, tmp2, SYMBOL_TLS_LE));
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

  /* If add the compilation option '-cmodel=medium', and the assembler does
     not support call36.  The following sequence of instructions will be
     used for the function call:
	pcalau12i $rd, %pc_hi20(sym)
	jr $rd, %pc_lo12(sym)
  */

  if (TARGET_CMODEL_MEDIUM
      && !HAVE_AS_SUPPORT_CALL36
      && (la_opt_explicit_relocs != EXPLICIT_RELOCS_NONE)
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

bool
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

   Return false if build with '-mexplicit-relocs=none'.

   TEMP is as for loongarch_force_temporary and is used to load the high
   part into a register.

   When MODE is MAX_MACHINE_MODE, the low part is guaranteed to be
   a legitimize SET_SRC for an .md pattern, otherwise the low part
   is guaranteed to be a legitimate address for mode MODE.  */

bool
loongarch_split_symbol (rtx temp, rtx addr, machine_mode mode, rtx *low_out)
{
  enum loongarch_symbol_type symbol_type;

  if ((GET_CODE (addr) == HIGH && mode == MAX_MACHINE_MODE)
      || !loongarch_symbolic_constant_p (addr, &symbol_type)
      || !loongarch_explicit_relocs_p (symbol_type)
      || loongarch_symbol_insns (symbol_type, mode) == 0
      || !loongarch_split_symbol_type (symbol_type))
    return false;

  rtx high;

  if (temp == NULL)
    temp = gen_reg_rtx (Pmode);

  if (loongarch_symbol_extreme_p (symbol_type) && can_create_pseudo_p ())
    {
      gcc_assert (la_opt_explicit_relocs != EXPLICIT_RELOCS_NONE);

      high = gen_reg_rtx (Pmode);
      emit_insn (gen_la_pcrel64_two_parts (high, temp, addr));
    }
  else
    {
      /* Get the 12-31 bits of the address.  */
      high = gen_rtx_HIGH (Pmode, copy_rtx (addr));
      high = loongarch_force_temporary (temp, high);
    }

  if (low_out)
    switch (symbol_type)
      {
      case SYMBOL_PCREL64:
	if (can_create_pseudo_p ())
	  {
	    *low_out = gen_rtx_PLUS (Pmode, high, temp);
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
	    *low_out = gen_rtx_MEM (Pmode, gen_rtx_PLUS (Pmode, high,
							 temp));
	  else
	    {
	      rtx low = gen_rtx_LO_SUM (Pmode, high, addr);
	      rtx mem = gen_rtx_MEM (Pmode, low);
	      *low_out = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, mem),
					 UNSPEC_LOAD_FROM_GOT);

	      /* Nonzero in a mem, if the memory is statically allocated and
		 read-only.  A common example of the later is a shared library’s
		 global offset table.  */
	      MEM_READONLY_P (mem) = 1;
	    }

	  break;
	}

      default:
	gcc_unreachable ();
      }

  return true;
}

/* Helper loongarch_legitimize_address.  Given X, return true if it
   is a left shift by 1, 2 or 3 positions or a multiply by 2, 4 or 8.

   This respectively represent canonical shift-add rtxs or scaled
   memory addresses.  */
static bool
mem_shadd_or_shadd_rtx_p (rtx x)
{
  return ((GET_CODE (x) == ASHIFT
	   || GET_CODE (x) == MULT)
	  && CONST_INT_P (XEXP (x, 1))
	  && ((GET_CODE (x) == ASHIFT && IN_RANGE (INTVAL (XEXP (x, 1)), 1, 3))
	      || (GET_CODE (x) == MULT
		  && IN_RANGE (exact_log2 (INTVAL (XEXP (x, 1))), 1, 3))));
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
      /* Handle (plus (plus (mult (a) (mem_shadd_constant)) (fp)) (C)) case.  */
      if (GET_CODE (base) == PLUS && mem_shadd_or_shadd_rtx_p (XEXP (base, 0))
	  && IMM12_OPERAND (offset))
	{
	  rtx index = XEXP (base, 0);
	  rtx fp = XEXP (base, 1);

	  if (REG_P (fp) && REGNO (fp) == VIRTUAL_STACK_VARS_REGNUM)
	    {
	      /* If we were given a MULT, we must fix the constant
		 as we're going to create the ASHIFT form.  */
	      int shift_val = INTVAL (XEXP (index, 1));
	      if (GET_CODE (index) == MULT)
		shift_val = exact_log2 (shift_val);

	      rtx reg1 = gen_reg_rtx (Pmode);
	      rtx reg3 = gen_reg_rtx (Pmode);
	      loongarch_emit_binary (PLUS, reg1, fp, GEN_INT (offset));
	      loongarch_emit_binary (PLUS, reg3,
				     gen_rtx_ASHIFT (Pmode, XEXP (index, 0),
						     GEN_INT (shift_val)),
				     reg1);

	      return reg3;
	    }
	}

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
	case METHOD_MIRROR:
	  gcc_assert (mode == DImode);
	  emit_insn (gen_insvdi (x, GEN_INT (32), GEN_INT (32), x));
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
     LSX and LASX are never able to source the zero register directly in
     memory operations.  */
  if (!register_operand (dest, mode) && !register_operand (src, mode)
      && (!const_0_operand (src, mode)
	  || LSX_SUPPORTED_MODE_P (mode) || LASX_SUPPORTED_MODE_P (mode)))
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

  /* Obtain the address of the symbol through the macro instruction
     of two registers.  */
  enum loongarch_symbol_type symbol_type;
  if (TARGET_64BIT && register_operand (dest, mode)
      && loongarch_symbolic_constant_p (src, &symbol_type)
      && loongarch_symbol_extreme_p (symbol_type))
    {
      gcc_assert (can_create_pseudo_p ());
      rtx tmp_reg = gen_reg_rtx (DImode);
      emit_insn (gen_movdi_symbolic_off64 (dest, src, tmp_reg));
      set_unique_reg_note (get_last_insn (), REG_UNUSED, tmp_reg);
      set_unique_reg_note (get_last_insn (), REG_EQUAL, src);
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

static int
loongarch_use_bstrins_for_ior_with_mask_1 (machine_mode mode,
					   unsigned HOST_WIDE_INT mask1,
					   unsigned HOST_WIDE_INT mask2)
{
  if (mask1 != ~mask2 || !mask1 || !mask2)
    return 0;

  /* Try to avoid a right-shift.  */
  if (low_bitmask_len (mode, mask1) != -1)
    return -1;

  if (low_bitmask_len (mode, mask2 >> (ffs_hwi (mask2) - 1)) != -1)
    return 1;

  if (low_bitmask_len (mode, mask1 >> (ffs_hwi (mask1) - 1)) != -1)
    return -1;

  return 0;
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
      {
	rtx op[2] = {XEXP (x, 0), XEXP (x, 1)};
	if (GET_CODE (op[0]) == AND && GET_CODE (op[1]) == AND
	    && (mode == SImode || (TARGET_64BIT && mode == DImode)))
	  {
	    rtx rtx_mask0 = XEXP (op[0], 1), rtx_mask1 = XEXP (op[1], 1);
	    if (CONST_INT_P (rtx_mask0) && CONST_INT_P (rtx_mask1))
	      {
		unsigned HOST_WIDE_INT mask0 = UINTVAL (rtx_mask0);
		unsigned HOST_WIDE_INT mask1 = UINTVAL (rtx_mask1);
		if (loongarch_use_bstrins_for_ior_with_mask_1 (mode,
							       mask0,
							       mask1))
		  {
		    /* A bstrins instruction */
		    *total = COSTS_N_INSNS (1);

		    /* A srai instruction */
		    if (low_bitmask_len (mode, mask0) == -1
			&& low_bitmask_len (mode, mask1) == -1)
		      *total += COSTS_N_INSNS (1);

		    for (int i = 0; i < 2; i++)
		      *total += set_src_cost (XEXP (op[i], 0), mode, speed);

		    return true;
		  }
	      }
	  }
      }

      /* Fall through.  */
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
      if (mode == DImode)
	*total = loongarch_cost->int_div_di;
      else
	{
	  *total = loongarch_cost->int_div_si;
	  if (TARGET_64BIT && !ISA_HAS_DIV32)
	    *total += COSTS_N_INSNS (2);
	}

      if (TARGET_CHECK_ZERO_DIV)
	*total += COSTS_N_INSNS (2);

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
	  if (mode == DImode)
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

/* Implement targetm.vectorize.builtin_vectorization_cost.  */

static int
loongarch_builtin_vectorization_cost (enum vect_cost_for_stmt type_of_cost,
				      tree vectype,
				      int misalign ATTRIBUTE_UNUSED)
{
  unsigned elements;
  machine_mode mode = vectype != NULL ? TYPE_MODE (vectype) : DImode;

  switch (type_of_cost)
    {
      case scalar_stmt:
      case scalar_load:
      case vector_stmt:
      case vec_to_scalar:
      case scalar_to_vec:
      case scalar_store:
	return 1;

      case vec_promote_demote:
      case vec_perm:
	return LASX_SUPPORTED_MODE_P (mode)
	  && !LSX_SUPPORTED_MODE_P (mode) ? 2 : 1;

      case vector_load:
      case vector_store:
      case unaligned_load:
      case unaligned_store:
	return 2;

      case cond_branch_taken:
	return 4;

      case cond_branch_not_taken:
	return 2;

      case vec_construct:
	elements = TYPE_VECTOR_SUBPARTS (vectype);
	if (LASX_SUPPORTED_MODE_P (mode) && !LSX_SUPPORTED_MODE_P (mode))
	  return elements / 2 + 3;
	else
	  return elements / 2 + 1;

      default:
	gcc_unreachable ();
    }
}

class loongarch_vector_costs : public vector_costs
{
public:
  using vector_costs::vector_costs;

  unsigned int add_stmt_cost (int count, vect_cost_for_stmt kind,
			      stmt_vec_info stmt_info, slp_tree, tree vectype,
			      int misalign,
			      vect_cost_model_location where) override;
  void finish_cost (const vector_costs *) override;

protected:
  void count_operations (vect_cost_for_stmt, stmt_vec_info,
			 vect_cost_model_location, unsigned int);
  unsigned int determine_suggested_unroll_factor (loop_vec_info);
  /* The number of vectorized stmts in loop.  */
  unsigned m_stmts = 0;
  /* The number of load and store operations in loop.  */
  unsigned m_loads = 0;
  unsigned m_stores = 0;
  /* Reduction factor for suggesting unroll factor.  */
  unsigned m_reduc_factor = 0;
  /* True if the loop contains an average operation. */
  bool m_has_avg = false;
  /* True if the loop uses approximation instruction sequence.  */
  bool m_has_recip = false;
};

/* Implement TARGET_VECTORIZE_CREATE_COSTS.  */
static vector_costs *
loongarch_vectorize_create_costs (vec_info *vinfo, bool costing_for_scalar)
{
  return new loongarch_vector_costs (vinfo, costing_for_scalar);
}

void
loongarch_vector_costs::count_operations (vect_cost_for_stmt kind,
					  stmt_vec_info stmt_info,
					  vect_cost_model_location where,
					  unsigned int count)
{
  if (!m_costing_for_scalar
      && is_a<loop_vec_info> (m_vinfo)
      && where == vect_body)
    {
      m_stmts += count;

      if (kind == scalar_load
	  || kind == vector_load
	  || kind == unaligned_load)
	m_loads += count;
      else if (kind == scalar_store
	       || kind == vector_store
	       || kind == unaligned_store)
	m_stores += count;
      else if ((kind == scalar_stmt
		|| kind == vector_stmt
		|| kind == vec_to_scalar)
	       && stmt_info && vect_is_reduction (stmt_info))
	{
	  tree lhs = gimple_get_lhs (stmt_info->stmt);
	  unsigned int base = FLOAT_TYPE_P (TREE_TYPE (lhs)) ? 2 : 1;
	  m_reduc_factor = MAX (base * count, m_reduc_factor);
	}
    }
}

unsigned int
loongarch_vector_costs::determine_suggested_unroll_factor (loop_vec_info loop_vinfo)
{
  class loop *loop = LOOP_VINFO_LOOP (loop_vinfo);

  if (m_has_avg || m_has_recip)
    return 1;

  /* Don't unroll if it's specified explicitly not to be unrolled.  */
  if (loop->unroll == 1
      || (OPTION_SET_P (flag_unroll_loops) && !flag_unroll_loops)
      || (OPTION_SET_P (flag_unroll_all_loops) && !flag_unroll_all_loops))
    return 1;

  unsigned int nstmts_nonldst = m_stmts - m_loads - m_stores;
  /* Don't unroll if no vector instructions excepting for memory access.  */
  if (nstmts_nonldst == 0)
    return 1;

  /* Use this simple hardware resource model that how many non vld/vst
     vector instructions can be issued per cycle.  */
  unsigned int issue_info = la_vect_issue_info;
  unsigned int reduc_factor = m_reduc_factor > 1 ? m_reduc_factor : 1;
  unsigned int uf = CEIL (reduc_factor * issue_info, nstmts_nonldst);
  uf = MIN ((unsigned int) la_vect_unroll_limit, uf);

  return 1 << ceil_log2 (uf);
}

/* Check if assign stmt rhs op comes from a multiply-add operation.  */
static bool
loongarch_multiply_add_p (vec_info *vinfo, stmt_vec_info stmt_info)
{
  gassign *assign = dyn_cast<gassign *> (stmt_info->stmt);
  if (!assign)
    return false;
  tree_code code = gimple_assign_rhs_code (assign);
  if (code != PLUS_EXPR && code != MINUS_EXPR)
    return false;

  auto is_mul_result = [&](int i)
    {
      tree rhs = gimple_op (assign, i);
      if (TREE_CODE (rhs) != SSA_NAME)
	return false;

      stmt_vec_info def_stmt_info = vinfo->lookup_def (rhs);
      if (!def_stmt_info
	  || STMT_VINFO_DEF_TYPE (def_stmt_info) != vect_internal_def)
	return false;
      gassign *rhs_assign = dyn_cast<gassign *> (def_stmt_info->stmt);
      if (!rhs_assign || gimple_assign_rhs_code (rhs_assign) != MULT_EXPR)
	return false;

      return true;
    };

  return is_mul_result (1) || is_mul_result (2);
}

unsigned
loongarch_vector_costs::add_stmt_cost (int count, vect_cost_for_stmt kind,
				       stmt_vec_info stmt_info, slp_tree,
				       tree vectype, int misalign,
				       vect_cost_model_location where)
{
  unsigned retval = 0;

  if (flag_vect_cost_model)
    {
      int stmt_cost = loongarch_builtin_vectorization_cost (kind, vectype,
							    misalign);
      if (vectype && stmt_info)
	{
	  gassign *assign = dyn_cast<gassign *> (STMT_VINFO_STMT (stmt_info));
	  machine_mode mode = TYPE_MODE (vectype);

	  /* We found through testing that this strategy (the stmt that
	     matches the multiply-add pattern) has positive returns only
	     when applied to the 128-bit vector stmt, so this restriction
	     is currently made.  */
	  if (kind == vector_stmt && GET_MODE_SIZE (mode) == 16 && assign)
	    {
	      if (!vect_is_reduction (stmt_info)
		  && loongarch_multiply_add_p (m_vinfo, stmt_info))
		stmt_cost = 0;
	    }
	}

      retval = adjust_cost_for_freq (stmt_info, where, count * stmt_cost);
      m_costs[where] += retval;

      count_operations (kind, stmt_info, where, count);
    }

  if (stmt_info)
    {
      /* Detect the use of an averaging operation.  */
      gimple *stmt = stmt_info->stmt;
      if (is_gimple_call (stmt)
	  && gimple_call_internal_p (stmt))
	{
	  switch (gimple_call_internal_fn (stmt))
	    {
	    case IFN_AVG_FLOOR:
	    case IFN_AVG_CEIL:
	      m_has_avg = true;
	    default:
	      break;
	    }
	}
    }

  combined_fn cfn;
  if (kind == vector_stmt
      && stmt_info
      && stmt_info->stmt)
    {
      /* Detect the use of approximate instruction sequence.  */
      if ((TARGET_RECIP_VEC_SQRT || TARGET_RECIP_VEC_RSQRT)
	  && (cfn = gimple_call_combined_fn (stmt_info->stmt)) != CFN_LAST)
	switch (cfn)
	  {
	  case CFN_BUILT_IN_SQRTF:
	    m_has_recip = true;
	  default:
	    break;
	  }
      else if (TARGET_RECIP_VEC_DIV
	       && gimple_code (stmt_info->stmt) == GIMPLE_ASSIGN)
	{
	  machine_mode mode = TYPE_MODE (vectype);
	  switch (gimple_assign_rhs_code (stmt_info->stmt))
	    {
	    case RDIV_EXPR:
	      if (GET_MODE_INNER (mode) == SFmode)
		m_has_recip = true;
	    default:
	      break;
	    }
	}
    }

  return retval;
}

void
loongarch_vector_costs::finish_cost (const vector_costs *scalar_costs)
{
  loop_vec_info loop_vinfo = dyn_cast<loop_vec_info> (m_vinfo);

  if (loop_vinfo)
    m_suggested_unroll_factor
      = determine_suggested_unroll_factor (loop_vinfo);

  vector_costs::finish_cost (scalar_costs);
}

/* Implement TARGET_ADDRESS_COST.  */

static int
loongarch_address_cost (rtx addr, machine_mode mode,
			addr_space_t as ATTRIBUTE_UNUSED,
			bool speed ATTRIBUTE_UNUSED)
{
  return loongarch_address_insns (addr, mode, false);
}

/* Implement TARGET_INSN_COST.  */

static int
loongarch_insn_cost (rtx_insn *insn, bool speed)
{
  rtx x = PATTERN (insn);
  int cost = pattern_cost (x, speed);

  /* On LA464, prevent movcf2fr and movfr2gr from merging into movcf2gr.  */
  if (GET_CODE (x) == SET
      && GET_MODE (XEXP (x, 0)) == FCCmode)
    {
      rtx dest, src;
      dest = XEXP (x, 0);
      src = XEXP (x, 1);

      if (REG_P (dest) && REG_P (src))
	{
	  if (GP_REG_P (REGNO (dest)) && FCC_REG_P (REGNO (src)))
	    cost = loongarch_cost->movcf2gr;
	  else if (FCC_REG_P (REGNO (dest)) && GP_REG_P (REGNO (src)))
	    cost = loongarch_cost->movgr2cf;
	}
    }
  return cost;
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

static bool loongarch_split_vector_move_p (rtx dest, rtx src);
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


  /* Check if vector moves need splitting.  */
  if (LSX_SUPPORTED_MODE_P (GET_MODE (dest))
      || LASX_SUPPORTED_MODE_P (GET_MODE (dest)))
    return loongarch_split_vector_move_p (dest, src);

  /* Otherwise split all multiword moves.  */
  return size > UNITS_PER_WORD;
}

/* Split a move from SRC to DEST, given that loongarch_split_move_p holds.
   SPLIT_TYPE describes the split condition.  */

void
loongarch_split_move (rtx dest, rtx src)
{
  gcc_checking_assert (loongarch_split_move_p (dest, src));
  if (LSX_SUPPORTED_MODE_P (GET_MODE (dest))
      || LASX_SUPPORTED_MODE_P (GET_MODE (dest)))
    loongarch_split_vector_move (dest, src);
  else
    gcc_unreachable ();
}

/* Check if adding an integer constant value for a specific mode can be
   performed with an addu16i.d instruction and an addi.{w/d}
   instruction.  */

bool
loongarch_addu16i_imm12_operand_p (HOST_WIDE_INT value, machine_mode mode)
{
  /* Not necessary, but avoid unnecessary calculation if !TARGET_64BIT.  */
  if (!TARGET_64BIT)
    return false;

  if ((value & 0xffff) == 0)
    return false;

  if (IMM12_OPERAND (value))
    return false;

  value = (value & ~HWIT_UC_0xFFF) + ((value & 0x800) << 1);
  return ADDU16I_OPERAND (trunc_int_for_mode (value, mode));
}

/* Split one integer constant op[0] into two (op[1] and op[2]) for constant
   plus operation in a specific mode.  The splitted constants can be added
   onto a register with a single instruction (addi.{d/w} or addu16i.d).  */

void
loongarch_split_plus_constant (rtx *op, machine_mode mode)
{
  HOST_WIDE_INT v = INTVAL (op[0]), a;

  if (DUAL_IMM12_OPERAND (v))
    a = (v > 0 ? 2047 : -2048);
  else if (loongarch_addu16i_imm12_operand_p (v, mode))
    a = (v & ~HWIT_UC_0xFFF) + ((v & 0x800) << 1);
  else if (mode == DImode && DUAL_ADDU16I_OPERAND (v))
    a = (v > 0 ? 0x7fff0000 : ~0x7fffffff);
  else
    gcc_unreachable ();

  op[1] = gen_int_mode (a, mode);
  v = v - (unsigned HOST_WIDE_INT) a;
  op[2] = gen_int_mode (v, mode);
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
  if (!IN_RANGE (index, 2, 5))
    return NULL;

  struct loongarch_address_info info;
  if ((loongarch_classify_address (&info, x, mode, false)
       && !(info.type == ADDRESS_REG_REG))
      || !loongarch_legitimate_address_p (mode, x, false))
    return NULL;

  const char *const insn[][4] =
    {
	{
	  "fstx.s\t%1,%0",
	  "fstx.d\t%1,%0",
	  "vstx\t%w1,%0",
	  "xvstx\t%u1,%0"
	},
	{
	  "fldx.s\t%0,%1",
	  "fldx.d\t%0,%1",
	  "vldx\t%w0,%1",
	  "xvldx\t%u0,%1"
	}
    };

  return insn[ldr][index-2];
}
/* Return true if a vector move from SRC to DEST should be split.  */

static bool
loongarch_split_vector_move_p (rtx dest, rtx src)
{
  /* Vector moves can be done in a single instruction.  */
  if (FP_REG_RTX_P (src) && FP_REG_RTX_P (dest))
    return false;

  /* Check for vector loads and stores.  */
  if (FP_REG_RTX_P (dest) && MEM_P (src))
    return false;
  if (FP_REG_RTX_P (src) && MEM_P (dest))
    return false;

  /* Check for vector set to an immediate const vector with valid replicated
     element.  */
  if (FP_REG_RTX_P (dest)
      && loongarch_const_vector_same_int_p (src, GET_MODE (src), -512, 511))
    return false;

  /* Check for vector load zero immediate.  */
  if (FP_REG_RTX_P (dest) && src == CONST0_RTX (GET_MODE (src)))
    return false;

  return true;
}

/* Split a vector move from SRC to DEST.  */

void
loongarch_split_vector_move (rtx dest, rtx src)
{
  int byte, index;
  rtx s, d;
  machine_mode mode = GET_MODE (dest);
  bool lsx_p = LSX_SUPPORTED_MODE_P (mode);

  if (FP_REG_RTX_P (dest))
    {
      gcc_assert (!MEM_P (src));

      rtx (*gen_vinsgr2vr_d) (rtx, rtx, rtx, rtx);

      if (lsx_p)
	{
	  mode = V2DImode;
	  gen_vinsgr2vr_d = gen_lsx_vinsgr2vr_d;
	}
      else
	{
	  mode = V4DImode;
	  gen_vinsgr2vr_d = gen_lasx_xvinsgr2vr_d;
	}

      rtx new_dest = dest;

      if (GET_MODE (dest) != mode)
	new_dest = simplify_gen_subreg (mode, dest, GET_MODE (dest), 0);

      for (byte = 0, index = 0; byte < GET_MODE_SIZE (GET_MODE (dest));
	   byte += UNITS_PER_WORD, index++)
	{
	  s = loongarch_subword_at_byte (src, byte);
	  emit_insn (gen_vinsgr2vr_d (new_dest, s, new_dest,
					  GEN_INT (1 << index)));
	}
    }
  else if (FP_REG_RTX_P (src))
    {
      gcc_assert (!MEM_P (dest));

      rtx (*gen_vpickve2gr_d) (rtx, rtx, rtx);

      if (lsx_p)
	{
	  mode = V2DImode;
	  gen_vpickve2gr_d = gen_lsx_vpickve2gr_d;
	}
      else
	{
	  mode = V4DImode;
	  gen_vpickve2gr_d = gen_lasx_xvpickve2gr_d;
	}

      rtx new_src = src;
      if (GET_MODE (src) != mode)
	new_src = simplify_gen_subreg (mode, src, GET_MODE (src), 0);

      for (byte = 0, index = 0; byte < GET_MODE_SIZE (GET_MODE (src));
	   byte += UNITS_PER_WORD, index++)
	{
	  d = loongarch_subword_at_byte (dest, byte);
	  emit_insn (gen_vpickve2gr_d (d, new_src, GEN_INT (index)));
	}
    }
  else
    {
      /* This part of the code is designed to handle the following situations:
	 (set (reg:V2DI 4 $r4)
	      (reg:V2DI 6 $r6))
	 The trigger test case is lsx-mov-1.c.  */
      rtx low_dest, low_src;

      low_dest = loongarch_subword_at_byte (dest, 0);
      low_src = loongarch_subword_at_byte (src, 0);
      gcc_assert (REG_P (low_dest) && REG_P (low_src));
      /* Make sure the source register is not written before reading.  */
      if (REGNO (low_dest) <= REGNO (low_src))
	{
	  for (byte = 0; byte < GET_MODE_SIZE (GET_MODE (dest));
	       byte += UNITS_PER_WORD)
	    {
	      d = loongarch_subword_at_byte (dest, byte);
	      s = loongarch_subword_at_byte (src, byte);
	      loongarch_emit_move (d, s);
	    }
	}
      else
	{
	  for (byte = GET_MODE_SIZE (GET_MODE (dest)) - UNITS_PER_WORD;
	       byte >= 0; byte -= UNITS_PER_WORD)
	    {
	      d = loongarch_subword_at_byte (dest, byte);
	      s = loongarch_subword_at_byte (src, byte);
	      loongarch_emit_move (d, s);
	    }
	}
    }
}

/* Return the appropriate instructions to move SRC into DEST.  Assume
   that SRC is operand 1 and DEST is operand 0.  */

const char *
loongarch_output_move (rtx *operands)
{
  rtx src = operands[1];
  rtx dest = operands[0];
  enum rtx_code dest_code = GET_CODE (dest);
  enum rtx_code src_code = GET_CODE (src);
  machine_mode mode = GET_MODE (dest);
  bool dbl_p = (GET_MODE_SIZE (mode) == 8);
  bool lsx_p = LSX_SUPPORTED_MODE_P (mode);
  bool lasx_p = LASX_SUPPORTED_MODE_P (mode);

  if (loongarch_split_move_p (dest, src))
    return "#";

  if ((lsx_p || lasx_p)
      && dest_code == REG && FP_REG_P (REGNO (dest))
      && src_code == CONST_VECTOR
      && CONST_INT_P (CONST_VECTOR_ELT (src, 0)))
    {
      gcc_assert (loongarch_const_vector_same_int_p (src, mode, -512, 511));
      switch (GET_MODE_SIZE (mode))
	{
	case 16:
	  return "vrepli.%v0\t%w0,%E1";
	case 32:
	  return "xvrepli.%v0\t%u0,%E1";
	default: gcc_unreachable ();
	}
    }

  if ((src_code == REG && GP_REG_P (REGNO (src)))
      || (src == CONST0_RTX (mode)))
    {
      if (dest_code == REG)
	{
	  if (GP_REG_P (REGNO (dest)))
	    return "or\t%0,%z1,$r0";

	  if (FP_REG_P (REGNO (dest)))
	    {
	      if (lsx_p || lasx_p)
		{
		  gcc_assert (src == CONST0_RTX (GET_MODE (src)));
		  switch (GET_MODE_SIZE (mode))
		    {
		    case 16:
		      return "vrepli.b\t%w0,0";
		    case 32:
		      return "xvrepli.b\t%u0,0";
		    default:
		      gcc_unreachable ();
		    }
		}
	      if (ISA_HAS_LSX && src == CONST0_RTX (GET_MODE (src)))
		return "vxor.v\t%w0,%w0,%w0";

	      return dbl_p ? "movgr2fr.d\t%0,%z1" : "movgr2fr.w\t%0,%z1";
	    }
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
	  {
	    gcc_assert (!lsx_p);
	    return dbl_p ? "movfr2gr.d\t%0,%1" : "movfr2gr.s\t%0,%1";
	  }

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
	    return "%Q1pcalau12i\t%0,%h1";
	}

      if (src_code == CONST_INT)
	{
	  if (LU12I_INT (src))
	    {
	      operands[1] = GEN_INT (INTVAL (operands[1]) >> 12);
	      return "lu12i.w\t%0,%1\t\t\t# %X1";
	    }
	  else if (IMM12_INT (src))
	    return "addi.w\t%0,$r0,%1\t\t\t# %X1";
	  else if (IMM12_INT_UNSIGNED (src))
	    return "ori\t%0,$r0,%1\t\t\t# %X1";
	  else if (LU52I_INT (src))
	    {
	      operands[1] = GEN_INT (INTVAL (operands[1]) >> 52);
	      return "lu52i.d\t%0,$r0,%X1\t\t\t# %1";
	    }
	  else
	    gcc_unreachable ();
	}
    }

  if (!loongarch_explicit_relocs_p (loongarch_classify_symbol (src))
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
	{
	  if (lsx_p || lasx_p)
	    {
	      switch (GET_MODE_SIZE (mode))
		{
		case 16:
		  return "vori.b\t%w0,%w1,0";
		case 32:
		  return "xvori.b\t%u0,%u1,0";
		default:
		  gcc_unreachable ();
		}
	    }

	  return dbl_p ? "fmov.d\t%0,%1" : "fmov.s\t%0,%1";
	}

      if (dest_code == MEM)
	{
	  const char *insn = NULL;
	  insn = loongarch_output_move_index_float (XEXP (dest, 0),
						    GET_MODE (dest),
						    false);
	  if (insn)
	    return insn;

	  if (lsx_p || lasx_p)
	    {
	      switch (GET_MODE_SIZE (mode))
		{
		case 16:
		  return "vst\t%w1,%0";
		case 32:
		  return "xvst\t%u1,%0";
		default:
		  gcc_unreachable ();
		}
	    }

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

	  if (lsx_p || lasx_p)
	    {
	      switch (GET_MODE_SIZE (mode))
		{
		case 16:
		  return "vld\t%w0,%1";
		case 32:
		  return "xvld\t%u0,%1";
		default:
		  gcc_unreachable ();
		}
	    }
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

/* Sign- or zero-extend OP0 and OP1 for integer comparisons.  */

static void
loongarch_extend_comparands (rtx_code code, rtx *op0, rtx *op1)
{
  /* Comparisons consider all GRLEN bits, so extend sub-GRLEN values.  */
  if (GET_MODE_SIZE (word_mode) > GET_MODE_SIZE (GET_MODE (*op0)))
    {
      /* It is more profitable to zero-extend QImode values.  But not if the
	 first operand has already been sign-extended, and the second one is
	 is a constant or has already been sign-extended also.  */
      if (unsigned_condition (code) == code
	  && (GET_MODE (*op0) == QImode
	      && ! (GET_CODE (*op0) == SUBREG
		    && SUBREG_PROMOTED_VAR_P (*op0)
		    && SUBREG_PROMOTED_SIGNED_P (*op0)
		    && (CONST_INT_P (*op1)
			|| (GET_CODE (*op1) == SUBREG
			    && SUBREG_PROMOTED_VAR_P (*op1)
			    && SUBREG_PROMOTED_SIGNED_P (*op1))))))
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
  *op0 = gen_reg_rtx (FCCmode);

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
  rtx op0_extend = op0;
  rtx op1_extend = op1;

  /* Record whether operands[2] and operands[3] modes are promoted to word_mode.  */
  bool promote_op[2] = {false, false};
  bool promote_p = false;
  machine_mode mode = GET_MODE (operands[0]);

  if (FLOAT_MODE_P (GET_MODE (op1)))
    loongarch_emit_float_compare (&code, &op0, &op1);
  else
    {
      /* Optimize to reduce the number of instructions for ternary operations.
	 Mainly implemented based on noce_try_cmove_arith.
	 For dest = (condition) ? value_if_true : value_if_false;
	 the optimization requires:
	  a. value_if_false = var;
	  b. value_if_true = var OP C (a positive integer power of 2).

	 Situations similar to the following:
	    if (condition)
	      dest += 1 << imm;
	 to:
	    dest += (condition ? 1 : 0) << imm;  */

      rtx_insn *insn;
      HOST_WIDE_INT val = 0; /* The value of rtx C.  */
      /* INSN with operands[2] as the output.  */
      rtx_insn *value_if_true_insn = NULL;
      /* INSN with operands[3] as the output.  */
      rtx_insn *value_if_false_insn = NULL;
      rtx value_if_true_insn_src = NULL_RTX;
      /* Common operand var in value_if_true and value_if_false.  */
      rtx comm_var = NULL_RTX;
      bool can_be_optimized = false;

      /* Search value_if_true_insn and value_if_false_insn.  */
      struct sequence_stack *seq = get_current_sequence ()->next;
      for (insn = seq->last; insn; insn = PREV_INSN (insn))
	{
	  if (single_set (insn))
	    {
	      rtx set_dest = SET_DEST (single_set (insn));
	      if (rtx_equal_p (set_dest, operands[2]))
		value_if_true_insn = insn;
	      else if (rtx_equal_p (set_dest, operands[3]))
		value_if_false_insn = insn;
	      if (value_if_true_insn && value_if_false_insn)
		break;
	    }
	}

      /* Check if the optimization conditions are met.  */
      if (value_if_true_insn
	  && value_if_false_insn
	  /* Make sure that value_if_false and var are the same.  */
	  && BINARY_P (value_if_true_insn_src
		       = SET_SRC (single_set (value_if_true_insn)))
	  /* Make sure that both value_if_true and value_if_false
	     has the same var.  */
	  && rtx_equal_p (XEXP (value_if_true_insn_src, 0),
			  SET_SRC (single_set (value_if_false_insn))))
	{
	  comm_var = SET_SRC (single_set (value_if_false_insn));
	  rtx src = XEXP (value_if_true_insn_src, 1);
	  rtx imm = NULL_RTX;
	  if (CONST_INT_P (src))
	    imm = src;
	  else
	    for (insn = seq->last; insn; insn = PREV_INSN (insn))
	      {
		rtx set = single_set (insn);
		if (set && rtx_equal_p (SET_DEST (set), src))
		  {
		    imm = SET_SRC (set);
		    break;
		  }
	      }
	  if (imm && CONST_INT_P (imm))
	    {
	      val = INTVAL (imm);
	      /* Make sure that imm is a positive integer power of 2.  */
	      if (val > 0 && !(val & (val - 1)))
		can_be_optimized = true;
	    }
	}

      if (GET_MODE_SIZE (GET_MODE (op0)) < UNITS_PER_WORD)
	{
	  promote_op[0] = (REG_P (op0) && REG_P (operands[2]) &&
			   REGNO (op0) == REGNO (operands[2]));
	  promote_op[1] = (REG_P (op1) && REG_P (operands[3]) &&
			   REGNO (op1) == REGNO (operands[3]));
	}

      if (promote_op[0] || promote_op[1])
	{
	  mode = word_mode;
	  promote_p = true;
	}

      loongarch_extend_comparands (code, &op0, &op1);

      op0 = force_reg (word_mode, op0);
      op0_extend = op0;
      op1_extend = force_reg (word_mode, op1);

      rtx target = gen_reg_rtx (GET_MODE (op0));

      if (code == EQ || code == NE)
	{
	  op0 = loongarch_zero_if_equal (op0, op1);
	  op1 = const0_rtx;
	  /* For EQ, set target to 1 if op0 and op1 are the same,
	     otherwise set to 0.
	     For NE, set target to 0 if op0 and op1 are the same,
	     otherwise set to 1.  */
	  if (can_be_optimized)
	    loongarch_emit_binary (code, target, op0, const0_rtx);
	}
      else
	{
	  /* The comparison needs a separate scc instruction.  Store the
	     result of the scc in *OP0 and compare it against zero.  */
	  bool invert = false;
	  loongarch_emit_int_order_test (code, &invert, target, op0, op1);
	  if (can_be_optimized && invert)
	    loongarch_emit_binary (EQ, target, target, const0_rtx);
	  code = invert ? EQ : NE;
	  op0 = target;
	  op1 = const0_rtx;
	}

      if (can_be_optimized)
	{
	  /* Perform (condition ? 1 : 0) << log2 (C).  */
	  loongarch_emit_binary (ASHIFT, target, target,
				 GEN_INT (exact_log2 (val)));
	  /* Shift-related insn patterns only support SImode operands[2].  */
	  enum rtx_code opcode = GET_CODE (value_if_true_insn_src);
	  if (opcode == ASHIFT || opcode == ASHIFTRT || opcode == LSHIFTRT
	      || opcode == ROTATE || opcode == ROTATERT)
	    target = gen_lowpart (SImode, target);
	  /* Perform target = target OP ((condition ? 1 : 0) << log2 (C)).  */
	  loongarch_emit_binary (opcode, operands[0],
				 force_reg (GET_MODE (operands[3]), comm_var),
				 target);
	  return;
	}
    }

  rtx cond = gen_rtx_fmt_ee (code, GET_MODE (op0), op0, op1);
  /* There is no direct support for general conditional GP move involving
     two registers using SEL.  */
  if (INTEGRAL_MODE_P (GET_MODE (operands[2]))
      && register_operand (operands[2], VOIDmode)
      && register_operand (operands[3], VOIDmode))
    {
      rtx op2 = operands[2];
      rtx op3 = operands[3];

      if (promote_p)
	{
	  if (promote_op[0])
	    op2 = op0_extend;
	  else
	    {
	      loongarch_extend_comparands (code, &op2, &const0_rtx);
	      op2 = force_reg (mode, op2);
	    }

	  if (promote_op[1])
	    op3 = op1_extend;
	  else
	    {
	      loongarch_extend_comparands (code, &op3, &const0_rtx);
	      op3 = force_reg (mode, op3);
	    }
	}

      rtx temp = gen_reg_rtx (mode);
      rtx temp2 = gen_reg_rtx (mode);

      emit_insn (gen_rtx_SET (temp,
			      gen_rtx_IF_THEN_ELSE (mode, cond,
						    op2, const0_rtx)));

      /* Flip the test for the second operand.  */
      cond = gen_rtx_fmt_ee ((code == EQ) ? NE : EQ, GET_MODE (op0), op0, op1);

      emit_insn (gen_rtx_SET (temp2,
			      gen_rtx_IF_THEN_ELSE (mode, cond,
						    op3, const0_rtx)));

      /* Merge the two results, at least one is guaranteed to be zero.  */
      if (promote_p)
	{
	  rtx temp3 = gen_reg_rtx (mode);
	  emit_insn (gen_rtx_SET (temp3, gen_rtx_IOR (mode, temp, temp2)));
	  temp3 = gen_lowpart (GET_MODE (operands[0]), temp3);
	  /* Nonzero in a subreg if it was made when accessing an object that
	     was promoted to a wider mode in accord with the PROMOTED_MODE
	     machine description macro.  */
	  SUBREG_PROMOTED_VAR_P (temp3) = 1;
	  /* Sets promoted mode for SUBREG_PROMOTED_VAR_P.  */
	  SUBREG_PROMOTED_SET (temp3, SRP_SIGNED);
	  loongarch_emit_move (operands[0], temp3);
	}
      else
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

static machine_mode
loongarch_mode_for_move_size (HOST_WIDE_INT size)
{
  switch (size)
    {
    case 32:
      return V32QImode;
    case 16:
      return V16QImode;
    }

  return int_mode_for_size (size * BITS_PER_UNIT, 0).require ();
}

/* Emit straight-line code to move LENGTH bytes from SRC to DEST.
   Assume that the areas do not overlap.  */

static void
loongarch_block_move_straight (rtx dest, rtx src, HOST_WIDE_INT length,
			       HOST_WIDE_INT delta)
{
  HOST_WIDE_INT offs, delta_cur;
  int i;
  machine_mode mode;
  rtx *regs;

  /* Calculate how many registers we'll need for the block move.
     We'll emit length / delta move operations with delta as the size
     first.  Then we may still have length % delta bytes not copied.
     We handle these remaining bytes by move operations with smaller
     (halfed) sizes.  For example, if length = 21 and delta = 8, we'll
     emit two ld.d/st.d pairs, one ld.w/st.w pair, and one ld.b/st.b
     pair.  For each load/store pair we use a dedicated register to keep
     the pipeline as populated as possible.  */
  gcc_assert (pow2p_hwi (delta));
  HOST_WIDE_INT num_reg = length / delta + popcount_hwi (length % delta);

  /* Allocate a buffer for the temporary registers.  */
  regs = XALLOCAVEC (rtx, num_reg);

  for (delta_cur = delta, i = 0, offs = 0; offs < length; delta_cur /= 2)
    {
      mode = loongarch_mode_for_move_size (delta_cur);

      for (; offs + delta_cur <= length; offs += delta_cur, i++)
	{
	  regs[i] = gen_reg_rtx (mode);
	  loongarch_emit_move (regs[i], adjust_address (src, mode, offs));
	}
    }

  for (delta_cur = delta, i = 0, offs = 0; offs < length; delta_cur /= 2)
    {
      mode = loongarch_mode_for_move_size (delta_cur);

      for (; offs + delta_cur <= length; offs += delta_cur, i++)
	loongarch_emit_move (adjust_address (dest, mode, offs), regs[i]);
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
			   HOST_WIDE_INT align)
{
  rtx_code_label *label;
  rtx src_reg, dest_reg, final_src, test;
  HOST_WIDE_INT bytes_per_iter = align * LARCH_MAX_MOVE_OPS_PER_LOOP_ITER;
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
  loongarch_block_move_straight (dest, src, bytes_per_iter, align);

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
    loongarch_block_move_straight (dest, src, leftover, align);
  else
    /* Temporary fix for PR79150.  */
    emit_insn (gen_nop ());
}

/* Expand a cpymemsi instruction, which copies LENGTH bytes from
   memory reference SRC to memory reference DEST.  */

bool
loongarch_expand_block_move (rtx dest, rtx src, rtx r_length, rtx r_align)
{
  if (!CONST_INT_P (r_length))
    return false;

  HOST_WIDE_INT length = INTVAL (r_length);
  if (length > la_max_inline_memcpy_size)
    return false;

  HOST_WIDE_INT align = INTVAL (r_align);

  if (!TARGET_STRICT_ALIGN || align > LARCH_MAX_MOVE_PER_INSN)
    align = LARCH_MAX_MOVE_PER_INSN;

  if (length <= align * LARCH_MAX_MOVE_OPS_STRAIGHT)
    {
      loongarch_block_move_straight (dest, src, length, align);
      return true;
    }

  if (optimize)
    {
      loongarch_block_move_loop (dest, src, length, align);
      return true;
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

/* Predicate for pre-reload splitters with associated instructions,
   which can match any time before the split1 pass (usually combine),
   then are unconditionally split in that pass and should not be
   matched again afterwards.  */

bool loongarch_pre_reload_split (void)
{
  return (can_create_pseudo_p ()
	  && !(cfun->curr_properties & PROP_rtl_split_insns));
}

/* Check if we can use bstrins.<d> for
   op0 = (op1 & op2) | (op3 & op4)
   where op0, op1, op3 are regs, and op2, op4 are integer constants.  */
int
loongarch_use_bstrins_for_ior_with_mask (machine_mode mode, rtx *op)
{
  return loongarch_use_bstrins_for_ior_with_mask_1 (mode,
						    UINTVAL (op[2]),
						    UINTVAL (op[4]));
}

/* Rewrite a MEM for simple load/store under -mexplicit-relocs=auto
   -mcmodel={normal/medium}.  */
rtx
loongarch_rewrite_mem_for_simple_ldst (rtx mem)
{
  rtx addr = XEXP (mem, 0);
  rtx hi = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr),
			   UNSPEC_PCALAU12I_GR);
  rtx new_mem;

  addr = gen_rtx_LO_SUM (Pmode, force_reg (Pmode, hi), addr);
  new_mem = gen_rtx_MEM (GET_MODE (mem), addr);
  MEM_COPY_ATTRIBUTES (new_mem, mem);
  return new_mem;
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
  switch (memmodel_base (model))
    {
      case MEMMODEL_ACQ_REL:
      case MEMMODEL_SEQ_CST:
      case MEMMODEL_RELEASE:
      case MEMMODEL_ACQUIRE:
	return true;

      case MEMMODEL_RELAXED:
	return false;

      default:
	gcc_unreachable ();
    }
}

/* Return true if a FENCE should be emitted after a failed CAS to
   implement the acquire semantic of failure_memorder.  */

static bool
loongarch_cas_failure_memorder_needs_acquire (enum memmodel model)
{
  switch (memmodel_base (model))
    {
    case MEMMODEL_ACQUIRE:
    case MEMMODEL_ACQ_REL:
    case MEMMODEL_SEQ_CST:
      return true;

    case MEMMODEL_RELAXED:
    case MEMMODEL_RELEASE:
      return false;

    /* MEMMODEL_CONSUME is deliberately not handled because it's always
       replaced by MEMMODEL_ACQUIRE as at now.  If you see an ICE caused by
       MEMMODEL_CONSUME, read the change (re)introducing it carefully and
       decide what to do.  See PR 59448 and get_memmodel in builtins.cc.  */
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
    gcc_assert (la_opt_explicit_relocs != EXPLICIT_RELOCS_NONE);

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
	{
	  if (HAVE_AS_TLS_LE_RELAXATION && !TARGET_CMODEL_EXTREME)
	    reloc = hi_reloc ? "%le_hi20_r" : "%le_lo12_r";
	  else
	    reloc = hi_reloc ? "%le_hi20" : "%le_lo12";
	}
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
   'B'	Print CONST_INT OP element 0 of a replicated CONST_VECTOR
	  as an unsigned byte [0..255].
   'c'  Print an integer.
   'C'	Print the integer branch condition for comparison OP.
   'd'	Print CONST_INT OP in decimal.
   'E'	Print CONST_INT OP element 0 of a replicated CONST_VECTOR in decimal.
   'F'	Print the FPU branch condition for comparison OP.
   'G'	Print a DBAR insn for CAS failure (with an acquire semantic if
	needed, otherwise a simple load-load barrier).
   'H'  Print address 52-61bit relocation associated with OP.
   'h'  Print the high-part relocation associated with OP.
   'i'	Print i if the operand is not a register.
   'L'  Print the low-part relocation associated with OP.
   'm'	Print one less than CONST_INT OP in decimal.
   'N'	Print the inverse of the integer branch condition for comparison OP.
   'Q'  Print R_LARCH_RELAX for TLS IE.
   'r'  Print address 12-31bit relocation associated with OP.
   'R'  Print address 32-51bit relocation associated with OP.
   'T'	Print 'f' for (eq:CC ...), 't' for (ne:CC ...),
	      'z' for (eq:?I ...), 'n' for (ne:?I ...).
   't'	Like 'T', but with the EQ/NE cases reversed
   'u'	Print a LASX register.
   'v'	Print the insn size suffix b, h, w or d for vector modes V16QI, V8HI,
	  V4SI, V2SI, and w, d for vector modes V4SF, V2DF respectively.
   'V'	Print exact log2 of CONST_INT OP element 0 of a replicated
	  CONST_VECTOR in decimal.
   'W'	Print the inverse of the FPU branch condition for comparison OP.
   'w'	Print a LSX register.
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
    case 'E':
      if (GET_CODE (op) == CONST_VECTOR)
	{
	  gcc_assert (loongarch_const_vector_same_val_p (op, GET_MODE (op)));
	  op = CONST_VECTOR_ELT (op, 0);
	  gcc_assert (CONST_INT_P (op));
	  fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (op));
	}
      else
	output_operand_lossage ("invalid use of '%%%c'", letter);
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
      if (loongarch_cas_failure_memorder_needs_acquire (
	    memmodel_from_int (INTVAL (op))))
	fputs ("dbar\t0b10100", file);
      else if (!ISA_HAS_LD_SEQ_SA)
	fputs ("dbar\t0x700", file);
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
    case 'B':
      if (GET_CODE (op) == CONST_VECTOR)
	{
	  gcc_assert (loongarch_const_vector_same_val_p (op, GET_MODE (op)));
	  op = CONST_VECTOR_ELT (op, 0);
	  gcc_assert (CONST_INT_P (op));
	  unsigned HOST_WIDE_INT val8 = UINTVAL (op) & GET_MODE_MASK (QImode);
	  fprintf (file, HOST_WIDE_INT_PRINT_UNSIGNED, val8);
	}
      else
	output_operand_lossage ("invalid use of '%%%c'", letter);
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

    case 'Q':
      if (!TARGET_LINKER_RELAXATION)
	break;

      if (code == HIGH)
	op = XEXP (op, 0);

      if (loongarch_classify_symbolic_expression (op) == SYMBOL_TLS_IE)
	fprintf (file, ".reloc\t.,R_LARCH_RELAX\n\t");

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

    case 'w':
      if (code == REG && LSX_REG_P (REGNO (op)))
	fprintf (file, "$vr%s", &reg_names[REGNO (op)][2]);
      else
	output_operand_lossage ("invalid use of '%%%c'", letter);
      break;

    case 'u':
      if (code == REG && LASX_REG_P (REGNO (op)))
	fprintf (file, "$xr%s", &reg_names[REGNO (op)][2]);
      else
	output_operand_lossage ("invalid use of '%%%c'", letter);
      break;

    case 'v':
      switch (GET_MODE (op))
	{
	case E_V16QImode:
	case E_V32QImode:
	  fprintf (file, "b");
	  break;
	case E_V8HImode:
	case E_V16HImode:
	  fprintf (file, "h");
	  break;
	case E_V4SImode:
	case E_V4SFmode:
	case E_V8SImode:
	case E_V8SFmode:
	  fprintf (file, "w");
	  break;
	case E_V2DImode:
	case E_V2DFmode:
	case E_V4DImode:
	case E_V4DFmode:
	  fprintf (file, "d");
	  break;
	default:
	  output_operand_lossage ("invalid use of '%%%c'", letter);
	}
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
    return FCC_REG_P (regno) || GP_REG_P (regno) || FP_REG_P (regno);

  size = GET_MODE_SIZE (mode);
  mclass = GET_MODE_CLASS (mode);

  if (GP_REG_P (regno)
      && !LSX_SUPPORTED_MODE_P (mode)
      && !LASX_SUPPORTED_MODE_P (mode))
    return ((regno - GP_REG_FIRST) & 1) == 0 || size <= UNITS_PER_WORD;

  if (FP_REG_P (regno))
    {
      /* Allow 128-bit or 256-bit vector modes in all FPR.  */
      if (LSX_SUPPORTED_MODE_P (mode)
	  || LASX_SUPPORTED_MODE_P (mode))
	return true;

      if (mclass == MODE_FLOAT
	  || mclass == MODE_COMPLEX_FLOAT
	  || mclass == MODE_VECTOR_FLOAT)
	return size <= UNITS_PER_HWFPVALUE;

      /* Allow integer modes that fit into a single register.  We need
	 to put integers into FPRs when using instructions like CVT
	 and TRUNC.  There's no point allowing sizes smaller than a word,
	 because the FPU has no appropriate load/store instructions.  */
      if (mclass == MODE_INT)
	return size >= MIN_UNITS_PER_WORD && size <= UNITS_PER_FP_REG;
    }

  return false;
}

/* Implement TARGET_HARD_REGNO_MODE_OK.  */

static bool
loongarch_hard_regno_mode_ok (unsigned int regno, machine_mode mode)
{
  return loongarch_hard_regno_mode_ok_p[mode][regno];
}


static bool
loongarch_hard_regno_call_part_clobbered (unsigned int,
					  unsigned int regno, machine_mode mode)
{
  if (ISA_HAS_LSX && FP_REG_P (regno) && GET_MODE_SIZE (mode) > 8)
    return true;

  return false;
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
    {
      if (LSX_SUPPORTED_MODE_P (mode))
	return 1;

      if (LASX_SUPPORTED_MODE_P (mode))
	return 1;

      return (GET_MODE_SIZE (mode) + UNITS_PER_FP_REG - 1) / UNITS_PER_FP_REG;
    }

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
	{
	  /* Fixed me.  */
	  if (LASX_SUPPORTED_MODE_P (mode))
	    size = MIN (size, UNITS_PER_LASX_REG);
	  else if (LSX_SUPPORTED_MODE_P (mode))
	    size = MIN (size, UNITS_PER_LSX_REG);
	  else
	    size = MIN (size, UNITS_PER_FP_REG);
	}
      left &= ~reg_class_contents[FP_REGS];
    }
  if (!hard_reg_set_empty_p (left))
    size = MIN (size, UNITS_PER_WORD);
  return (GET_MODE_SIZE (mode) + size - 1) / size;
}

/* Implement TARGET_CAN_CHANGE_MODE_CLASS.  */

static bool
loongarch_can_change_mode_class (machine_mode from, machine_mode to,
				 reg_class_t rclass)
{
  /* Allow conversions between different LSX/LASX vector modes.  */
  if (LASX_SUPPORTED_MODE_P (from) && LASX_SUPPORTED_MODE_P (to))
    return true;

  /* Allow conversions between different LSX vector modes.  */
  if (LSX_SUPPORTED_MODE_P (from) && LSX_SUPPORTED_MODE_P (to))
    return true;

  /* Allow conversion between LSX vector mode and scalar fp mode. */
  if ((LSX_SUPPORTED_MODE_P (from) && SCALAR_FLOAT_MODE_P (to))
      || ((SCALAR_FLOAT_MODE_P (from) && LSX_SUPPORTED_MODE_P (to))))
    return true;

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
      return ISA_HAS_LASX ? LASX_SUPPORTED_MODE_P (mode)
	: LSX_SUPPORTED_MODE_P (mode);
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

    case FCC_REGS:
      return loongarch_cost->movcf2gr;

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

    case FCC_REGS:
      return loongarch_cost->movgr2cf;

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

  /* fcc -> fcc, fcc -> fpr, or fpr -> fcc. */
  if (from == FCC_REGS || to == FCC_REGS)
    return COSTS_N_INSNS (from == to ? 2 : 1);

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

  if (mode == FCCmode)
    {
      if (reg_class_subset_p (rclass, FCC_REGS) && !FP_REG_P (regno))
	{
	  if (FCC_REG_P (regno))
	    return FP_REGS;

	  auto fn = in_p ? loongarch_move_from_gpr_cost
			 : loongarch_move_to_gpr_cost;

	  if (fn (FCC_REGS) > fn (FP_REGS) + COSTS_N_INSNS (1))
	    return FP_REGS;

	  return GP_REG_P (regno) ? NO_REGS : GR_REGS;
	}

      if (reg_class_subset_p (rclass, GR_REGS) && FCC_REG_P (regno))
	{
	  auto fn = in_p ? loongarch_move_to_gpr_cost
			 : loongarch_move_from_gpr_cost;

	  if (fn (FCC_REGS) > fn (FP_REGS) + COSTS_N_INSNS (1))
	    return FP_REGS;

	  return NO_REGS;
	}

      if (reg_class_subset_p (rclass, FP_REGS)
	  && (regno == -1 || MEM_P (x)))
	return GR_REGS;

      return NO_REGS;
    }

  if (reg_class_subset_p (rclass, FP_REGS))
    {
      if (regno < 0
	  || (MEM_P (x)
	      && (GET_MODE_SIZE (mode) == 4 || GET_MODE_SIZE (mode) == 8)))
	/* In this case we can use lwc1, swc1, ldc1 or sdc1.  We'll use
	   pairs of lwc1s and swc1s if ldc1 and sdc1 are not supported.  */
	return NO_REGS;

      if (MEM_P (x) && LSX_SUPPORTED_MODE_P (mode))
	/* In this case we can use LSX LD.* and ST.*.  */
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

/* Implement TARGET_IRA_CHANGE_PSEUDO_ALLOCNO_CLASS.

   The register allocator chooses ALL_REGS if FP_REGS and GR_REGS have the
   same cost - even if ALL_REGS has a much higher cost.  ALL_REGS is also used
   if the cost of both FP_REGS and GR_REGS is lower than the memory cost (in
   this case the best class is the lowest cost one).  Using ALL_REGS
   irrespectively of itself cost results in bad allocations with many redundant
   int<->FP moves which are expensive on various cores.

   To avoid this we don't allow ALL_REGS as the allocno class, but force a
   decision between FP_REGS and GR_REGS.  We use the allocno class if it isn't
   ALL_REGS.  Similarly, use the best class if it isn't ALL_REGS.  Otherwise Set
   the allocno class depending on the mode.

   This change has a similar effect to increasing the cost of FPR->GPR register
   moves for integer modes so that they are higher than the cost of memory but
   changing the allocno class is more reliable.  */

static reg_class_t
loongarch_ira_change_pseudo_allocno_class (int regno, reg_class_t allocno_class,
					   reg_class_t best_class)
{
  enum machine_mode mode;

  if (allocno_class != ALL_REGS)
    return allocno_class;

  if (best_class != ALL_REGS)
    return best_class;

  mode = PSEUDO_REGNO_MODE (regno);
  return FLOAT_MODE_P (mode) || VECTOR_MODE_P (mode) ? FP_REGS : GR_REGS;
}

/* Implement TARGET_VALID_POINTER_MODE.  */

static bool
loongarch_valid_pointer_mode (scalar_int_mode mode)
{
  return mode == SImode || (TARGET_64BIT && mode == DImode);
}

/* Implement TARGET_VECTOR_MODE_SUPPORTED_P.  */

static bool
loongarch_vector_mode_supported_p (machine_mode mode)
{
  return ISA_HAS_LASX ? LASX_SUPPORTED_MODE_P (mode)
    : LSX_SUPPORTED_MODE_P (mode);
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

/* Implement TARGET_VECTORIZE_PREFERRED_SIMD_MODE.  */

static machine_mode
loongarch_preferred_simd_mode (scalar_mode mode)
{
  if (!ISA_HAS_LSX)
    return word_mode;

  switch (mode)
    {
    case E_QImode:
      return ISA_HAS_LASX ? E_V32QImode : E_V16QImode;
    case E_HImode:
      return ISA_HAS_LASX ? E_V16HImode : E_V8HImode;
    case E_SImode:
      return ISA_HAS_LASX ? E_V8SImode : E_V4SImode;
    case E_DImode:
      return ISA_HAS_LASX ? E_V4DImode : E_V2DImode;

    case E_SFmode:
      return ISA_HAS_LASX ? E_V8SFmode : E_V4SFmode;

    case E_DFmode:
      return ISA_HAS_LASX ? E_V4DFmode : E_V2DFmode;

    default:
      break;
    }
  return word_mode;
}

static unsigned int
loongarch_autovectorize_vector_modes (vector_modes *modes, bool)
{
  if (ISA_HAS_LASX)
    {
      modes->safe_push (V32QImode);
      modes->safe_push (V16QImode);
    }
  else if (ISA_HAS_LSX)
    {
      modes->safe_push (V16QImode);
    }

  return 0;
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

/* Return the assembly code for LSX DIV_{S,U}.DF or MOD_{S,U}.DF instructions,
   which has the operands given by OPERANDS.  Add in a divide-by-zero check
   if needed.  */

const char *
loongarch_lsx_output_division (const char *division, rtx *operands)
{
  const char *s;
  machine_mode mode = GET_MODE (*operands);

  s = division;
  if (TARGET_CHECK_ZERO_DIV)
    {
      if (ISA_HAS_LASX && GET_MODE_SIZE (mode) == 32)
	{
	  output_asm_insn ("xvsetallnez.%v0\t$fcc7,%u2",operands);
	  output_asm_insn (s, operands);
	  output_asm_insn ("bcnez\t$fcc7,1f", operands);
	}
      else if (ISA_HAS_LSX)
	{
	  output_asm_insn ("vsetallnez.%v0\t$fcc7,%w2",operands);
	  output_asm_insn (s, operands);
	  output_asm_insn ("bcnez\t$fcc7,1f", operands);
	}
      s = "break\t7\n1:";
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
  if ((unsigned long) la_target.cpu_tune < N_TUNE_TYPES)
    return loongarch_cpu_issue_rate[la_target.cpu_tune];
  else
    return 1;
}

/* Implement TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD.  This should
   be as wide as the scheduling freedom in the DFA.  */

static int
loongarch_multipass_dfa_lookahead (void)
{
  if ((unsigned long) la_target.cpu_tune < N_ARCH_TYPES)
    return loongarch_cpu_multipass_dfa_lookahead[la_target.cpu_tune];
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
      /* If TARGET_CMODEL_EXTREME, we cannot do a direct jump at all
	 and const_call_insn_operand should have returned false.  */
      gcc_assert (!TARGET_CMODEL_EXTREME);

      insn = emit_call_insn (gen_sibcall_internal (fnaddr, const0_rtx));
      SIBLING_CALL_P (insn) = 1;
    }
  else
    {
      if (!TARGET_CMODEL_EXTREME)
	loongarch_emit_move (temp1, fnaddr);
      else if (la_opt_explicit_relocs == EXPLICIT_RELOCS_NONE)
	emit_insn (gen_movdi_symbolic_off64 (temp1, fnaddr, temp2));
      else
	{
	  emit_insn (gen_la_pcrel64_two_parts (temp1, temp2, fnaddr));
	  emit_move_insn (temp1, gen_rtx_PLUS (Pmode, temp1, temp2));
	}

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
loongarch_global_init (void)
{
  /* Initialize loongarch_print_operand_punct.  */
  for (const char *p = ".$"; *p; p++)
    loongarch_print_operand_punct[(unsigned char) *p] = true;

  /* Set up array to map GCC register number to debug register number.
     Ignore the special purpose register numbers.  */
  for (int i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (GP_REG_P (i) || FP_REG_P (i))
	loongarch_dwarf_regno[i] = i;
      else
	loongarch_dwarf_regno[i] = INVALID_REGNUM;
    }

  /* Function to allocate machine-dependent function status.  */
  init_machine_status = &loongarch_init_machine_status;
}

static void
loongarch_reg_init (void)
{
  /* Set up loongarch_hard_regno_mode_ok.  */
  for (int mode = 0; mode < MAX_MACHINE_MODE; mode++)
    for (int regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
      loongarch_hard_regno_mode_ok_p[mode][regno]
	= loongarch_hard_regno_mode_ok_uncached (regno, (machine_mode) mode);
}

static void
loongarch_option_override_internal (struct loongarch_target *target,
				    struct gcc_options *opts,
				    struct gcc_options *opts_set)
{
  /* Handle options not covered by struct loongarch_target.  */
  loongarch_init_misc_options (opts, opts_set);

  /* Resolve the target struct.  */
  loongarch_init_target (target,
			 opts->x_la_opt_cpu_arch,
			 opts->x_la_opt_cpu_tune,
			 opts->x_la_opt_fpu,
			 opts->x_la_opt_simd,
			 opts->x_la_opt_abi_base,
			 opts->x_la_opt_abi_ext,
			 opts->x_la_opt_cmodel,
			 opts->x_la_opt_tls_dialect,
			 opts->x_la_isa_evolution,
			 opts_set->x_la_isa_evolution);

  loongarch_config_target (target, NULL, 0);

  /* Override some options according to the resolved target.  */
  loongarch_target_option_override (target, opts, opts_set);

  target_option_default_node = target_option_current_node
    = build_target_option_node (opts, opts_set);

  loongarch_reg_init ();
}

/* Remember the last target of loongarch_set_current_function.  */

static GTY(()) tree loongarch_previous_fndecl;

/* Restore or save the TREE_TARGET_GLOBALS from or to new_tree.
   Used by loongarch_set_current_function to
   make sure optab availability predicates are recomputed when necessary.  */

static void
loongarch_save_restore_target_globals (tree new_tree)
{
  if (TREE_TARGET_GLOBALS (new_tree))
    restore_target_globals (TREE_TARGET_GLOBALS (new_tree));
  else if (new_tree == target_option_default_node)
    restore_target_globals (&default_target_globals);
  else
    TREE_TARGET_GLOBALS (new_tree) = save_target_globals_default_opts ();
}

/* Implement TARGET_SET_CURRENT_FUNCTION.  */

static void
loongarch_set_current_function (tree fndecl)
{
  if (fndecl == loongarch_previous_fndecl)
    return;

  tree old_tree;
  if (loongarch_previous_fndecl == NULL_TREE)
    old_tree = target_option_current_node;
  else if (DECL_FUNCTION_SPECIFIC_TARGET (loongarch_previous_fndecl))
    old_tree = DECL_FUNCTION_SPECIFIC_TARGET (loongarch_previous_fndecl);
  else
    old_tree = target_option_default_node;

  if (fndecl == NULL_TREE)
    {
      if (old_tree != target_option_current_node)
	{
	  loongarch_previous_fndecl = NULL_TREE;
	  cl_target_option_restore (&global_options, &global_options_set,
				    TREE_TARGET_OPTION
				    (target_option_current_node));
	}
      return;
    }

  tree new_tree = DECL_FUNCTION_SPECIFIC_TARGET (fndecl);
  if (new_tree == NULL_TREE)
    new_tree = target_option_default_node;

  loongarch_previous_fndecl = fndecl;

  if (new_tree == old_tree)
    return;

  cl_target_option_restore (&global_options, &global_options_set,
			    TREE_TARGET_OPTION (new_tree));

  loongarch_reg_init ();

  loongarch_save_restore_target_globals (new_tree);
}



/* Implement TARGET_OPTION_OVERRIDE.  */

static void
loongarch_option_override (void)
{
  /* Global initializations.  */
  loongarch_global_init ();

  /* Setting up the target configuration.  */
  loongarch_option_override_internal (&la_target,
				      &global_options,
				      &global_options_set);

}

/* Implement TARGET_OPTION_SAVE.  */
static void
loongarch_option_save (struct cl_target_option *,
		       struct gcc_options *opts,
		       struct gcc_options *opts_set)
{
  loongarch_update_gcc_opt_status (&la_target, opts, opts_set);
}

/* Implement TARGET_OPTION_RESTORE.  */
static void
loongarch_option_restore (struct gcc_options *,
			  struct gcc_options *,
			  struct cl_target_option *ptr)
{
  la_target.cpu_arch = ptr->x_la_opt_cpu_arch;
  la_target.cpu_tune = ptr->x_la_opt_cpu_tune;

  la_target.isa.fpu = ptr->x_la_opt_fpu;
  la_target.isa.simd = ptr->x_la_opt_simd;
  la_target.isa.evolution = ptr->x_la_isa_evolution;

  la_target.cmodel = ptr->x_la_opt_cmodel;
  la_target.tls_dialect = ptr->x_la_opt_tls_dialect;
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

/* Generate or test for an insn that supports a constant permutation.  */

#define MAX_VECT_LEN 32

struct expand_vec_perm_d
{
  rtx target, op0, op1;
  unsigned char perm[MAX_VECT_LEN];
  machine_mode vmode;
  unsigned char nelt;
  bool one_vector_p;
  bool testing_p;
};

/* Construct (set target (vec_select op0 (parallel perm))) and
   return true if that's a valid instruction in the active ISA.  */

static bool
loongarch_expand_vselect (rtx target, rtx op0,
			  const unsigned char *perm, unsigned nelt,
			  bool testing_p)
{
  rtx rperm[MAX_VECT_LEN], x;
  rtx_insn *insn;
  unsigned i;

  for (i = 0; i < nelt; ++i)
    rperm[i] = GEN_INT (perm[i]);

  x = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (nelt, rperm));
  x = gen_rtx_VEC_SELECT (GET_MODE (target), op0, x);
  x = gen_rtx_SET (target, x);

  insn = emit_insn (x);
  if (recog_memoized (insn) < 0)
    {
      remove_insn (insn);
      return false;
    }

  if (testing_p)
      remove_insn (insn);
  return true;
}

/* Similar, but generate a vec_concat from op0 and op1 as well.  */

static bool
loongarch_expand_vselect_vconcat (rtx target, rtx op0, rtx op1,
				  const unsigned char *perm, unsigned nelt,
				  bool testing_p)
{
  machine_mode v2mode;
  rtx x;

  if (!GET_MODE_2XWIDER_MODE (GET_MODE (op0)).exists (&v2mode))
    return false;
  x = gen_rtx_VEC_CONCAT (v2mode, op0, op1);
  return loongarch_expand_vselect (target, x, perm, nelt, testing_p);
}

static tree
loongarch_handle_model_attribute (tree *node, tree name, tree arg, int,
				  bool *no_add_attrs)
{
  tree decl = *node;
  if (VAR_P (decl))
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

TARGET_GNU_ATTRIBUTES (loongarch_attribute_table,
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req,
       affects_type_identity, handler, exclude } */
  { "model", 1, 1, true, false, false, false,
    loongarch_handle_model_attribute, NULL }
});

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

static sbitmap
loongarch_get_separate_components (void)
{
  HOST_WIDE_INT offset;
  sbitmap components = sbitmap_alloc (FIRST_PSEUDO_REGISTER);
  bitmap_clear (components);
  offset = cfun->machine->frame.gp_sp_offset;

  /* The stack should be aligned to 16-bytes boundary, so we can make the use
     of ldptr instructions.  */
  gcc_assert (offset % UNITS_PER_WORD == 0);

  for (unsigned int regno = GP_REG_FIRST; regno <= GP_REG_LAST; regno++)
    if (BITSET_P (cfun->machine->frame.mask, regno - GP_REG_FIRST))
      {
	/* We can wrap general registers saved at [sp, sp + 32768) using the
	   ldptr/stptr instructions.  For large offsets a pseudo register
	   might be needed which cannot be created during the shrink
	   wrapping pass.

	   TODO: This may need a revise when we add LA32 as ldptr.w is not
	   guaranteed available by the manual.  */
	if (offset < 32768)
	  bitmap_set_bit (components, regno);

	offset -= UNITS_PER_WORD;
      }

  offset = cfun->machine->frame.fp_sp_offset;
  for (unsigned int regno = FP_REG_FIRST; regno <= FP_REG_LAST; regno++)
    if (BITSET_P (cfun->machine->frame.fmask, regno - FP_REG_FIRST))
      {
	/* We can only wrap FP registers with imm12 offsets.  For large
	   offsets a pseudo register might be needed which cannot be
	   created during the shrink wrapping pass.  */
	if (IMM12_OPERAND (offset))
	  bitmap_set_bit (components, regno);

	offset -= UNITS_PER_FP_REG;
      }

  /* Don't mess with the hard frame pointer.  */
  if (frame_pointer_needed)
    bitmap_clear_bit (components, HARD_FRAME_POINTER_REGNUM);

  bitmap_clear_bit (components, RETURN_ADDR_REGNUM);

  return components;
}

static sbitmap
loongarch_components_for_bb (basic_block bb)
{
  /* Registers are used in a bb if they are in the IN, GEN, or KILL sets.  */
  auto_bitmap used;
  bitmap_copy (used, DF_LIVE_IN (bb));
  bitmap_ior_into (used, &DF_LIVE_BB_INFO (bb)->gen);
  bitmap_ior_into (used, &DF_LIVE_BB_INFO (bb)->kill);

  sbitmap components = sbitmap_alloc (FIRST_PSEUDO_REGISTER);
  bitmap_clear (components);

  function_abi_aggregator callee_abis;
  rtx_insn *insn;
  FOR_BB_INSNS (bb, insn)
    if (CALL_P (insn))
      callee_abis.note_callee_abi (insn_callee_abi (insn));

  HARD_REG_SET extra_caller_saves =
    callee_abis.caller_save_regs (*crtl->abi);

  for (unsigned int regno = GP_REG_FIRST; regno <= GP_REG_LAST; regno++)
    if (!fixed_regs[regno]
	&& !crtl->abi->clobbers_full_reg_p (regno)
	&& (TEST_HARD_REG_BIT (extra_caller_saves, regno) ||
	    bitmap_bit_p (used, regno)))
      bitmap_set_bit (components, regno);

  for (unsigned int regno = FP_REG_FIRST; regno <= FP_REG_LAST; regno++)
    if (!fixed_regs[regno]
	&& !crtl->abi->clobbers_full_reg_p (regno)
	&& (TEST_HARD_REG_BIT (extra_caller_saves, regno) ||
	    bitmap_bit_p (used, regno)))
      bitmap_set_bit (components, regno);

  return components;
}

static void
loongarch_disqualify_components (sbitmap, edge, sbitmap, bool)
{
  /* Do nothing.  */
}

static void
loongarch_process_components (sbitmap components, loongarch_save_restore_fn fn)
{
  HOST_WIDE_INT offset = cfun->machine->frame.gp_sp_offset;

  for (unsigned int regno = GP_REG_FIRST; regno <= GP_REG_LAST; regno++)
    if (BITSET_P (cfun->machine->frame.mask, regno - GP_REG_FIRST))
      {
	if (bitmap_bit_p (components, regno))
	  loongarch_save_restore_reg (word_mode, regno, offset, fn);

	offset -= UNITS_PER_WORD;
      }

  offset = cfun->machine->frame.fp_sp_offset;
  machine_mode mode = TARGET_DOUBLE_FLOAT ? DFmode : SFmode;

  for (unsigned int regno = FP_REG_FIRST; regno <= FP_REG_LAST; regno++)
    if (BITSET_P (cfun->machine->frame.fmask, regno - FP_REG_FIRST))
      {
	if (bitmap_bit_p (components, regno))
	  loongarch_save_restore_reg (mode, regno, offset, fn);

	offset -= UNITS_PER_FP_REG;
      }
}

static void
loongarch_emit_prologue_components (sbitmap components)
{
  loongarch_process_components (components, loongarch_save_reg);
}

static void
loongarch_emit_epilogue_components (sbitmap components)
{
  loongarch_process_components (components, loongarch_restore_reg);
}

static void
loongarch_set_handled_components (sbitmap components)
{
    for (unsigned int regno = GP_REG_FIRST; regno <= GP_REG_LAST; regno++)
      if (bitmap_bit_p (components, regno))
	cfun->machine->reg_is_wrapped_separately[regno] = true;

    for (unsigned int regno = FP_REG_FIRST; regno <= FP_REG_LAST; regno++)
      if (bitmap_bit_p (components, regno))
	cfun->machine->reg_is_wrapped_separately[regno] = true;
}

/* Use the vshuf instruction to implement all 128-bit constant vector
   permuatation.  */

static bool
loongarch_try_expand_lsx_vshuf_const (struct expand_vec_perm_d *d)
{
  int i;
  rtx target, op0, op1, sel, tmp;
  rtx rperm[MAX_VECT_LEN];

  if (GET_MODE_SIZE (d->vmode) == 16)
    {
      target = d->target;
      op0 = d->op0;
      op1 = d->one_vector_p ? d->op0 : d->op1;

      if (GET_MODE (op0) != GET_MODE (op1)
	  || GET_MODE (op0) != GET_MODE (target))
	return false;

      if (d->testing_p)
	return true;

      for (i = 0; i < d->nelt; i += 1)
	  rperm[i] = GEN_INT (d->perm[i]);

      if (d->vmode == E_V2DFmode)
	{
	  sel = gen_rtx_CONST_VECTOR (E_V2DImode, gen_rtvec_v (d->nelt, rperm));
	  tmp = simplify_gen_subreg (E_V2DImode, d->target, d->vmode, 0);
	  emit_move_insn (tmp, sel);
	}
      else if (d->vmode == E_V4SFmode)
	{
	  sel = gen_rtx_CONST_VECTOR (E_V4SImode, gen_rtvec_v (d->nelt, rperm));
	  tmp = simplify_gen_subreg (E_V4SImode, d->target, d->vmode, 0);
	  emit_move_insn (tmp, sel);
	}
      else
	{
	  sel = gen_rtx_CONST_VECTOR (d->vmode, gen_rtvec_v (d->nelt, rperm));
	  emit_move_insn (d->target, sel);
	}

      switch (d->vmode)
	{
	case E_V2DFmode:
	  emit_insn (gen_lsx_vshuf_d_f (target, target, op1, op0));
	  break;
	case E_V2DImode:
	  emit_insn (gen_lsx_vshuf_d (target, target, op1, op0));
	  break;
	case E_V4SFmode:
	  emit_insn (gen_lsx_vshuf_w_f (target, target, op1, op0));
	  break;
	case E_V4SImode:
	  emit_insn (gen_lsx_vshuf_w (target, target, op1, op0));
	  break;
	case E_V8HImode:
	  emit_insn (gen_lsx_vshuf_h (target, target, op1, op0));
	  break;
	case E_V16QImode:
	  emit_insn (gen_lsx_vshuf_b (target, op1, op0, target));
	  break;
	default:
	  break;
	}

      return true;
    }
  return false;
}

/* Construct (set target (vec_select op0 (parallel selector))) and
   return true if that's a valid instruction in the active ISA.
   In fact, it matches the special constant vector with repeated
   4-element sets.  */

static bool
loongarch_is_imm_set_shuffle (struct expand_vec_perm_d *d)
{
  rtx x, elts[MAX_VECT_LEN];
  rtvec v;
  rtx_insn *insn;
  unsigned i;

  if (!ISA_HAS_LSX && !ISA_HAS_LASX)
    return false;

  for (i = 0; i < d->nelt; i++)
    elts[i] = GEN_INT (d->perm[i]);

  v = gen_rtvec_v (d->nelt, elts);
  x = gen_rtx_PARALLEL (VOIDmode, v);

  if (!loongarch_const_vector_shuffle_set_p (x, d->vmode))
    return false;

  if (d->testing_p)
    return true;

  x = gen_rtx_VEC_SELECT (d->vmode, d->op0, x);
  x = gen_rtx_SET (d->target, x);

  insn = emit_insn (x);
  if (recog_memoized (insn) < 0)
    {
      remove_insn (insn);
      return false;
    }
  return true;
}

static bool
loongarch_expand_vec_perm_even_odd (struct expand_vec_perm_d *);

/* Try to match and expand all kinds of 128-bit const vector permutation
   cases.  */

static bool
loongarch_expand_lsx_shuffle (struct expand_vec_perm_d *d)
{
  if (!ISA_HAS_LSX && GET_MODE_SIZE (d->vmode) != 16)
    return false;

  if (loongarch_is_imm_set_shuffle (d))
      return true;

  if (loongarch_expand_vec_perm_even_odd (d))
    return true;

  return loongarch_try_expand_lsx_vshuf_const (d);
}

/* Try to simplify a two vector permutation using 2 intra-lane interleave
   insns and cross-lane shuffle for 32-byte vectors.  */

static bool
loongarch_expand_vec_perm_interleave (struct expand_vec_perm_d *d)
{
  unsigned i, nelt;
  rtx t1,t2,t3;
  rtx (*gen_high) (rtx, rtx, rtx);
  rtx (*gen_low) (rtx, rtx, rtx);
  machine_mode mode = GET_MODE (d->target);

  if (d->one_vector_p)
    return false;
  if (ISA_HAS_LASX && GET_MODE_SIZE (d->vmode) == 32)
    ;
  else
    return false;

  nelt = d->nelt;
  if (d->perm[0] != 0 && d->perm[0] != nelt / 2)
    return false;
  for (i = 0; i < nelt; i += 2)
    if (d->perm[i] != d->perm[0] + i / 2
	|| d->perm[i + 1] != d->perm[0] + i / 2 + nelt)
      return false;

  if (d->testing_p)
    return true;

  switch (d->vmode)
    {
    case E_V32QImode:
      gen_high = gen_lasx_xvilvh_b;
      gen_low = gen_lasx_xvilvl_b;
      break;
    case E_V16HImode:
      gen_high = gen_lasx_xvilvh_h;
      gen_low = gen_lasx_xvilvl_h;
      break;
    case E_V8SImode:
      gen_high = gen_lasx_xvilvh_w;
      gen_low = gen_lasx_xvilvl_w;
      break;
    case E_V4DImode:
      gen_high = gen_lasx_xvilvh_d;
      gen_low = gen_lasx_xvilvl_d;
      break;
    case E_V8SFmode:
      gen_high = gen_lasx_xvilvh_w_f;
      gen_low = gen_lasx_xvilvl_w_f;
      break;
    case E_V4DFmode:
      gen_high = gen_lasx_xvilvh_d_f;
      gen_low = gen_lasx_xvilvl_d_f;
      break;
    default:
      gcc_unreachable ();
    }

  t1 = gen_reg_rtx (mode);
  t2 = gen_reg_rtx (mode);
  emit_insn (gen_high (t1, d->op0, d->op1));
  emit_insn (gen_low (t2, d->op0, d->op1));
  if (mode == V4DFmode || mode == V8SFmode)
    {
      t3 = gen_reg_rtx (V4DFmode);
      if (d->perm[0])
	emit_insn (gen_lasx_xvpermi_q_v4df (t3, gen_lowpart (V4DFmode, t1),
					    gen_lowpart (V4DFmode, t2),
					    GEN_INT (0x31)));
      else
	emit_insn (gen_lasx_xvpermi_q_v4df (t3, gen_lowpart (V4DFmode, t1),
					    gen_lowpart (V4DFmode, t2),
					    GEN_INT (0x20)));
    }
  else
    {
      t3 = gen_reg_rtx (V4DImode);
      if (d->perm[0])
	emit_insn (gen_lasx_xvpermi_q_v4di (t3, gen_lowpart (V4DImode, t1),
					    gen_lowpart (V4DImode, t2),
					    GEN_INT (0x31)));
      else
	emit_insn (gen_lasx_xvpermi_q_v4di (t3, gen_lowpart (V4DImode, t1),
					    gen_lowpart (V4DImode, t2),
					    GEN_INT (0x20)));
    }
  emit_move_insn (d->target, gen_lowpart (mode, t3));
  return true;
}

/* Implement 128-bit and 256-bit extract-even and extract-odd permutations.  */

static bool
loongarch_expand_vec_perm_even_odd_1 (struct expand_vec_perm_d *d, unsigned odd)
{
  rtx t1;
  machine_mode mode = GET_MODE (d->target);

  if (d->testing_p)
    return true;

  t1 = gen_reg_rtx (mode);

  switch (d->vmode)
    {
    /* 128 bit.  */
    case E_V2DFmode:
      if (odd)
	emit_insn (gen_lsx_vilvh_d_f (d->target, d->op0, d->op1));
      else
	emit_insn (gen_lsx_vilvl_d_f (d->target, d->op0, d->op1));
      break;

    case E_V2DImode:
      if (odd)
	emit_insn (gen_lsx_vilvh_d (d->target, d->op0, d->op1));
      else
	emit_insn (gen_lsx_vilvl_d (d->target, d->op0, d->op1));
      break;

    case E_V4SFmode:
      if (odd)
	emit_insn (gen_lsx_vpickod_w_f (d->target, d->op0, d->op1));
      else
	emit_insn (gen_lsx_vpickev_w_f (d->target, d->op0, d->op1));
      break;

    case E_V4SImode:
      if (odd)
	emit_insn (gen_lsx_vpickod_w (d->target, d->op0, d->op1));
      else
	emit_insn (gen_lsx_vpickev_w (d->target, d->op0, d->op1));
      break;

    case E_V8HImode:
      if (odd)
	emit_insn (gen_lsx_vpickod_h (d->target, d->op0, d->op1));
      else
	emit_insn (gen_lsx_vpickev_h (d->target, d->op0, d->op1));
      break;

    case E_V16QImode:
      if (odd)
	emit_insn (gen_lsx_vpickod_b (d->target, d->op0, d->op1));
      else
	emit_insn (gen_lsx_vpickev_b (d->target, d->op0, d->op1));
      break;

    /* 256 bit.  */
    case E_V4DFmode:
      /* Shuffle the lanes around into { 0 4 2 6 } and { 1 5 3 7 }.  */
      if (odd)
	emit_insn (gen_lasx_xvilvh_d_f (t1, d->op0, d->op1));
      else
	emit_insn (gen_lasx_xvilvl_d_f (t1, d->op0, d->op1));

      /* Shuffle within the 256-bit lanes to produce the result required.
	 { 0 2 4 6 } | { 1 3 5 7 }.  */
      emit_insn (gen_lasx_xvpermi_d_v4df (d->target, t1, GEN_INT (0xd8)));
      break;

    case E_V4DImode:
      if (odd)
	emit_insn (gen_lasx_xvilvh_d (t1, d->op0, d->op1));
      else
	emit_insn (gen_lasx_xvilvl_d (t1, d->op0, d->op1));

      emit_insn (gen_lasx_xvpermi_d_v4di (d->target, t1, GEN_INT (0xd8)));
      break;

    case E_V8SFmode:
      /* Shuffle the lanes around into:
	 { 0 2 8 a 4 6 c e } | { 1 3 9 b 5 7 d f }.  */
      if (odd)
	emit_insn (gen_lasx_xvpickod_w_f (t1, d->op0, d->op1));
      else
	emit_insn (gen_lasx_xvpickev_w_f (t1, d->op0, d->op1));

      /* Shuffle within the 256-bit lanes to produce the result required.
	 { 0 2 4 6 8 a c e } | { 1 3 5 7 9 b d f }.  */
      emit_insn (gen_lasx_xvpermi_d_v8sf (d->target, t1, GEN_INT (0xd8)));
      break;

    case E_V8SImode:
      if (odd)
	emit_insn (gen_lasx_xvpickod_w (t1, d->op0, d->op1));
      else
	emit_insn (gen_lasx_xvpickev_w (t1, d->op0, d->op1));

      emit_insn (gen_lasx_xvpermi_d_v8si (d->target, t1, GEN_INT (0xd8)));
      break;

    case E_V16HImode:
      if (odd)
	emit_insn (gen_lasx_xvpickod_h (t1, d->op0, d->op1));
      else
	emit_insn (gen_lasx_xvpickev_h (t1, d->op0, d->op1));

      emit_insn (gen_lasx_xvpermi_d_v16hi (d->target, t1, GEN_INT (0xd8)));
      break;

    case E_V32QImode:
      if (odd)
	emit_insn (gen_lasx_xvpickod_b (t1, d->op0, d->op1));
      else
	emit_insn (gen_lasx_xvpickev_b (t1, d->op0, d->op1));

      emit_insn (gen_lasx_xvpermi_d_v32qi (d->target, t1, GEN_INT (0xd8)));
      break;

    default:
      gcc_unreachable ();
    }

  return true;
}

/* Pattern match extract-even and extract-odd permutations.  */

static bool
loongarch_expand_vec_perm_even_odd (struct expand_vec_perm_d *d)
{
  unsigned i, odd, nelt = d->nelt;
  if (!ISA_HAS_LASX && !ISA_HAS_LSX)
    return false;

  odd = d->perm[0];
  if (odd != 0 && odd != 1)
    return false;

  for (i = 1; i < nelt; ++i)
    if (d->perm[i] != 2 * i + odd)
      return false;

  return loongarch_expand_vec_perm_even_odd_1 (d, odd);
}

static void
loongarch_expand_vec_interleave (rtx target, rtx op0, rtx op1, bool high_p)
{
  struct expand_vec_perm_d d;
  unsigned i, nelt, base;
  bool ok;

  d.target = target;
  d.op0 = op0;
  d.op1 = op1;
  d.vmode = GET_MODE (target);
  d.nelt = nelt = GET_MODE_NUNITS (d.vmode);
  d.one_vector_p = false;
  d.testing_p = false;

  base = high_p ? nelt / 2 : 0;
  for (i = 0; i < nelt / 2; ++i)
    {
      d.perm[i * 2] = i + base;
      d.perm[i * 2 + 1] = i + base + nelt;
    }

  ok = loongarch_expand_vec_perm_interleave (&d);
  gcc_assert (ok);
}

/* The loongarch lasx instructions xvmulwev and xvmulwod return the even or odd
   parts of the double sized result elements in the corresponding elements of
   the target register. That's NOT what the vec_widen_umult_lo/hi patterns are
   expected to do. We emulate the widening lo/hi multiplies with the even/odd
   versions followed by a vector merge.  */

void
loongarch_expand_vec_widen_hilo (rtx dest, rtx op1, rtx op2,
				 bool uns_p, bool high_p, const char *optab)
{
  machine_mode wmode = GET_MODE (dest);
  machine_mode mode = GET_MODE (op1);
  rtx t1, t2, t3;

  t1 = gen_reg_rtx (wmode);
  t2 = gen_reg_rtx (wmode);
  t3 = gen_reg_rtx (wmode);
  switch (mode)
    {
    case V16HImode:
      if (!strcmp (optab, "add"))
	{
	  if (!uns_p)
	    {
	      emit_insn (gen_lasx_xvaddwev_w_h (t1, op1, op2));
	      emit_insn (gen_lasx_xvaddwod_w_h (t2, op1, op2));
	    }
	  else
	    {
	      emit_insn (gen_lasx_xvaddwev_w_hu (t1, op1, op2));
	      emit_insn (gen_lasx_xvaddwod_w_hu (t2, op1, op2));
	    }
	}
      else if (!strcmp (optab, "mult"))
	{
	  if (!uns_p)
	    {
	      emit_insn (gen_lasx_xvmulwev_w_h (t1, op1, op2));
	      emit_insn (gen_lasx_xvmulwod_w_h (t2, op1, op2));
	    }
	  else
	    {
	      emit_insn (gen_lasx_xvmulwev_w_hu (t1, op1, op2));
	      emit_insn (gen_lasx_xvmulwod_w_hu (t2, op1, op2));
	    }
	}
      else if (!strcmp (optab, "sub"))
	{
	  if (!uns_p)
	    {
	      emit_insn (gen_lasx_xvsubwev_w_h (t1, op1, op2));
	      emit_insn (gen_lasx_xvsubwod_w_h (t2, op1, op2));
	    }
	  else
	    {
	      emit_insn (gen_lasx_xvsubwev_w_hu (t1, op1, op2));
	      emit_insn (gen_lasx_xvsubwod_w_hu (t2, op1, op2));
	    }
	}
      break;

    case V32QImode:
      if (!strcmp (optab, "add"))
	{
	  if (!uns_p)
	    {
	      emit_insn (gen_lasx_xvaddwev_h_b (t1, op1, op2));
	      emit_insn (gen_lasx_xvaddwod_h_b (t2, op1, op2));
	    }
	  else
	    {
	      emit_insn (gen_lasx_xvaddwev_h_bu (t1, op1, op2));
	      emit_insn (gen_lasx_xvaddwod_h_bu (t2, op1, op2));
	    }
	}
      else if (!strcmp (optab, "mult"))
	{
	  if (!uns_p)
	    {
	      emit_insn (gen_lasx_xvmulwev_h_b (t1, op1, op2));
	      emit_insn (gen_lasx_xvmulwod_h_b (t2, op1, op2));
	    }
	  else
	    {
	      emit_insn (gen_lasx_xvmulwev_h_bu (t1, op1, op2));
	      emit_insn (gen_lasx_xvmulwod_h_bu (t2, op1, op2));
	    }
	}
      else if (!strcmp (optab, "sub"))
	{
	  if (!uns_p)
	    {
	      emit_insn (gen_lasx_xvsubwev_h_b (t1, op1, op2));
	      emit_insn (gen_lasx_xvsubwod_h_b (t2, op1, op2));
	    }
	  else
	    {
	      emit_insn (gen_lasx_xvsubwev_h_bu (t1, op1, op2));
	      emit_insn (gen_lasx_xvsubwod_h_bu (t2, op1, op2));
	    }
	}
      break;

    default:
      gcc_unreachable ();
    }

  loongarch_expand_vec_interleave (t3, t1, t2, high_p);
  emit_move_insn (dest, gen_lowpart (wmode, t3));
}

/* Expand a variable vector permutation for LASX.  */

void
loongarch_expand_vec_perm_1 (rtx operands[])
{
  rtx target = operands[0];
  rtx op0 = operands[1];
  rtx op1 = operands[2];
  rtx mask = operands[3];

  bool one_operand_shuffle = rtx_equal_p (op0, op1);
  rtx t1 = NULL;
  rtx t2 = NULL;
  rtx t3, t4, t5, t6, vt = NULL;
  rtx vec[32] = {NULL};
  machine_mode mode = GET_MODE (op0);
  machine_mode maskmode = GET_MODE (mask);
  int w, i;

  /* Number of elements in the vector.  */
  w = GET_MODE_NUNITS (mode);

  rtx round_data[MAX_VECT_LEN];
  rtx round_reg, round_data_rtx;

  if (mode != E_V32QImode)
    {
      for (int i = 0; i < w; i += 1)
	{
	  round_data[i] = GEN_INT (0x1f);
	}

      if (mode == E_V4DFmode)
	{
	  round_data_rtx = gen_rtx_CONST_VECTOR (E_V4DImode,
						 gen_rtvec_v (w, round_data));
	  round_reg = gen_reg_rtx (E_V4DImode);
	}
      else if (mode == E_V8SFmode)
	{

	  round_data_rtx = gen_rtx_CONST_VECTOR (E_V8SImode,
						 gen_rtvec_v (w, round_data));
	  round_reg = gen_reg_rtx (E_V8SImode);
	}
      else
	{
	  round_data_rtx = gen_rtx_CONST_VECTOR (mode,
						 gen_rtvec_v (w, round_data));
	  round_reg = gen_reg_rtx (mode);
	}

      emit_move_insn (round_reg, round_data_rtx);
      switch (mode)
	{
	case E_V32QImode:
	  emit_insn (gen_andv32qi3 (mask, mask, round_reg));
	  break;
	case E_V16HImode:
	  emit_insn (gen_andv16hi3 (mask, mask, round_reg));
	  break;
	case E_V8SImode:
	case E_V8SFmode:
	  emit_insn (gen_andv8si3 (mask, mask, round_reg));
	  break;
	case E_V4DImode:
	case E_V4DFmode:
	  emit_insn (gen_andv4di3 (mask, mask, round_reg));
	  break;
	default:
	  gcc_unreachable ();
	  break;
	}
    }

  if (mode == V4DImode || mode == V4DFmode)
    {
      maskmode = mode = V8SImode;
      w = 8;
      t1 = gen_reg_rtx (maskmode);

      /* Replicate the low bits of the V4DImode mask into V8SImode:
	 mask = { A B C D }
	 t1 = { A A B B C C D D }.  */
      for (i = 0; i < w / 2; ++i)
	vec[i*2 + 1] = vec[i*2] = GEN_INT (i * 2);
      vt = gen_rtx_CONST_VECTOR (maskmode, gen_rtvec_v (w, vec));
      vt = force_reg (maskmode, vt);
      mask = gen_lowpart (maskmode, mask);
      emit_insn (gen_lasx_xvperm_w (t1, mask, vt));

      /* Multiply the shuffle indicies by two.  */
      t1 = expand_simple_binop (maskmode, PLUS, t1, t1, t1, 1,
				OPTAB_DIRECT);

      /* Add one to the odd shuffle indicies:
	 t1 = { A*2, A*2+1, B*2, B*2+1, ... }.  */
      for (i = 0; i < w / 2; ++i)
	{
	  vec[i * 2] = const0_rtx;
	  vec[i * 2 + 1] = const1_rtx;
	}
      vt = gen_rtx_CONST_VECTOR (maskmode, gen_rtvec_v (w, vec));
      vt = validize_mem (force_const_mem (maskmode, vt));
      t1 = expand_simple_binop (maskmode, PLUS, t1, vt, t1, 1,
				OPTAB_DIRECT);

      /* Continue as if V8SImode (resp.  V32QImode) was used initially.  */
      operands[3] = mask = t1;
      target = gen_reg_rtx (mode);
      op0 = gen_lowpart (mode, op0);
      op1 = gen_lowpart (mode, op1);
    }

  switch (mode)
    {
    case E_V8SImode:
      if (one_operand_shuffle)
	{
	  emit_insn (gen_lasx_xvperm_w (target, op0, mask));
	  if (target != operands[0])
	    emit_move_insn (operands[0],
			    gen_lowpart (GET_MODE (operands[0]), target));
	}
      else
	{
	  t1 = gen_reg_rtx (V8SImode);
	  t2 = gen_reg_rtx (V8SImode);
	  emit_insn (gen_lasx_xvperm_w (t1, op0, mask));
	  emit_insn (gen_lasx_xvperm_w (t2, op1, mask));
	  goto merge_two;
	}
      return;

    case E_V8SFmode:
      mask = gen_lowpart (V8SImode, mask);
      if (one_operand_shuffle)
	emit_insn (gen_lasx_xvperm_w_f (target, op0, mask));
      else
	{
	  t1 = gen_reg_rtx (V8SFmode);
	  t2 = gen_reg_rtx (V8SFmode);
	  emit_insn (gen_lasx_xvperm_w_f (t1, op0, mask));
	  emit_insn (gen_lasx_xvperm_w_f (t2, op1, mask));
	  goto merge_two;
	}
      return;

    case E_V16HImode:
      if (one_operand_shuffle)
	{
	  t1 = gen_reg_rtx (V16HImode);
	  t2 = gen_reg_rtx (V16HImode);
	  emit_insn (gen_lasx_xvpermi_d_v16hi (t1, op0, GEN_INT (0x44)));
	  emit_insn (gen_lasx_xvpermi_d_v16hi (t2, op0, GEN_INT (0xee)));
	  emit_insn (gen_lasx_xvshuf_h (target, mask, t2, t1));
	}
      else
	{
	  t1 = gen_reg_rtx (V16HImode);
	  t2 = gen_reg_rtx (V16HImode);
	  t3 = gen_reg_rtx (V16HImode);
	  t4 = gen_reg_rtx (V16HImode);
	  t5 = gen_reg_rtx (V16HImode);
	  t6 = gen_reg_rtx (V16HImode);
	  emit_insn (gen_lasx_xvpermi_d_v16hi (t3, op0, GEN_INT (0x44)));
	  emit_insn (gen_lasx_xvpermi_d_v16hi (t4, op0, GEN_INT (0xee)));
	  emit_insn (gen_lasx_xvshuf_h (t1, mask, t4, t3));
	  emit_insn (gen_lasx_xvpermi_d_v16hi (t5, op1, GEN_INT (0x44)));
	  emit_insn (gen_lasx_xvpermi_d_v16hi (t6, op1, GEN_INT (0xee)));
	  emit_insn (gen_lasx_xvshuf_h (t2, mask, t6, t5));
	  goto merge_two;
	}
      return;

    case E_V32QImode:
      if (one_operand_shuffle)
	{
	  t1 = gen_reg_rtx (V32QImode);
	  t2 = gen_reg_rtx (V32QImode);
	  emit_insn (gen_lasx_xvpermi_d_v32qi (t1, op0, GEN_INT (0x44)));
	  emit_insn (gen_lasx_xvpermi_d_v32qi (t2, op0, GEN_INT (0xee)));
	  emit_insn (gen_lasx_xvshuf_b (target, t2, t1, mask));
	}
      else
	{
	  t1 = gen_reg_rtx (V32QImode);
	  t2 = gen_reg_rtx (V32QImode);
	  t3 = gen_reg_rtx (V32QImode);
	  t4 = gen_reg_rtx (V32QImode);
	  t5 = gen_reg_rtx (V32QImode);
	  t6 = gen_reg_rtx (V32QImode);
	  emit_insn (gen_lasx_xvpermi_d_v32qi (t3, op0, GEN_INT (0x44)));
	  emit_insn (gen_lasx_xvpermi_d_v32qi (t4, op0, GEN_INT (0xee)));
	  emit_insn (gen_lasx_xvshuf_b (t1, t4, t3, mask));
	  emit_insn (gen_lasx_xvpermi_d_v32qi (t5, op1, GEN_INT (0x44)));
	  emit_insn (gen_lasx_xvpermi_d_v32qi (t6, op1, GEN_INT (0xee)));
	  emit_insn (gen_lasx_xvshuf_b (t2, t6, t5, mask));
	  goto merge_two;
	}
      return;

    default:
      gcc_assert (GET_MODE_SIZE (mode) == 32);
      break;
    }

merge_two:
  /* Then merge them together.  The key is whether any given control
     element contained a bit set that indicates the second word.  */
  rtx xops[6];
  mask = operands[3];
  vt = GEN_INT (w);
  vt = gen_const_vec_duplicate (maskmode, vt);
  vt = force_reg (maskmode, vt);
  mask = expand_simple_binop (maskmode, AND, mask, vt,
			      NULL_RTX, 0, OPTAB_DIRECT);
  if (GET_MODE (target) != mode)
    target = gen_reg_rtx (mode);
  xops[0] = target;
  xops[1] = gen_lowpart (mode, t2);
  xops[2] = gen_lowpart (mode, t1);
  xops[3] = gen_rtx_EQ (maskmode, mask, vt);
  xops[4] = mask;
  xops[5] = vt;

  loongarch_expand_vec_cond_expr (mode, maskmode, xops);
  if (target != operands[0])
    emit_move_insn (operands[0],
		    gen_lowpart (GET_MODE (operands[0]), target));
}

void
loongarch_expand_vec_perm (rtx target, rtx op0, rtx op1, rtx sel)
{
  machine_mode vmode = GET_MODE (target);
  machine_mode vimode = GET_MODE (sel);
  auto nelt = GET_MODE_NUNITS (vmode);
  auto round_reg = gen_reg_rtx (vimode);
  rtx round_data[MAX_VECT_LEN];

  for (int i = 0; i < nelt; i += 1)
    {
      round_data[i] = GEN_INT (0x1f);
    }

  rtx round_data_rtx = gen_rtx_CONST_VECTOR (vimode, gen_rtvec_v (nelt, round_data));
  emit_move_insn (round_reg, round_data_rtx);

  if (vmode != vimode)
    {
      target = lowpart_subreg (vimode, target, vmode);
      op0 = lowpart_subreg (vimode, op0, vmode);
      op1 = lowpart_subreg (vimode, op1, vmode);
    }

  switch (vmode)
    {
    case E_V16QImode:
      emit_insn (gen_andv16qi3 (sel, sel, round_reg));
      emit_insn (gen_lsx_vshuf_b (target, op1, op0, sel));
      break;
    case E_V2DFmode:
    case E_V2DImode:
      emit_insn (gen_andv2di3 (sel, sel, round_reg));
      emit_insn (gen_lsx_vshuf_d (target, sel, op1, op0));
      break;
    case E_V4SFmode:
    case E_V4SImode:
      emit_insn (gen_andv4si3 (sel, sel, round_reg));
      emit_insn (gen_lsx_vshuf_w (target, sel, op1, op0));
      break;
    case E_V8HImode:
      emit_insn (gen_andv8hi3 (sel, sel, round_reg));
      emit_insn (gen_lsx_vshuf_h (target, sel, op1, op0));
      break;
    default:
      break;
    }
}

/* Following are the assist function for const vector permutation support.  */
static bool
loongarch_is_quad_duplicate (struct expand_vec_perm_d *d)
{
  if (d->perm[0] >= d->nelt / 2)
    return false;

  bool result = true;
  unsigned char lhs = d->perm[0];
  unsigned char rhs = d->perm[d->nelt / 2];

  if ((rhs - lhs) != d->nelt / 2)
    return false;

  for (int i = 1; i < d->nelt; i += 1)
    {
      if ((i < d->nelt / 2) && (d->perm[i] != lhs))
	{
	  result = false;
	  break;
	}
      if ((i > d->nelt / 2) && (d->perm[i] != rhs))
	{
	  result = false;
	  break;
	}
    }

  return result;
}

static bool
loongarch_is_extraction_permutation (struct expand_vec_perm_d *d)
{
  bool result = true;
  unsigned char buf = d->perm[0];

  if (buf != 0 || buf != d->nelt)
    return false;

  for (int i = 0; i < d->nelt; i += 1)
    {
      if (buf != d->perm[i])
	{
	  result = false;
	  break;
	}
      buf += 1;
    }

  return result;
}

static bool
loongarch_is_lasx_lowpart_interleave (struct expand_vec_perm_d *d)
{
  bool result = true;
  unsigned char buf = 0;

  for (int i = 0;i < d->nelt; i += 2)
    {
      if (buf != d->perm[i])
	{
	  result = false;
	  break;
	}
      buf += 1;
    }

  if (result)
    {
      buf = d->nelt;
      for (int i = 1; i < d->nelt; i += 2)
	{
	  if (buf != d->perm[i])
	    {
	      result = false;
	      break;
	    }
	  buf += 1;
	}
    }

  return result;
}

static bool
loongarch_is_lasx_lowpart_interleave_2 (struct expand_vec_perm_d *d)
{
  if (d->vmode != E_V32QImode)
    return false;
  bool result = true;
  unsigned char buf = 0;

#define COMPARE_SELECTOR(INIT, BEGIN, END) \
  buf = INIT; \
  for (int i = BEGIN; i < END && result; i += 1) \
    { \
      if (buf != d->perm[i]) \
	{ \
	  result = false; \
	  break; \
	} \
      buf += 1; \
    }

  COMPARE_SELECTOR (0, 0, 8);
  COMPARE_SELECTOR (32, 8, 16);
  COMPARE_SELECTOR (8, 16, 24);
  COMPARE_SELECTOR (40, 24, 32);

#undef COMPARE_SELECTOR
  return result;
}

static bool
loongarch_is_lasx_highpart_interleave (expand_vec_perm_d *d)
{
  bool result = true;
  unsigned char buf = d->nelt / 2;

  for (int i = 0; i < d->nelt; i += 2)
    {
      if (buf != d->perm[i])
	{
	  result = false;
	  break;
	}
      buf += 1;
    }

  if (result)
    {
      buf = d->nelt + d->nelt / 2;
      for (int i = 1; i < d->nelt;i += 2)
	{
	  if (buf != d->perm[i])
	    {
	      result = false;
	      break;
	    }
	  buf += 1;
	}
    }

  return result;
}

static bool
loongarch_is_lasx_highpart_interleave_2 (struct expand_vec_perm_d *d)
{
  if (d->vmode != E_V32QImode)
    return false;

  bool result = true;
  unsigned char buf = 0;

#define COMPARE_SELECTOR(INIT, BEGIN, END) \
  buf = INIT; \
  for (int i = BEGIN; i < END && result; i += 1) \
    { \
      if (buf != d->perm[i]) \
	{ \
	  result = false; \
	  break; \
	} \
      buf += 1; \
    }

  COMPARE_SELECTOR (16, 0, 8);
  COMPARE_SELECTOR (48, 8, 16);
  COMPARE_SELECTOR (24, 16, 24);
  COMPARE_SELECTOR (56, 24, 32);

#undef COMPARE_SELECTOR
  return result;
}

static bool
loongarch_is_elem_duplicate (struct expand_vec_perm_d *d)
{
  bool result = true;
  unsigned char buf = d->perm[0];

  for (int i = 0; i < d->nelt; i += 1)
    {
      if (buf != d->perm[i])
	{
	  result = false;
	  break;
	}
    }

  return result;
}

/* In LASX, some permutation insn does not have the behavior that gcc expects
   when compiler wants to emit a vector permutation.

   1.  What GCC provides via vectorize_vec_perm_const ()'s paramater:
   When GCC wants to performs a vector permutation, it provides two op
   reigster, one target register, and a selector.
   In const vector permutation case, GCC provides selector as a char array
   that contains original value; in variable vector permuatation
   (performs via vec_perm<mode> insn template), it provides a vector register.
   We assume that nelt is the elements numbers inside single vector in current
   256bit vector mode.

   2.  What GCC expects to perform:
   Two op registers (op0, op1) will "combine" into a 512bit temp vector storage
   that has 2*nelt elements inside it; the low 256bit is op0, and high 256bit
   is op1, then the elements are indexed as below:
   0 ~ nelt - 1		nelt ~ 2 * nelt - 1
   |-------------------------|-------------------------|
   Low 256bit (op0)	High 256bit (op1)
   For example, the second element in op1 (V8SImode) will be indexed with 9.
   Selector is a vector that has the same mode and number of elements  with
   op0,op1 and target, it's look like this:
   0 ~ nelt - 1
   |-------------------------|
   256bit (selector)
   It describes which element from 512bit temp vector storage will fit into
   target's every element slot.
   GCC expects that every element in selector can be ANY indices of 512bit
   vector storage (Selector can pick literally any element from op0 and op1, and
   then fits into any place of target register). This is also what LSX 128bit
   vshuf.* instruction do similarly, so we can handle 128bit vector permutation
   by single instruction easily.

   3.  What LASX permutation instruction does:
   In short, it just execute two independent 128bit vector permuatation, and
   it's the reason that we need to do the jobs below.  We will explain it.
   op0, op1, target, and selector will be separate into high 128bit and low
   128bit, and do permutation as the description below:

   a) op0's low 128bit and op1's low 128bit "combines" into a 256bit temp
   vector storage (TVS1), elements are indexed as below:
   0 ~ nelt / 2 - 1	  nelt / 2 ~ nelt - 1
   |---------------------|---------------------| TVS1
   op0's low 128bit      op1's low 128bit
   op0's high 128bit and op1's high 128bit are "combined" into TVS2 in the
   same way.
   0 ~ nelt / 2 - 1	  nelt / 2 ~ nelt - 1
   |---------------------|---------------------| TVS2
   op0's high 128bit	op1's high 128bit
   b) Selector's low 128bit describes which elements from TVS1 will fit into
   target vector's low 128bit.  No TVS2 elements are allowed.
   c) Selector's high 128bit describes which elements from TVS2 will fit into
   target vector's high 128bit.  No TVS1 elements are allowed.

   As we can see, if we want to handle vector permutation correctly, we can
   achieve it in three ways:
   a) Modify selector's elements, to make sure that every elements can inform
   correct value that will put into target vector.
   b) Generate extra instruction before/after permutation instruction, for
   adjusting op vector or target vector, to make sure target vector's value is
   what GCC expects.
   c) Use other instructions to process op and put correct result into target.
   */

/* Implementation of constant vector permuatation.  This function identifies
   recognized pattern of permuation selector argument, and use one or more
   instruction (s) to finish the permutation job correctly.  For unsupported
   patterns, it will return false.  */

static bool
loongarch_expand_vec_perm_const (struct expand_vec_perm_d *d)
{
  bool flag = false;
  unsigned int i;
  unsigned char idx;
  rtx target, op0, op1, sel, tmp;
  rtx rperm[MAX_VECT_LEN];
  unsigned int remapped[MAX_VECT_LEN];
  unsigned char perm2[MAX_VECT_LEN];

  if (GET_MODE_SIZE (d->vmode) == 16)
    return loongarch_expand_lsx_shuffle (d);
  else
    {
      if (d->one_vector_p)
	{
	  /* Try interleave with alternating operands.  */
	  memcpy (perm2, d->perm, sizeof (perm2));
	  for (i = 1; i < d->nelt; i += 2)
	    perm2[i] += d->nelt;
	  if (loongarch_expand_vselect_vconcat (d->target, d->op0, d->op1,
						perm2, d->nelt, d->testing_p))
	    return true;
	}
      else
	{
	  if (loongarch_expand_vselect_vconcat (d->target, d->op0, d->op1,
						d->perm, d->nelt,
						d->testing_p))
	    return true;

	  /* Try again with swapped operands.  */
	  for (i = 0; i < d->nelt; ++i)
	    perm2[i] = (d->perm[i] + d->nelt) & (2 * d->nelt - 1);
	  if (loongarch_expand_vselect_vconcat (d->target, d->op1, d->op0,
						perm2, d->nelt, d->testing_p))
	    return true;
	}

      if (loongarch_is_imm_set_shuffle (d))
	return true;

      if (loongarch_expand_vec_perm_even_odd (d))
	return true;

      if (loongarch_is_lasx_lowpart_interleave (d)
	  || loongarch_is_lasx_lowpart_interleave_2 (d)
	  || loongarch_is_lasx_highpart_interleave (d)
	  || loongarch_is_lasx_highpart_interleave_2 (d))
	{
	  if (loongarch_expand_vec_perm_interleave (d))
	    return true;
	}

      if (loongarch_is_quad_duplicate (d))
	{
	  if (d->testing_p)
	    return true;
	  /* Selector example: E_V8SImode, { 0, 0, 0, 0, 4, 4, 4, 4 }.  */
	  for (i = 0; i < d->nelt; i += 1)
	    {
	      rperm[i] = GEN_INT (d->perm[0]);
	    }
	  /* Selector after: { 0, 0, 0, 0, 0, 0, 0, 0 }.  */
	  flag = true;
	  goto expand_perm_const_end;
	}

      if (loongarch_is_extraction_permutation (d))
	{
	  if (d->testing_p)
	    return true;
	  /* Selector sample: E_V8SImode, { 0, 1, 2, 3, 4, 5, 6, 7 }.  */
	  if (d->perm[0] == 0)
	    {
	      for (i = 0; i < d->nelt / 2; i += 1)
		{
		  remapped[i] = i;
		  remapped[i + d->nelt / 2] = i;
		}
	    }
	  else
	    {
	      /* { 8, 9, 10, 11, 12, 13, 14, 15 }.  */
	      for (i = 0; i < d->nelt / 2; i += 1)
		{
		  idx = i + d->nelt / 2;
		  remapped[i] = idx;
		  remapped[i + d->nelt / 2] = idx;
		}
	    }
	  /* Selector after: { 0, 1, 2, 3, 0, 1, 2, 3 }
	     { 8, 9, 10, 11, 8, 9, 10, 11 }  */

	  /* Convert remapped selector array to RTL array.  */
	  for (i = 0; i < d->nelt; i += 1)
	    {
	      rperm[i] = GEN_INT (remapped[i]);
	    }

	  flag = true;
	  goto expand_perm_const_end;
	}

      if (loongarch_is_elem_duplicate (d))
	{
	  if (d->testing_p)
	    return true;
	  /* Brocast single element (from op0 or op1) to all slot of target
	     register.
	     Selector sample:E_V8SImode, { 2, 2, 2, 2, 2, 2, 2, 2 }  */
	  rtx conv_op1 = simplify_gen_subreg (E_V4DImode, d->op1, d->vmode, 0);
	  rtx conv_op0 = simplify_gen_subreg (E_V4DImode, d->op0, d->vmode, 0);
	  rtx temp_reg = gen_reg_rtx (d->vmode);
	  rtx conv_temp = simplify_gen_subreg (E_V4DImode, temp_reg,
					       d->vmode, 0);
	  emit_move_insn (temp_reg, d->op0);

	  idx = d->perm[0];
	  /* We will use xvrepl128vei.* insn to achieve the result, but we need
	     to make the high/low 128bit has the same contents that contain the
	     value that we need to broardcast, because xvrepl128vei does the
	     broardcast job from every 128bit of source register to
	     corresponded part of target register! (A deep sigh.)  */
	  if (idx < d->nelt / 2)
	    {
	      emit_insn (gen_lasx_xvpermi_q_v4di (conv_temp, conv_temp,
						  conv_op0, GEN_INT (0x0)));
	    }
	  else if (idx >= d->nelt / 2 && idx < d->nelt)
	    {
	      emit_insn (gen_lasx_xvpermi_q_v4di (conv_temp, conv_temp,
						  conv_op0, GEN_INT (0x11)));
	      idx -= d->nelt / 2;
	    }
	  else if (idx >= d->nelt && idx < (d->nelt + d->nelt / 2))
	    {
	      emit_insn (gen_lasx_xvpermi_q_v4di (conv_temp, conv_temp,
						  conv_op1, GEN_INT (0x0)));
	    }
	  else if (idx >= (d->nelt + d->nelt / 2) && idx < d->nelt * 2)
	    {
	      emit_insn (gen_lasx_xvpermi_q_v4di (conv_temp, conv_temp,
						  conv_op1, GEN_INT (0x11)));
	      idx -= d->nelt / 2;
	    }

	  /* Then we can finally generate this insn.  */
	  switch (d->vmode)
	    {
	    case E_V4DImode:
	      emit_insn (gen_lasx_xvrepl128vei_d (d->target, temp_reg,
						  GEN_INT (idx)));
	      break;
	    case E_V4DFmode:
	      emit_insn (gen_lasx_xvrepl128vei_d_f (d->target, temp_reg,
						    GEN_INT (idx)));
	      break;
	    case E_V8SImode:
	      emit_insn (gen_lasx_xvrepl128vei_w (d->target, temp_reg,
						  GEN_INT (idx)));
	      break;
	    case E_V8SFmode:
	      emit_insn (gen_lasx_xvrepl128vei_w_f (d->target, temp_reg,
						    GEN_INT (idx)));
	      break;
	    case E_V16HImode:
	      emit_insn (gen_lasx_xvrepl128vei_h (d->target, temp_reg,
						  GEN_INT (idx)));
	      break;
	    case E_V32QImode:
	      emit_insn (gen_lasx_xvrepl128vei_b (d->target, temp_reg,
						  GEN_INT (idx)));
	      break;
	    default:
	      gcc_unreachable ();
	      break;
	    }

	  return true;
	}

expand_perm_const_end:
      if (flag)
	{
	  /* Copy selector vector from memory to vector register for later insn
	     gen function.
	     If vector's element in floating point value, we cannot fit
	     selector argument into insn gen function directly, because of the
	     insn template definition.  As a solution, generate a integral mode
	     subreg of target, then copy selector vector (that is in integral
	     mode) to this subreg.  */
	  switch (d->vmode)
	    {
	    case E_V4DFmode:
	      sel = gen_rtx_CONST_VECTOR (E_V4DImode, gen_rtvec_v (d->nelt,
								   rperm));
	      tmp = simplify_gen_subreg (E_V4DImode, d->target, d->vmode, 0);
	      emit_move_insn (tmp, sel);
	      break;
	    case E_V8SFmode:
	      sel = gen_rtx_CONST_VECTOR (E_V8SImode, gen_rtvec_v (d->nelt,
								   rperm));
	      tmp = simplify_gen_subreg (E_V8SImode, d->target, d->vmode, 0);
	      emit_move_insn (tmp, sel);
	      break;
	    default:
	      sel = gen_rtx_CONST_VECTOR (d->vmode, gen_rtvec_v (d->nelt,
								 rperm));
	      emit_move_insn (d->target, sel);
	      break;
	    }

	  target = d->target;
	  op0 = d->op0;
	  op1 = d->one_vector_p ? d->op0 : d->op1;

	  /* We FINALLY can generate xvshuf.* insn.  */
	  switch (d->vmode)
	    {
	    case E_V4DFmode:
	      emit_insn (gen_lasx_xvshuf_d_f (target, target, op1, op0));
	      break;
	    case E_V4DImode:
	      emit_insn (gen_lasx_xvshuf_d (target, target, op1, op0));
	      break;
	    case E_V8SFmode:
	      emit_insn (gen_lasx_xvshuf_w_f (target, target, op1, op0));
	      break;
	    case E_V8SImode:
	      emit_insn (gen_lasx_xvshuf_w (target, target, op1, op0));
	      break;
	    case E_V16HImode:
	      emit_insn (gen_lasx_xvshuf_h (target, target, op1, op0));
	      break;
	    case E_V32QImode:
	      emit_insn (gen_lasx_xvshuf_b (target, op1, op0, target));
	      break;
	    default:
	      gcc_unreachable ();
	      break;
	    }

	  return true;
	}
    }

  return false;
}

/* Implement TARGET_VECTORIZE_VEC_PERM_CONST.  */

static bool
loongarch_vectorize_vec_perm_const (machine_mode vmode, machine_mode op_mode,
				    rtx target, rtx op0, rtx op1,
				    const vec_perm_indices &sel)
{
  if (vmode != op_mode)
    return false;

  struct expand_vec_perm_d d;
  int i, nelt, which;
  unsigned char orig_perm[MAX_VECT_LEN];
  bool ok;

  d.target = target;
  if (op0)
    {
      rtx nop0 = force_reg (vmode, op0);
      if (op0 == op1)
	op1 = nop0;
      op0 = nop0;
    }
  if (op1)
    op1 = force_reg (vmode, op1);
  d.op0 = op0;
  d.op1 = op1;

  d.vmode = vmode;
  gcc_assert (VECTOR_MODE_P (vmode));
  d.nelt = nelt = GET_MODE_NUNITS (vmode);
  d.testing_p = !target;

  /* This is overly conservative, but ensures we don't get an
     uninitialized warning on ORIG_PERM.  */
  memset (orig_perm, 0, MAX_VECT_LEN);
  for (i = which = 0; i < nelt; ++i)
    {
      int ei = sel[i] & (2 * nelt - 1);
      which |= (ei < nelt ? 1 : 2);
      orig_perm[i] = ei;
    }
  memcpy (d.perm, orig_perm, MAX_VECT_LEN);

  switch (which)
    {
    default:
      gcc_unreachable ();

    case 3:
      d.one_vector_p = false;
      if (d.testing_p || !rtx_equal_p (d.op0, d.op1))
	break;
      /* FALLTHRU */

    case 2:
      for (i = 0; i < nelt; ++i)
	d.perm[i] &= nelt - 1;
      d.op0 = d.op1;
      d.one_vector_p = true;
      break;

    case 1:
      d.op1 = d.op0;
      d.one_vector_p = true;
      break;
    }

  // Do rounding for selector to avoid vshuf undefined behavior.
  for (i = 0; i < d.nelt; i += 1)
    {
      d.perm[i] %= (d.nelt * 2);
    }

  if (d.testing_p)
    {
      d.target = gen_raw_REG (d.vmode, LAST_VIRTUAL_REGISTER + 1);
      d.op1 = d.op0 = gen_raw_REG (d.vmode, LAST_VIRTUAL_REGISTER + 2);
      if (!d.one_vector_p)
	d.op1 = gen_raw_REG (d.vmode, LAST_VIRTUAL_REGISTER + 3);

      start_sequence ();
      ok = loongarch_expand_vec_perm_const (&d);
      end_sequence ();
      return ok;
    }

  ok = loongarch_expand_vec_perm_const (&d);

  /* If we were given a two-vector permutation which just happened to
     have both input vectors equal, we folded this into a one-vector
     permutation.  There are several loongson patterns that are matched
     via direct vec_select+vec_concat expansion, but we do not have
     support in loongarch_expand_vec_perm_const to guess the adjustment
     that should be made for a single operand.  Just try again with
     the original permutation.  */
  if (!ok && which == 3)
    {
      d.op0 = op0;
      d.op1 = op1;
      d.one_vector_p = false;
      memcpy (d.perm, orig_perm, MAX_VECT_LEN);
      ok = loongarch_expand_vec_perm_const (&d);
    }

  return ok;
}

static int
loongarch_cpu_sched_reassociation_width (struct loongarch_target *target,
					 unsigned int opc, machine_mode mode)
{
  /* unreferenced argument */
  (void) opc;

  switch (target->cpu_tune)
    {
    case TUNE_GENERIC:
    case TUNE_LOONGARCH64:
    case TUNE_LA464:
    case TUNE_LA664:
      /* Vector part.  */
      if (LSX_SUPPORTED_MODE_P (mode) || LASX_SUPPORTED_MODE_P (mode))
	{
	  /* Integer vector instructions execute in FP unit.
	     The width of integer/float-point vector instructions is 3.  */
	  return 3;
	}

      /* Scalar part.  */
      else if (INTEGRAL_MODE_P (mode))
	return 1;
      else if (FLOAT_MODE_P (mode))
	{
	  if (opc == PLUS_EXPR)
	    {
	      return 2;
	    }
	  return 4;
	}
      break;
    default:
      break;
    }

  /* default is 1 */
  return 1;
}

/* Implement TARGET_SCHED_REASSOCIATION_WIDTH.  */

static int
loongarch_sched_reassociation_width (unsigned int opc, machine_mode mode)
{
  return loongarch_cpu_sched_reassociation_width (&la_target, opc, mode);
}

/* Implement extract a scalar element from vecotr register */

void
loongarch_expand_vector_extract (rtx target, rtx vec, int elt)
{
  machine_mode mode = GET_MODE (vec);
  machine_mode inner_mode = GET_MODE_INNER (mode);
  rtx tmp;

  switch (mode)
    {
    case E_V8HImode:
    case E_V16QImode:
      break;

    case E_V32QImode:
      if (ISA_HAS_LASX)
	{
	  if (elt >= 16)
	    {
	      tmp = gen_reg_rtx (V32QImode);
	      emit_insn (gen_lasx_xvpermi_d_v32qi (tmp, vec, GEN_INT (0xe)));
	      loongarch_expand_vector_extract (target,
					       gen_lowpart (V16QImode, tmp),
					       elt & 15);
	    }
	  else
	    loongarch_expand_vector_extract (target,
					     gen_lowpart (V16QImode, vec),
					     elt & 15);
	  return;
	}
      break;

    case E_V16HImode:
      if (ISA_HAS_LASX)
	{
	  if (elt >= 8)
	    {
	      tmp = gen_reg_rtx (V16HImode);
	      emit_insn (gen_lasx_xvpermi_d_v16hi (tmp, vec, GEN_INT (0xe)));
	      loongarch_expand_vector_extract (target,
					       gen_lowpart (V8HImode, tmp),
					       elt & 7);
	    }
	  else
	    loongarch_expand_vector_extract (target,
					     gen_lowpart (V8HImode, vec),
					     elt & 7);
	  return;
	}
      break;

    default:
      break;
    }

  tmp = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (1, GEN_INT (elt)));
  tmp = gen_rtx_VEC_SELECT (inner_mode, vec, tmp);

  /* Let the rtl optimizers know about the zero extension performed.  */
  if (inner_mode == QImode || inner_mode == HImode)
    {
      tmp = gen_rtx_ZERO_EXTEND (SImode, tmp);
      target = gen_lowpart (SImode, target);
    }
  if (inner_mode == SImode || inner_mode == DImode)
    {
      tmp = gen_rtx_SIGN_EXTEND (inner_mode, tmp);
    }

  emit_insn (gen_rtx_SET (target, tmp));
}

/* Generate code to copy vector bits i / 2 ... i - 1 from vector SRC
   to bits 0 ... i / 2 - 1 of vector DEST, which has the same mode.
   The upper bits of DEST are undefined, though they shouldn't cause
   exceptions (some bits from src or all zeros are ok).  */

static void
emit_reduc_half (rtx dest, rtx src, int i)
{
  rtx tem, d = dest;
  switch (GET_MODE (src))
    {
    case E_V4SFmode:
      tem = gen_lsx_vbsrl_w_f (dest, src, GEN_INT (i == 128 ? 8 : 4));
      break;
    case E_V2DFmode:
      tem = gen_lsx_vbsrl_d_f (dest, src, GEN_INT (8));
      break;
    case E_V8SFmode:
      if (i == 256)
	tem = gen_lasx_xvpermi_d_v8sf (dest, src, GEN_INT (0xe));
      else
	tem = gen_lasx_xvshuf4i_w_f (dest, src,
				     GEN_INT (i == 128 ? 2 + (3 << 2) : 1));
      break;
    case E_V4DFmode:
      if (i == 256)
	tem = gen_lasx_xvpermi_d_v4df (dest, src, GEN_INT (0xe));
      else
	tem = gen_lasx_xvpermi_d_v4df (dest, src, const1_rtx);
      break;
    case E_V32QImode:
    case E_V16HImode:
    case E_V8SImode:
    case E_V4DImode:
      d = gen_reg_rtx (V4DImode);
      if (i == 256)
	tem = gen_lasx_xvpermi_d_v4di (d, gen_lowpart (V4DImode, src),
				       GEN_INT (0xe));
      else
	tem = gen_lasx_xvbsrl_d (d, gen_lowpart (V4DImode, src),
				 GEN_INT (i/16));
      break;
    case E_V16QImode:
    case E_V8HImode:
    case E_V4SImode:
    case E_V2DImode:
      d = gen_reg_rtx (V2DImode);
      tem = gen_lsx_vbsrl_d (d, gen_lowpart (V2DImode, src), GEN_INT (i/16));
      break;
    default:
      gcc_unreachable ();
    }
  emit_insn (tem);
  if (d != dest)
    emit_move_insn (dest, gen_lowpart (GET_MODE (dest), d));
}

/* Expand a vector reduction.  FN is the binary pattern to reduce;
   DEST is the destination; IN is the input vector.  */

void
loongarch_expand_vector_reduc (rtx (*fn) (rtx, rtx, rtx), rtx dest, rtx in)
{
  rtx half, dst, vec = in;
  machine_mode mode = GET_MODE (in);
  int i;

  for (i = GET_MODE_BITSIZE (mode);
       i > GET_MODE_UNIT_BITSIZE (mode);
       i >>= 1)
    {
      half = gen_reg_rtx (mode);
      emit_reduc_half (half, vec, i);
      if (i == GET_MODE_UNIT_BITSIZE (mode) * 2)
	dst = dest;
      else
	dst = gen_reg_rtx (mode);
      emit_insn (fn (dst, half, vec));
      vec = dst;
    }
}

/* Expand an integral vector unpack operation.  */

void
loongarch_expand_vec_unpack (rtx operands[2], bool unsigned_p)
{
  machine_mode imode = GET_MODE (operands[1]);
  rtx (*unpack) (rtx, rtx, rtx);
  rtx (*extend) (rtx, rtx);
  rtx (*cmpFunc) (rtx, rtx, rtx);
  rtx (*swap_hi_lo) (rtx, rtx, rtx, rtx);
  rtx tmp, dest;

  /* In LASX, only vec_unpacks_hi_<mode> requires expander.  */
  if (ISA_HAS_LASX && GET_MODE_SIZE (imode) == 32)
    {
      switch (imode)
	{
	case E_V8SImode:
	  if (unsigned_p)
	    extend = gen_vec_unpacku_lo_v8si;
	  else
	    extend = gen_vec_unpacks_lo_v8si;
	  swap_hi_lo = gen_lasx_xvpermi_q_v8si;
	  break;

	case E_V16HImode:
	  if (unsigned_p)
	    extend = gen_vec_unpacku_lo_v16hi;
	  else
	    extend = gen_vec_unpacks_lo_v16hi;
	  swap_hi_lo = gen_lasx_xvpermi_q_v16hi;
	  break;

	case E_V32QImode:
	  if (unsigned_p)
	    extend = gen_vec_unpacku_lo_v32qi;
	  else
	    extend = gen_vec_unpacks_lo_v32qi;
	  swap_hi_lo = gen_lasx_xvpermi_q_v32qi;
	  break;

	default:
	  gcc_unreachable ();
	  break;
	}

      tmp = gen_reg_rtx (imode);
      emit_insn (swap_hi_lo (tmp, tmp, operands[1], const1_rtx));
      emit_insn (extend (operands[0], tmp));
      return;
    }
  /* In LSX, only vec_unpacks_lo_<mode> requires expander.  */
  else if (ISA_HAS_LSX && !ISA_HAS_LASX)
    {
      switch (imode)
	{
	case E_V4SImode:
	  unpack = gen_lsx_vilvl_w;
	  cmpFunc = gen_lsx_vslt_w;
	  break;

	case E_V8HImode:
	  unpack = gen_lsx_vilvl_h;
	  cmpFunc = gen_lsx_vslt_h;
	  break;

	case E_V16QImode:
	  unpack = gen_lsx_vilvl_b;
	  cmpFunc = gen_lsx_vslt_b;
	  break;

	default:
	  gcc_unreachable ();
	  break;
	}

      if (!unsigned_p)
	{
	  /* Extract sign extention for each element comparing each element
	     with immediate zero.  */
	  tmp = gen_reg_rtx (imode);
	  emit_insn (cmpFunc (tmp, operands[1], CONST0_RTX (imode)));
	}
      else
	tmp = force_reg (imode, CONST0_RTX (imode));

      dest = gen_reg_rtx (imode);

      emit_insn (unpack (dest, operands[1], tmp));
      emit_move_insn (operands[0], gen_lowpart (GET_MODE (operands[0]), dest));
      return;
    }
  gcc_unreachable ();
}

/* Construct and return PARALLEL RTX with CONST_INTs for HIGH (high_p == TRUE)
   or LOW (high_p == FALSE) half of a vector for mode MODE.  */

rtx
loongarch_lsx_vec_parallel_const_half (machine_mode mode, bool high_p)
{
  int nunits = GET_MODE_NUNITS (mode);
  rtvec v = rtvec_alloc (nunits / 2);
  int base;
  int i;

  base = high_p ? nunits / 2 : 0;

  for (i = 0; i < nunits / 2; i++)
    RTVEC_ELT (v, i) = GEN_INT (base + i);

  return gen_rtx_PARALLEL (VOIDmode, v);
}

/* A subroutine of loongarch_expand_vec_init, match constant vector
   elements.  */

static inline bool
loongarch_constant_elt_p (rtx x)
{
  return CONST_INT_P (x) || GET_CODE (x) == CONST_DOUBLE;
}

rtx
loongarch_gen_const_int_vector_shuffle (machine_mode mode, int val)
{
  int nunits = GET_MODE_NUNITS (mode);
  int nsets = nunits / 4;
  rtx elts[MAX_VECT_LEN];
  int set = 0;
  int i, j;

  /* Generate a const_int vector replicating the same 4-element set
     from an immediate.  */
  for (j = 0; j < nsets; j++, set = 4 * j)
    for (i = 0; i < 4; i++)
      elts[set + i] = GEN_INT (set + ((val >> (2 * i)) & 0x3));

  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (nunits, elts));
}


/* Expand a vector initialization.  */

void
loongarch_expand_vector_group_init (rtx target, rtx vals)
{
  machine_mode vmode = GET_MODE (target);
  machine_mode half_mode = VOIDmode;
  rtx low = XVECEXP (vals, 0, 0);
  rtx high = XVECEXP (vals, 0, 1);

  switch (vmode)
    {
    case E_V32QImode:
      half_mode = V16QImode;
      break;
    case E_V16HImode:
      half_mode = V8HImode;
      break;
    case E_V8SImode:
      half_mode = V4SImode;
      break;
    case E_V4DImode:
      half_mode = V2DImode;
      break;
    case E_V8SFmode:
      half_mode = V4SFmode;
      break;
    case E_V4DFmode:
      half_mode = V2DFmode;
      break;
    default:
      gcc_unreachable ();
    }

  if (!register_operand (low, half_mode))
    low = force_reg (half_mode, low);
  if (!register_operand (high, half_mode))
    high = force_reg (half_mode, high);
  emit_insn (gen_rtx_SET (target,
			  gen_rtx_VEC_CONCAT (vmode, low, high)));
}

/* Expand initialization of a vector which has all same elements.  */

void
loongarch_expand_vector_init_same (rtx target, rtx vals, unsigned nvar)
{
  machine_mode vmode = GET_MODE (target);
  machine_mode imode = GET_MODE_INNER (vmode);
  rtx same = XVECEXP (vals, 0, 0);
  rtx temp;

  if (CONST_INT_P (same) && nvar == 0
      && loongarch_signed_immediate_p (INTVAL (same), 10, 0))
    {
      switch (vmode)
	{
	case E_V32QImode:
	case E_V16HImode:
	case E_V8SImode:
	case E_V4DImode:
	case E_V16QImode:
	case E_V8HImode:
	case E_V4SImode:
	case E_V2DImode:
	  temp = gen_rtx_CONST_VECTOR (vmode, XVEC (vals, 0));
	  emit_move_insn (target, temp);
	  return;
	default:
	  gcc_unreachable ();
	}
    }

  if (imode == GET_MODE (same))
    temp = same;
  else if (GET_MODE_SIZE (imode) >= UNITS_PER_WORD)
    {
      if (GET_CODE (same) == MEM)
	{
	  rtx reg_tmp = gen_reg_rtx (GET_MODE (same));
	  loongarch_emit_move (reg_tmp, same);
	  temp = simplify_gen_subreg (imode, reg_tmp, GET_MODE (reg_tmp), 0);
	}
      else
	temp = simplify_gen_subreg (imode, same, GET_MODE (same), 0);
    }
  else
    {
      if (GET_CODE (same) == MEM)
	{
	  rtx reg_tmp = gen_reg_rtx (GET_MODE (same));
	  loongarch_emit_move (reg_tmp, same);
	  temp = lowpart_subreg (imode, reg_tmp, GET_MODE (reg_tmp));
	}
      else
	temp = lowpart_subreg (imode, same, GET_MODE (same));
    }

  temp = force_reg (imode, temp);

  switch (vmode)
    {
    case E_V32QImode:
    case E_V16HImode:
    case E_V8SImode:
    case E_V4DImode:
    case E_V16QImode:
    case E_V8HImode:
    case E_V4SImode:
    case E_V2DImode:
      loongarch_emit_move (target, gen_rtx_VEC_DUPLICATE (vmode, temp));
      break;

    case E_V8SFmode:
      emit_insn (gen_lasx_xvreplve0_w_f_scalar (target, temp));
      break;

    case E_V4DFmode:
      emit_insn (gen_lasx_xvreplve0_d_f_scalar (target, temp));
      break;

    case E_V4SFmode:
      emit_insn (gen_lsx_vreplvei_w_f_scalar (target, temp));
      break;

    case E_V2DFmode:
      emit_insn (gen_lsx_vreplvei_d_f_scalar (target, temp));
      break;

    default:
      gcc_unreachable ();
    }
}

/* Expand a vector initialization.  */

void
loongarch_expand_vector_init (rtx target, rtx vals)
{
  machine_mode vmode = GET_MODE (target);
  machine_mode imode = GET_MODE_INNER (vmode);
  unsigned i, nelt = GET_MODE_NUNITS (vmode);
  /* VALS is divided into high and low half-part.  */
  /* Number of non constant elements in corresponding parts of VALS.  */
  unsigned nvar = 0, hi_nvar = 0, lo_nvar = 0;
  /* all_same : true if all elements of VALS are the same.
     hi_same : true if all elements of the high half-part are the same.
     lo_same : true if all elements of the low half-part are the same.
     half_same : true if the high half-part is the same as the low one.  */
  bool all_same = false, hi_same = true, lo_same = true, half_same = true;
  rtx val[32], val_hi[32], val_lo[16];
  rtx x, op0, op1;
  /* Copy one element of vals to per element of target vector.  */
  typedef rtx (*loongarch_vec_repl1_fn) (rtx, rtx);
  /* Copy two elements of vals to target vector.  */
  typedef rtx (*loongarch_vec_repl2_fn) (rtx, rtx, rtx);
  /* Insert scalar operands into the specified position of the vector.  */
  typedef rtx (*loongarch_vec_set_fn) (rtx, rtx, rtx);
  /* Copy 64bit lowpart to highpart.  */
  typedef rtx (*loongarch_vec_mirror_fn) (rtx, rtx, rtx);
  /* Merge lowpart and highpart into target.  */
  typedef rtx (*loongarch_vec_merge_fn) (rtx, rtx, rtx, rtx);

  loongarch_vec_repl1_fn loongarch_vec_repl1_128 = NULL,
			 loongarch_vec_repl1_256 = NULL;
  loongarch_vec_repl2_fn loongarch_vec_repl2_128 = NULL,
			 loongarch_vec_repl2_256 = NULL;
  loongarch_vec_set_fn loongarch_vec_set128 = NULL, loongarch_vec_set256 = NULL;
  loongarch_vec_mirror_fn loongarch_vec_mirror = NULL;
  loongarch_vec_merge_fn loongarch_lasx_vecinit_merge = NULL;
  machine_mode half_mode = VOIDmode;

  /* Check whether elements of each part are the same.  */
  for (i = 0; i < nelt / 2; ++i)
    {
      val_hi[i] = val_hi[i + nelt / 2] = val[i + nelt / 2]
	= XVECEXP (vals, 0, i + nelt / 2);
      val_lo[i] = val[i] = XVECEXP (vals, 0, i);
      if (!loongarch_constant_elt_p (val_hi[i]))
	hi_nvar++;
      if (!loongarch_constant_elt_p (val_lo[i]))
	lo_nvar++;
      if (i > 0 && !rtx_equal_p (val_hi[i], val_hi[0]))
	hi_same = false;
      if (i > 0 && !rtx_equal_p (val_lo[i], val_lo[0]))
	lo_same = false;
      if (!rtx_equal_p (val_hi[i], val_lo[i]))
	half_same = false;
    }

  /* If all elements are the same, set all_same true.  */
  if (hi_same && lo_same && half_same)
    all_same = true;

  nvar = hi_nvar + lo_nvar;

  switch (vmode)
    {
    case E_V32QImode:
      half_mode = E_V16QImode;
      loongarch_vec_set256 = gen_vec_setv32qi_internal;
      loongarch_vec_repl1_256 = gen_lasx_xvreplgr2vr_b;
      loongarch_lasx_vecinit_merge
	= half_same ? gen_lasx_xvpermi_q_v32qi : gen_lasx_vecinit_merge_v32qi;
      /* FALLTHRU.  */
    case E_V16QImode:
      loongarch_vec_set128 = gen_vec_setv16qi;
      loongarch_vec_repl1_128 = gen_lsx_vreplgr2vr_b;
      loongarch_vec_mirror = gen_lsx_vreplvei_mirror_b;
      break;

    case E_V16HImode:
      half_mode = E_V8HImode;
      loongarch_vec_set256 = gen_vec_setv16hi_internal;
      loongarch_vec_repl1_256 = gen_lasx_xvreplgr2vr_h;
      loongarch_lasx_vecinit_merge
	= half_same ? gen_lasx_xvpermi_q_v16hi : gen_lasx_vecinit_merge_v16hi;
      /* FALLTHRU.  */
    case E_V8HImode:
      loongarch_vec_set128 = gen_vec_setv8hi;
      loongarch_vec_repl1_128 = gen_lsx_vreplgr2vr_h;
      loongarch_vec_mirror = gen_lsx_vreplvei_mirror_h;
      break;

    case E_V8SImode:
      half_mode = V4SImode;
      loongarch_vec_set256 = gen_vec_setv8si;
      loongarch_vec_repl1_256 = gen_lasx_xvreplgr2vr_w;
      loongarch_lasx_vecinit_merge
	= half_same ? gen_lasx_xvpermi_q_v8si : gen_lasx_vecinit_merge_v8si;
      /* FALLTHRU.  */
    case E_V4SImode:
      loongarch_vec_set128 = gen_vec_setv4si;
      loongarch_vec_repl1_128 = gen_lsx_vreplgr2vr_w;
      loongarch_vec_mirror = gen_lsx_vreplvei_mirror_w;
      break;

    case E_V4DImode:
      half_mode = E_V2DImode;
      loongarch_vec_set256 = gen_vec_setv4di;
      loongarch_vec_repl1_256 = gen_lasx_xvreplgr2vr_d;
      loongarch_lasx_vecinit_merge
	= half_same ? gen_lasx_xvpermi_q_v4di : gen_lasx_vecinit_merge_v4di;
      /* FALLTHRU.  */
    case E_V2DImode:
      loongarch_vec_set128 = gen_vec_setv2di;
      loongarch_vec_repl1_128 = gen_lsx_vreplgr2vr_d;
      loongarch_vec_mirror = gen_lsx_vreplvei_mirror_d;
      break;

    case E_V8SFmode:
      half_mode = E_V4SFmode;
      loongarch_vec_set256 = gen_vec_setv8sf;
      loongarch_vec_repl1_128 = gen_lsx_vreplvei_w_f_scalar;
      loongarch_vec_repl2_256 = gen_lasx_xvilvl_w_f_internal;
      loongarch_lasx_vecinit_merge
	= half_same ? gen_lasx_xvpermi_q_v8sf : gen_lasx_vecinit_merge_v8sf;
      /* FALLTHRU.  */
    case E_V4SFmode:
      loongarch_vec_set128 = gen_vec_setv4sf;
      loongarch_vec_repl2_128 = gen_lsx_vilvl_w_f_internal;
      loongarch_vec_mirror = gen_lsx_vreplvei_mirror_w_f;
      break;

    case E_V4DFmode:
      half_mode = E_V2DFmode;
      loongarch_vec_set256 = gen_vec_setv4df;
      loongarch_vec_repl1_128 = gen_lsx_vreplvei_d_f_scalar;
      loongarch_vec_repl2_256 = gen_lasx_xvilvl_d_f_internal;
      loongarch_lasx_vecinit_merge
	= half_same ? gen_lasx_xvpermi_q_v4df : gen_lasx_vecinit_merge_v4df;
      /* FALLTHRU.  */
    case E_V2DFmode:
      loongarch_vec_set128 = gen_vec_setv2df;
      loongarch_vec_repl2_128 = gen_lsx_vilvl_d_f_internal;
      loongarch_vec_mirror = gen_lsx_vreplvei_mirror_d_f;
      break;

    default:
      gcc_unreachable ();
    }

  if (ISA_HAS_LASX && GET_MODE_SIZE (vmode) == 32)
    {
      /* If all elements are the same, just do a broadcost.  */
      if (all_same)
	loongarch_expand_vector_init_same (target, vals, nvar);
      else
	{
	  gcc_assert (nelt >= 4);

	  rtx target_hi, target_lo;
	  /* Write elements of high half-part in target directly.  */
	  target_hi = target;
	  target_lo = gen_reg_rtx (half_mode);

	  /* If all elements of high half-part are the same,
	     just do a broadcost.  Also applicable to low half-part.  */
	  if (hi_same)
	    {
	      rtx vtmp = gen_rtx_PARALLEL (vmode, gen_rtvec_v (nelt, val_hi));
	      loongarch_expand_vector_init_same (target_hi, vtmp, hi_nvar);
	    }
	  if (lo_same)
	    {
	      rtx vtmp
		= gen_rtx_PARALLEL (half_mode, gen_rtvec_v (nelt / 2, val_lo));
	      loongarch_expand_vector_init_same (target_lo, vtmp, lo_nvar);
	    }

	  for (i = 0; i < nelt / 2; ++i)
	    {
	      if (!hi_same)
		{
		  if (vmode == E_V8SFmode || vmode == E_V4DFmode)
		    {
		      /* Using xvilvl to load lowest 2 elements simultaneously
			 to reduce the number of instructions.  */
		      if (i == 1)
			{
			  op0 = force_reg (imode, val_hi[0]);
			  op1 = force_reg (imode, val_hi[1]);
			  emit_insn (
			    loongarch_vec_repl2_256 (target_hi, op0, op1));
			}
		      else if (i > 1)
			{
			  op0 = force_reg (imode, val_hi[i]);
			  emit_insn (
			    loongarch_vec_set256 (target_hi, op0, GEN_INT (i)));
			}
		    }
		  else
		    {
		      op0 = force_reg (imode, val_hi[i]);
		      /* Assign the lowest element of val_hi to all elements
			 of target_hi.  */
		      if (i == 0)
			{
			  emit_insn (loongarch_vec_repl1_256 (target_hi, op0));
			}
		      else if (!rtx_equal_p (val_hi[i], val_hi[0]))
			{
			  emit_insn (
			    loongarch_vec_set256 (target_hi, op0, GEN_INT (i)));
			}
		    }
		}
	      if (!lo_same && !half_same)
		{
		  op0 = force_reg (imode, val_lo[i]);
		  /* Assign the lowest element of val_lo to all elements
		     of target_lo.  */
		  if (i == 0)
		    {
		      emit_insn (loongarch_vec_repl1_128 (target_lo, op0));
		    }
		  else if (!rtx_equal_p (val_lo[i], val_lo[0]))
		    {
		      emit_insn (
			loongarch_vec_set128 (target_lo, op0, GEN_INT (i)));
		    }
		}
	    }
	  if (half_same)
	    {
	      emit_insn (loongarch_lasx_vecinit_merge (target, target_hi,
						       target_hi, const0_rtx));
	      return;
	    }
	  emit_insn (loongarch_lasx_vecinit_merge (target, target_hi, target_lo,
						   GEN_INT (0x20)));
	}
      return;
    }

  if (ISA_HAS_LSX)
    {
      if (all_same)
	loongarch_expand_vector_init_same (target, vals, nvar);
      else
	{
	  for (i = 0; i < nelt; ++i)
	    {
	      if (vmode == E_V4SFmode || vmode == E_V2DFmode)
		{
		  /* Using vilvl to load lowest 2 elements simultaneously to
		     reduce the number of instructions.  */
		  if (i == 1)
		    {
		      op0 = force_reg (imode, val[0]);
		      op1 = force_reg (imode, val[1]);
		      emit_insn (loongarch_vec_repl2_128 (target, op0, op1));
		    }
		  else if (i > 1)
		    {
		      op0 = force_reg (imode, val[i]);
		      emit_insn (
			loongarch_vec_set128 (target, op0, GEN_INT (i)));
		    }
		}
	      else
		{
		  if (half_same && i == nelt / 2)
		    {
		      emit_insn (
			loongarch_vec_mirror (target, target, const0_rtx));
		      return;
		    }
		  op0 = force_reg (imode, val[i]);
		  /* Assign the lowest element of val to all elements of
		     target.  */
		  if (i == 0)
		    {
		      emit_insn (loongarch_vec_repl1_128 (target, op0));
		    }
		  else if (!rtx_equal_p (val[i], val[0]))
		    {
		      emit_insn (
			loongarch_vec_set128 (target, op0, GEN_INT (i)));
		    }
		}
	    }
	}
      return;
    }

  /* Load constants from the pool, or whatever's handy.  */
  if (nvar == 0)
    {
      emit_move_insn (target, gen_rtx_CONST_VECTOR (vmode, XVEC (vals, 0)));
      return;
    }

  /* For two-part initialization, always use CONCAT.  */
  if (nelt == 2)
    {
      rtx op0 = force_reg (imode, val[0]);
      rtx op1 = force_reg (imode, val[1]);
      x = gen_rtx_VEC_CONCAT (vmode, op0, op1);
      emit_insn (gen_rtx_SET (target, x));
      return;
    }

  /* No LoongArch CPU supports vectors with more elements as at now.  */
  gcc_unreachable ();
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

/* Generate RTL for comparing CMP_OP0 and CMP_OP1 using condition COND and
   store the result -1 or 0 in DEST.  */

static void
loongarch_expand_lsx_cmp (rtx dest, enum rtx_code cond, rtx op0, rtx op1)
{
  machine_mode cmp_mode = GET_MODE (op0);
  bool negate = false;

  switch (cmp_mode)
    {
    case E_V16QImode:
    case E_V32QImode:
    case E_V8HImode:
    case E_V16HImode:
    case E_V4SImode:
    case E_V8SImode:
    case E_V2DImode:
    case E_V4DImode:
      switch (cond)
	{
	case NE:
	  if (!loongarch_const_vector_same_int_p (op1, cmp_mode, -16, 15))
	    op1 = force_reg (cmp_mode, op1);
	  cond = reverse_condition (cond);
	  negate = true;
	  break;
	case EQ:
	case LT:
	case LE:
	  if (!loongarch_const_vector_same_int_p (op1, cmp_mode, -16, 15))
	    op1 = force_reg (cmp_mode, op1);
	  break;
	case LTU:
	case LEU:
	  if (!loongarch_const_vector_same_int_p (op1, cmp_mode, 0, 31))
	    op1 = force_reg (cmp_mode, op1);
	  break;
	case GE:
	case GT:
	case GEU:
	case GTU:
	  /* Only supports reg-reg comparison.  */
	  if (!register_operand (op1, cmp_mode))
	    op1 = force_reg (cmp_mode, op1);
	  std::swap (op0, op1);
	  cond = swap_condition (cond);
	  break;
	default:
	  gcc_unreachable ();
	}
      loongarch_emit_binary (cond, dest, op0, op1);
      if (negate)
	emit_move_insn (dest, gen_rtx_NOT (GET_MODE (dest), dest));
      break;

    case E_V4SFmode:
    case E_V2DFmode:
    case E_V8SFmode:
    case E_V4DFmode:
      if (!register_operand (op1, cmp_mode))
	op1 = force_reg (cmp_mode, op1);
      loongarch_emit_binary (cond, dest, op0, op1);
      break;

    default:
      gcc_unreachable ();
      break;
    }
}

/* Expand VEC_COND_EXPR, where:
   MODE is mode of the result
   VIMODE equivalent integer mode
   OPERANDS operands of VEC_COND_EXPR.  */

void
loongarch_expand_vec_cond_expr (machine_mode mode, machine_mode vimode,
				rtx *operands)
{
  rtx cond = operands[3];
  rtx cmp_op0 = operands[4];
  rtx cmp_op1 = operands[5];
  rtx cmp_res = gen_reg_rtx (vimode);

  loongarch_expand_lsx_cmp (cmp_res, GET_CODE (cond), cmp_op0, cmp_op1);

  /* We handle the following cases:
     1) r = a CMP b ? -1 : 0
     2) r = a CMP b ? -1 : v
     3) r = a CMP b ?  v : 0
     4) r = a CMP b ? v1 : v2  */

  /* Case (1) above.  We only move the results.  */
  if (operands[1] == CONSTM1_RTX (vimode)
      && operands[2] == CONST0_RTX (vimode))
    emit_move_insn (operands[0], cmp_res);
  else
    {
      rtx src1 = gen_reg_rtx (vimode);
      rtx src2 = gen_reg_rtx (vimode);
      rtx mask = gen_reg_rtx (vimode);
      rtx bsel;

      /* Move the vector result to use it as a mask.  */
      emit_move_insn (mask, cmp_res);

      if (register_operand (operands[1], mode))
	{
	  rtx xop1 = operands[1];
	  if (mode != vimode)
	    {
	      xop1 = gen_reg_rtx (vimode);
	      emit_move_insn (xop1,
			      simplify_gen_subreg (vimode, operands[1],
						   mode, 0));
	    }
	  emit_move_insn (src1, xop1);
	}
      else
	{
	  gcc_assert (operands[1] == CONSTM1_RTX (vimode));
	  /* Case (2) if the below doesn't move the mask to src2.  */
	  emit_move_insn (src1, mask);
	}

      if (register_operand (operands[2], mode))
	{
	  rtx xop2 = operands[2];
	  if (mode != vimode)
	    {
	      xop2 = gen_reg_rtx (vimode);
	      emit_move_insn (xop2,
			      simplify_gen_subreg (vimode, operands[2],
						   mode, 0));
	    }
	  emit_move_insn (src2, xop2);
	}
      else
	{
	  gcc_assert (operands[2] == CONST0_RTX (mode));
	  /* Case (3) if the above didn't move the mask to src1.  */
	  emit_move_insn (src2, mask);
	}

      /* We deal with case (4) if the mask wasn't moved to either src1 or src2.
	 In any case, we eventually do vector mask-based copy.  */
      bsel = gen_rtx_IOR (vimode,
			  gen_rtx_AND (vimode,
				       gen_rtx_NOT (vimode, mask), src2),
			  gen_rtx_AND (vimode, mask, src1));
      /* The result is placed back to a register with the mask.  */
      emit_insn (gen_rtx_SET (mask, bsel));
      emit_move_insn (operands[0],
		      simplify_gen_subreg (mode, mask, vimode, 0));
    }
}

void
loongarch_expand_vec_cond_mask_expr (machine_mode mode, machine_mode vimode,
				    rtx *operands)
{
  rtx cmp_res = operands[3];

  /* We handle the following cases:
     1) r = a CMP b ? -1 : 0
     2) r = a CMP b ? -1 : v
     3) r = a CMP b ?  v : 0
     4) r = a CMP b ? v1 : v2  */

  /* Case (1) above.  We only move the results.  */
  if (operands[1] == CONSTM1_RTX (vimode)
      && operands[2] == CONST0_RTX (vimode))
    emit_move_insn (operands[0], cmp_res);
  else
    {
      rtx src1 = gen_reg_rtx (vimode);
      rtx src2 = gen_reg_rtx (vimode);
      rtx mask = gen_reg_rtx (vimode);
      rtx bsel;

      /* Move the vector result to use it as a mask.  */
      emit_move_insn (mask, cmp_res);

      if (register_operand (operands[1], mode))
	{
	  rtx xop1 = operands[1];
	  if (mode != vimode)
	    {
	      xop1 = gen_reg_rtx (vimode);
	      emit_move_insn (xop1,
			      simplify_gen_subreg (vimode, operands[1],
						   mode, 0));
	    }
	  emit_move_insn (src1, xop1);
	}
      else
	{
	  gcc_assert (operands[1] == CONSTM1_RTX (vimode));
	  /* Case (2) if the below doesn't move the mask to src2.  */
	  emit_move_insn (src1, mask);
	}

      if (register_operand (operands[2], mode))
	{
	  rtx xop2 = operands[2];
	  if (mode != vimode)
	    {
	      xop2 = gen_reg_rtx (vimode);
	      emit_move_insn (xop2,
			      simplify_gen_subreg (vimode, operands[2],
						   mode, 0));
	    }
	  emit_move_insn (src2, xop2);
	}
      else
	{
	  gcc_assert (operands[2] == CONST0_RTX (mode));
	  /* Case (3) if the above didn't move the mask to src1.  */
	  emit_move_insn (src2, mask);
	}

      /* We deal with case (4) if the mask wasn't moved to either src1 or src2.
	 In any case, we eventually do vector mask-based copy.  */
      bsel = gen_rtx_IOR (vimode,
			  gen_rtx_AND (vimode,
				       gen_rtx_NOT (vimode, mask), src2),
			  gen_rtx_AND (vimode, mask, src1));
      /* The result is placed back to a register with the mask.  */
      emit_insn (gen_rtx_SET (mask, bsel));
      emit_move_insn (operands[0], simplify_gen_subreg (mode, mask,
							vimode, 0));
    }
}

/* Expand integer vector comparison */
void
loongarch_expand_vec_cmp (rtx operands[])
{

  rtx_code code = GET_CODE (operands[1]);
  loongarch_expand_lsx_cmp (operands[0], code, operands[2], operands[3]);
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

/* A subroutine of loongarch_build_signbit_mask.  If VECT is true,
   then replicate the value for all elements of the vector
   register.  */

rtx
loongarch_build_const_vector (machine_mode mode, bool vect, rtx value)
{
  int i, n_elt;
  rtvec v;
  machine_mode scalar_mode;

  switch (mode)
    {
    case E_V32QImode:
    case E_V16QImode:
    case E_V32HImode:
    case E_V16HImode:
    case E_V8HImode:
    case E_V8SImode:
    case E_V4SImode:
    case E_V8DImode:
    case E_V4DImode:
    case E_V2DImode:
      gcc_assert (vect);
      /* FALLTHRU */
    case E_V8SFmode:
    case E_V4SFmode:
    case E_V8DFmode:
    case E_V4DFmode:
    case E_V2DFmode:
      n_elt = GET_MODE_NUNITS (mode);
      v = rtvec_alloc (n_elt);
      scalar_mode = GET_MODE_INNER (mode);

      RTVEC_ELT (v, 0) = value;

      for (i = 1; i < n_elt; ++i)
	RTVEC_ELT (v, i) = vect ? value : CONST0_RTX (scalar_mode);

      return gen_rtx_CONST_VECTOR (mode, v);

    default:
      gcc_unreachable ();
    }
}

/* Create a mask for the sign bit in MODE
   for an register.  If VECT is true, then replicate the mask for
   all elements of the vector register.  If INVERT is true, then create
   a mask excluding the sign bit.  */

rtx
loongarch_build_signbit_mask (machine_mode mode, bool vect, bool invert)
{
  machine_mode vec_mode, imode;
  wide_int w;
  rtx mask, v;

  switch (mode)
    {
    case E_V16SImode:
    case E_V16SFmode:
    case E_V8SImode:
    case E_V4SImode:
    case E_V8SFmode:
    case E_V4SFmode:
      vec_mode = mode;
      imode = SImode;
      break;

    case E_V8DImode:
    case E_V4DImode:
    case E_V2DImode:
    case E_V8DFmode:
    case E_V4DFmode:
    case E_V2DFmode:
      vec_mode = mode;
      imode = DImode;
      break;

    case E_TImode:
    case E_TFmode:
      vec_mode = VOIDmode;
      imode = TImode;
      break;

    default:
      gcc_unreachable ();
    }

  machine_mode inner_mode = GET_MODE_INNER (mode);
  w = wi::set_bit_in_zero (GET_MODE_BITSIZE (inner_mode) - 1,
			   GET_MODE_BITSIZE (inner_mode));
  if (invert)
    w = wi::bit_not (w);

  /* Force this value into the low part of a fp vector constant.  */
  mask = immed_wide_int_const (w, imode);
  mask = gen_lowpart (inner_mode, mask);

  if (vec_mode == VOIDmode)
    return force_reg (inner_mode, mask);

  v = loongarch_build_const_vector (vec_mode, vect, mask);
  return force_reg (vec_mode, v);
}

/* Use rsqrte instruction and Newton-Rhapson to compute the approximation of
   a single precision floating point [reciprocal] square root.  */

void loongarch_emit_swrsqrtsf (rtx res, rtx a, machine_mode mode, bool recip)
{
  rtx x0, e0, e1, e2, mhalf, monehalf;
  REAL_VALUE_TYPE r;
  int unspec;

  x0 = gen_reg_rtx (mode);
  e0 = gen_reg_rtx (mode);
  e1 = gen_reg_rtx (mode);
  e2 = gen_reg_rtx (mode);

  real_arithmetic (&r, ABS_EXPR, &dconsthalf, NULL);
  mhalf = const_double_from_real_value (r, SFmode);

  real_arithmetic (&r, PLUS_EXPR, &dconsthalf, &dconst1);
  monehalf = const_double_from_real_value (r, SFmode);
  unspec = UNSPEC_RSQRTE;

  if (VECTOR_MODE_P (mode))
    {
      mhalf = loongarch_build_const_vector (mode, true, mhalf);
      monehalf = loongarch_build_const_vector (mode, true, monehalf);
      unspec = GET_MODE_SIZE (mode) == 32 ? UNSPEC_LASX_XVFRSQRTE
					  : UNSPEC_LSX_VFRSQRTE;
    }

  /* rsqrt(a) =  rsqrte(a) * (1.5 - 0.5 * a * rsqrte(a) * rsqrte(a))
     sqrt(a)  =  a * rsqrte(a) * (1.5 - 0.5 * a * rsqrte(a) * rsqrte(a))  */

  a = force_reg (mode, a);

  /* x0 = rsqrt(a) estimate.  */
  emit_insn (gen_rtx_SET (x0, gen_rtx_UNSPEC (mode, gen_rtvec (1, a),
					      unspec)));

  /* If (a == 0.0) Filter out infinity to prevent NaN for sqrt(0.0).  */
  if (!recip)
    {
      rtx zero = force_reg (mode, CONST0_RTX (mode));

      if (VECTOR_MODE_P (mode))
	{
	  machine_mode imode = related_int_vector_mode (mode).require ();
	  rtx mask = gen_reg_rtx (imode);
	  emit_insn (gen_rtx_SET (mask, gen_rtx_NE (imode, a, zero)));
	  emit_insn (gen_rtx_SET (x0, gen_rtx_AND (mode, x0,
						   gen_lowpart (mode, mask))));
	}
      else
	{
	  rtx target = emit_conditional_move (x0, { GT, a, zero, mode },
					      x0, zero, mode, 0);
	  if (target != x0)
	    emit_move_insn (x0, target);
	}
    }

  /* e0 = x0 * a  */
  emit_insn (gen_rtx_SET (e0, gen_rtx_MULT (mode, x0, a)));
  /* e1 = e0 * x0  */
  emit_insn (gen_rtx_SET (e1, gen_rtx_MULT (mode, e0, x0)));

  /* e2 = 1.5 - e1 * 0.5  */
  mhalf = force_reg (mode, mhalf);
  monehalf = force_reg (mode, monehalf);
  emit_insn (gen_rtx_SET (e2, gen_rtx_FMA (mode,
					   gen_rtx_NEG (mode, e1),
							mhalf, monehalf)));

  if (recip)
    /* res = e2 * x0  */
    emit_insn (gen_rtx_SET (res, gen_rtx_MULT (mode, x0, e2)));
  else
    /* res = e2 * e0  */
    emit_insn (gen_rtx_SET (res, gen_rtx_MULT (mode, e2, e0)));
}

/* Use recipe instruction and Newton-Rhapson to compute the approximation of
   a single precision floating point divide.  */

void loongarch_emit_swdivsf (rtx res, rtx a, rtx b, machine_mode mode)
{
  rtx x0, e0, mtwo;
  REAL_VALUE_TYPE r;
  x0 = gen_reg_rtx (mode);
  e0 = gen_reg_rtx (mode);
  int unspec = UNSPEC_RECIPE;

  real_arithmetic (&r, ABS_EXPR, &dconst2, NULL);
  mtwo = const_double_from_real_value (r, SFmode);

  if (VECTOR_MODE_P (mode))
    {
      mtwo = loongarch_build_const_vector (mode, true, mtwo);
      unspec = GET_MODE_SIZE (mode) == 32 ? UNSPEC_LASX_XVFRECIPE
					  : UNSPEC_LSX_VFRECIPE;
    }

  mtwo = force_reg (mode, mtwo);

  /* a / b = a * recipe(b) * (2.0 - b * recipe(b))  */

  /* x0 = 1./b estimate.  */
  emit_insn (gen_rtx_SET (x0, gen_rtx_UNSPEC (mode, gen_rtvec (1, b),
					      unspec)));
  /* e0 = 2.0 - b * x0.  */
  emit_insn (gen_rtx_SET (e0, gen_rtx_FMA (mode,
					   gen_rtx_NEG (mode, b), x0, mtwo)));

  if (a != CONST1_RTX (mode))
    {
      rtx e1 = gen_reg_rtx (mode);
      /* e1 = a * x0.  */
      emit_insn (gen_rtx_SET (e1, gen_rtx_MULT (mode, a, x0)));
      /* res = e0 * e1.  */
      emit_insn (gen_rtx_SET (res, gen_rtx_MULT (mode, e0, e1)));
    }
  else
    {
      /* res = e0 * x0.  */
      emit_insn (gen_rtx_SET (res, gen_rtx_MULT (mode, e0, x0)));
    }
}

static bool
loongarch_builtin_support_vector_misalignment (machine_mode mode,
					       const_tree type,
					       int misalignment,
					       bool is_packed)
{
  if ((ISA_HAS_LSX || ISA_HAS_LASX) && STRICT_ALIGNMENT)
    {
      if (optab_handler (movmisalign_optab, mode) == CODE_FOR_nothing)
	return false;
      if (misalignment == -1)
	return false;
    }
  return default_builtin_support_vector_misalignment (mode, type, misalignment,
						      is_packed);
}

/* Implement TARGET_C_MODE_FOR_FLOATING_TYPE.  Return TFmode or DFmode
   for TI_LONG_DOUBLE_TYPE which is for long double type, go with the
   default one for the others.  */

static machine_mode
loongarch_c_mode_for_floating_type (enum tree_index ti)
{
  if (ti == TI_LONG_DOUBLE_TYPE)
    return TARGET_64BIT ? TFmode : DFmode;
  return default_mode_for_floating_type (ti);
}

static bool
use_rsqrt_p (void)
{
  return (flag_finite_math_only
	  && !flag_trapping_math
	  && flag_unsafe_math_optimizations);
}

/* Implement the TARGET_OPTAB_SUPPORTED_P hook.  */

static bool
loongarch_optab_supported_p (int op, machine_mode, machine_mode,
			     optimization_type opt_type)
{
  switch (op)
    {
    case rsqrt_optab:
      return opt_type == OPTIMIZE_FOR_SPEED && use_rsqrt_p ();

    default:
      return true;
    }
}

/* If -fverbose-asm, dump some info for debugging.  */
static void
loongarch_asm_code_end (void)
{
#define DUMP_FEATURE(PRED) \
  fprintf (asm_out_file, "%s %s: %s\n", ASM_COMMENT_START, #PRED, \
	   (PRED) ? "enabled" : "disabled")

  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\n%s CPU: %s\n", ASM_COMMENT_START,
	       loongarch_arch_strings[la_target.cpu_arch]);
      fprintf (asm_out_file, "%s Tune: %s\n", ASM_COMMENT_START,
	       loongarch_tune_strings[la_target.cpu_tune]);
      fprintf (asm_out_file, "%s Base ISA: %s\n", ASM_COMMENT_START,
	       loongarch_isa_base_strings [la_target.isa.base]);
      DUMP_FEATURE (ISA_HAS_FRECIPE);
      DUMP_FEATURE (ISA_HAS_DIV32);
      DUMP_FEATURE (ISA_HAS_LAM_BH);
      DUMP_FEATURE (ISA_HAS_LAMCAS);
      DUMP_FEATURE (ISA_HAS_LD_SEQ_SA);
    }

  fputs ("\n\n", asm_out_file);
#undef DUMP_FEATURE
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
#undef TARGET_OPTION_SAVE
#define TARGET_OPTION_SAVE loongarch_option_save
#undef TARGET_OPTION_RESTORE
#define TARGET_OPTION_RESTORE loongarch_option_restore

#undef TARGET_SET_CURRENT_FUNCTION
#define TARGET_SET_CURRENT_FUNCTION loongarch_set_current_function

#undef TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS loongarch_legitimize_address

#undef TARGET_ASM_SELECT_RTX_SECTION
#define TARGET_ASM_SELECT_RTX_SECTION loongarch_select_rtx_section
#undef TARGET_ASM_FUNCTION_RODATA_SECTION
#define TARGET_ASM_FUNCTION_RODATA_SECTION loongarch_function_rodata_section

#undef TARGET_ASM_CODE_END
#define TARGET_ASM_CODE_END loongarch_asm_code_end

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
#undef TARGET_INSN_COST
#define TARGET_INSN_COST loongarch_insn_cost
#undef TARGET_VECTORIZE_BUILTIN_VECTORIZATION_COST
#define TARGET_VECTORIZE_BUILTIN_VECTORIZATION_COST \
  loongarch_builtin_vectorization_cost
#undef TARGET_VECTORIZE_CREATE_COSTS
#define TARGET_VECTORIZE_CREATE_COSTS loongarch_vectorize_create_costs


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

#undef TARGET_VECTOR_MODE_SUPPORTED_P
#define TARGET_VECTOR_MODE_SUPPORTED_P loongarch_vector_mode_supported_p

#undef TARGET_SCALAR_MODE_SUPPORTED_P
#define TARGET_SCALAR_MODE_SUPPORTED_P loongarch_scalar_mode_supported_p

#undef TARGET_VECTORIZE_PREFERRED_SIMD_MODE
#define TARGET_VECTORIZE_PREFERRED_SIMD_MODE loongarch_preferred_simd_mode

#undef TARGET_VECTORIZE_AUTOVECTORIZE_VECTOR_MODES
#define TARGET_VECTORIZE_AUTOVECTORIZE_VECTOR_MODES \
  loongarch_autovectorize_vector_modes

#undef TARGET_OPTAB_SUPPORTED_P
#define TARGET_OPTAB_SUPPORTED_P loongarch_optab_supported_p

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
#undef TARGET_VECTORIZE_VEC_PERM_CONST
#define TARGET_VECTORIZE_VEC_PERM_CONST loongarch_vectorize_vec_perm_const

#undef TARGET_SCHED_REASSOCIATION_WIDTH
#define TARGET_SCHED_REASSOCIATION_WIDTH loongarch_sched_reassociation_width

#undef TARGET_ATOMIC_ASSIGN_EXPAND_FENV
#define TARGET_ATOMIC_ASSIGN_EXPAND_FENV loongarch_atomic_assign_expand_fenv

#undef TARGET_CALL_FUSAGE_CONTAINS_NON_CALLEE_CLOBBERS
#define TARGET_CALL_FUSAGE_CONTAINS_NON_CALLEE_CLOBBERS true

#undef TARGET_HARD_REGNO_NREGS
#define TARGET_HARD_REGNO_NREGS loongarch_hard_regno_nregs
#undef TARGET_HARD_REGNO_MODE_OK
#define TARGET_HARD_REGNO_MODE_OK loongarch_hard_regno_mode_ok

#undef TARGET_MODES_TIEABLE_P
#define TARGET_MODES_TIEABLE_P loongarch_modes_tieable_p

#undef TARGET_HARD_REGNO_CALL_PART_CLOBBERED
#define TARGET_HARD_REGNO_CALL_PART_CLOBBERED \
  loongarch_hard_regno_call_part_clobbered

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

#undef TARGET_IRA_CHANGE_PSEUDO_ALLOCNO_CLASS
#define TARGET_IRA_CHANGE_PSEUDO_ALLOCNO_CLASS \
  loongarch_ira_change_pseudo_allocno_class

#undef  TARGET_HAVE_SPECULATION_SAFE_VALUE
#define TARGET_HAVE_SPECULATION_SAFE_VALUE speculation_safe_value_not_needed

#undef  TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE loongarch_attribute_table

#undef  TARGET_USE_ANCHORS_FOR_SYMBOL_P
#define TARGET_USE_ANCHORS_FOR_SYMBOL_P loongarch_use_anchors_for_symbol_p

#undef TARGET_ASAN_SHADOW_OFFSET
#define TARGET_ASAN_SHADOW_OFFSET loongarch_asan_shadow_offset

#undef TARGET_SHRINK_WRAP_GET_SEPARATE_COMPONENTS
#define TARGET_SHRINK_WRAP_GET_SEPARATE_COMPONENTS \
  loongarch_get_separate_components

#undef TARGET_SHRINK_WRAP_COMPONENTS_FOR_BB
#define TARGET_SHRINK_WRAP_COMPONENTS_FOR_BB loongarch_components_for_bb

#undef TARGET_SHRINK_WRAP_DISQUALIFY_COMPONENTS
#define TARGET_SHRINK_WRAP_DISQUALIFY_COMPONENTS \
  loongarch_disqualify_components

#undef TARGET_SHRINK_WRAP_EMIT_PROLOGUE_COMPONENTS
#define TARGET_SHRINK_WRAP_EMIT_PROLOGUE_COMPONENTS \
  loongarch_emit_prologue_components

#undef TARGET_SHRINK_WRAP_EMIT_EPILOGUE_COMPONENTS
#define TARGET_SHRINK_WRAP_EMIT_EPILOGUE_COMPONENTS \
  loongarch_emit_epilogue_components

#undef TARGET_SHRINK_WRAP_SET_HANDLED_COMPONENTS
#define TARGET_SHRINK_WRAP_SET_HANDLED_COMPONENTS \
  loongarch_set_handled_components

#undef TARGET_VECTORIZE_SUPPORT_VECTOR_MISALIGNMENT
#define TARGET_VECTORIZE_SUPPORT_VECTOR_MISALIGNMENT \
  loongarch_builtin_support_vector_misalignment

#undef TARGET_C_MODE_FOR_FLOATING_TYPE
#define TARGET_C_MODE_FOR_FLOATING_TYPE loongarch_c_mode_for_floating_type

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-loongarch.h"
