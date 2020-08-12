/* Subroutines used for code generation for eBPF.
   Copyright (C) 2019-2020 Free Software Foundation, Inc.

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
#include "tm.h"
#include "rtl.h"
#include "regs.h"
#include "insn-config.h"
#include "insn-attr.h"
#include "recog.h"
#include "output.h"
#include "alias.h"
#include "tree.h"
#include "stringpool.h"
#include "attribs.h"
#include "varasm.h"
#include "stor-layout.h"
#include "calls.h"
#include "function.h"
#include "explow.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "reload.h"
#include "tm_p.h"
#include "target.h"
#include "target-def.h"
#include "basic-block.h"
#include "expr.h"
#include "optabs.h"
#include "bitmap.h"
#include "df.h"
#include "c-family/c-common.h"
#include "diagnostic.h"
#include "builtins.h"
#include "predict.h"
#include "langhooks.h"

/* Per-function machine data.  */
struct GTY(()) machine_function
{
  /* Number of bytes saved on the stack for local variables.  */
  int local_vars_size;

  /* Number of bytes saved on the stack for callee-saved
     registers.  */
  int callee_saved_reg_size;
};

/* Data structures for the eBPF specific built-ins.  */

/* Maximum number of arguments taken by a builtin function, plus
   one.  */
#define BPF_BUILTIN_MAX_ARGS 5

enum bpf_builtins
{
  BPF_BUILTIN_UNUSED = 0,
  /* Built-ins for kernel helpers.  */
#define DEF_HELPER(V,D,N,T) BPF_BUILTIN_HELPER_##D,
#  include "bpf-helpers.def"
#undef DEF_HELPER
  BPF_BUILTIN_HELPER_MAX,
  /* Built-ins for non-generic loads and stores.  */
  BPF_BUILTIN_LOAD_BYTE = BPF_BUILTIN_HELPER_MAX,
  BPF_BUILTIN_LOAD_HALF,
  BPF_BUILTIN_LOAD_WORD,
  BPF_BUILTIN_MAX,
};

/* This table is indexed by an enum bpf_builtin.  */
static const char *bpf_helper_names[] =
{
  NULL,
#define DEF_HELPER(V,D,N,T) #N,
#  include "bpf-helpers.def"
#undef DEF_HELPER
  NULL,
  NULL,
  NULL,
  NULL
};

/* Return the builtin code corresponding to the kernel helper builtin
   __builtin_NAME, or 0 if the name doesn't correspond to a kernel
   helper builtin.  */

static inline int
bpf_helper_code (const char *name)
{
  int i;

  for (i = 1; i < BPF_BUILTIN_HELPER_MAX; ++i)
    if (strcmp (name, bpf_helper_names[i]) == 0)
      return i;

  return 0;
}

static GTY (()) tree bpf_builtins[(int) BPF_BUILTIN_MAX];

/* Initialize the per-function machine status.  */

static struct machine_function *
bpf_init_machine_status (void)
{
  /* Note this initializes all fields to 0, which is just OK for
     us.  */
  return ggc_cleared_alloc<machine_function> ();
}

/* Override options and do some other initialization.  */

static void
bpf_option_override (void)
{
  /* Set the initializer for the per-function status structure.  */
  init_machine_status = bpf_init_machine_status;
}

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE bpf_option_override

/* Define target-specific CPP macros.  This function in used in the
   definition of TARGET_CPU_CPP_BUILTINS in bpf.h */

#define builtin_define(TXT) cpp_define (pfile, TXT)

void
bpf_target_macros (cpp_reader *pfile)
{
  builtin_define ("__BPF__");
  
  if (TARGET_BIG_ENDIAN)
    builtin_define ("__BPF_BIG_ENDIAN__");
  else
    builtin_define ("__BPF_LITTLE_ENDIAN__");

  /* Define BPF_KERNEL_VERSION_CODE */
  {
    const char *version_code;
    char *kernel_version_code;

    switch (bpf_kernel)
      {
      case LINUX_V4_0: version_code = "0x40000"; break;
      case LINUX_V4_1: version_code = "0x40100"; break;
      case LINUX_V4_2: version_code = "0x40200"; break;
      case LINUX_V4_3: version_code = "0x40300"; break;
      case LINUX_V4_4: version_code = "0x40400"; break;
      case LINUX_V4_5: version_code = "0x40500"; break;
      case LINUX_V4_6: version_code = "0x40600"; break;
      case LINUX_V4_7: version_code = "0x40700"; break;
      case LINUX_V4_8: version_code = "0x40800"; break;
      case LINUX_V4_9: version_code = "0x40900"; break;
      case LINUX_V4_10: version_code = "0x40a00"; break;
      case LINUX_V4_11: version_code = "0x40b00"; break;
      case LINUX_V4_12: version_code = "0x40c00"; break;
      case LINUX_V4_13: version_code = "0x40d00"; break;
      case LINUX_V4_14: version_code = "0x40e00"; break;
      case LINUX_V4_15: version_code = "0x40f00"; break;
      case LINUX_V4_16: version_code = "0x41000"; break;
      case LINUX_V4_17: version_code = "0x42000"; break;
      case LINUX_V4_18: version_code = "0x43000"; break;
      case LINUX_V4_19: version_code = "0x44000"; break;
      case LINUX_V4_20: version_code = "0x45000"; break;
      case LINUX_V5_0: version_code = "0x50000"; break;
      case LINUX_V5_1: version_code = "0x50100"; break;
      case LINUX_V5_2: version_code = "0x50200"; break;
      default:
	gcc_unreachable ();      
      }

    kernel_version_code = ACONCAT (("__BPF_KERNEL_VERSION_CODE__=",
				    version_code, NULL));
    builtin_define (kernel_version_code);
  }
}

/* Output assembly directives to switch to section NAME.  The section
   should have attributes as specified by FLAGS, which is a bit mask
   of the 'SECTION_*' flags defined in 'output.h'.  If DECL is
   non-NULL, it is the 'VAR_DECL' or 'FUNCTION_DECL' with which this
   section is associated.  */

static void
bpf_asm_named_section (const char *name,
		       unsigned int flags ATTRIBUTE_UNUSED,
		       tree decl ATTRIBUTE_UNUSED)
{
  fprintf (asm_out_file, "\t.section\t%s\n", name);
}

#undef TARGET_ASM_NAMED_SECTION
#define TARGET_ASM_NAMED_SECTION bpf_asm_named_section

/* Return an RTX representing the place where a function returns or
   receives a value of data type RET_TYPE, a tree node representing a
   data type.  */

static rtx
bpf_function_value (const_tree ret_type,
		    const_tree fntype_or_decl,
		    bool outgoing ATTRIBUTE_UNUSED)
{
  enum machine_mode mode;
  int unsignedp;

  mode = TYPE_MODE (ret_type);
  if (INTEGRAL_TYPE_P (ret_type))
    mode = promote_function_mode (ret_type, mode, &unsignedp,
				  fntype_or_decl, 1);

  return gen_rtx_REG (mode, BPF_R0);
}

#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE bpf_function_value

/* Return true if REGNO is the number of a hard register in which the
   values of called function may come back.  */

static bool
bpf_function_value_regno_p (const unsigned int regno)
{
  return (regno == BPF_R0);
}

#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P bpf_function_value_regno_p

/* Compute the size of the function's stack frame, including the local
   area and the register-save area.  */

static void
bpf_compute_frame_layout (void)
{
  int stack_alignment = STACK_BOUNDARY / BITS_PER_UNIT;
  int padding_locals, regno;

  /* Set the space used in the stack by local variables.  This is
     rounded up to respect the minimum stack alignment.  */
  cfun->machine->local_vars_size = get_frame_size ();

  padding_locals = cfun->machine->local_vars_size % stack_alignment;
  if (padding_locals)
    padding_locals = stack_alignment - padding_locals;

  cfun->machine->local_vars_size += padding_locals;

  if (TARGET_XBPF)
    {
      /* Set the space used in the stack by callee-saved used
	 registers in the current function.  There is no need to round
	 up, since the registers are all 8 bytes wide.  */
      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	if ((df_regs_ever_live_p (regno)
	     && !call_used_or_fixed_reg_p (regno))
	    || (cfun->calls_alloca
		&& regno == STACK_POINTER_REGNUM))
	  cfun->machine->callee_saved_reg_size += 8;
    }

  /* Check that the total size of the frame doesn't exceed the limit
     imposed by eBPF.  */
  if ((cfun->machine->local_vars_size
       + cfun->machine->callee_saved_reg_size) > bpf_frame_limit)
    {
      static int stack_limit_exceeded = 0;

      if (!stack_limit_exceeded)
	error ("eBPF stack limit exceeded");
      stack_limit_exceeded = 1;
    }
}

#undef TARGET_COMPUTE_FRAME_LAYOUT
#define TARGET_COMPUTE_FRAME_LAYOUT bpf_compute_frame_layout

/* Expand to the instructions in a function prologue.  This function
   is called when expanding the 'prologue' pattern in bpf.md.  */

void
bpf_expand_prologue (void)
{
  rtx insn;
  HOST_WIDE_INT size;

  size = (cfun->machine->local_vars_size
	  + cfun->machine->callee_saved_reg_size);

  /* The BPF "hardware" provides a fresh new set of registers for each
     called function, some of which are initialized to the values of
     the arguments passed in the first five registers.  In doing so,
     it saves the values of the registers of the caller, and restored
     them upon returning.  Therefore, there is no need to save the
     callee-saved registers here.  What is worse, the kernel
     implementation refuses to run programs in which registers are
     referred before being initialized.  */
  if (TARGET_XBPF)
    {
      int regno;
      int fp_offset = -cfun->machine->local_vars_size;

      /* Save callee-saved hard registes.  The register-save-area
	 starts right after the local variables.  */
      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	{
	  if ((df_regs_ever_live_p (regno)
	       && !call_used_or_fixed_reg_p (regno))
	      || (cfun->calls_alloca
		  && regno == STACK_POINTER_REGNUM))
	    {
	      rtx mem;

	      if (!IN_RANGE (fp_offset, -1 - 0x7fff, 0x7fff))
		/* This has been already reported as an error in
		   bpf_compute_frame_layout. */
		break;
	      else
		{
		  mem = gen_frame_mem (DImode,
				       plus_constant (DImode,
						      hard_frame_pointer_rtx,
						      fp_offset - 8));
		  insn = emit_move_insn (mem, gen_rtx_REG (DImode, regno));
		  RTX_FRAME_RELATED_P (insn) = 1;
		  fp_offset -= 8;
		}
	    }
	}
    }

  /* Set the stack pointer, if the function allocates space
     dynamically.  Note that the value of %sp should be directly
     derived from %fp, for the kernel verifier to track it as a stack
     accessor.  */
  if (cfun->calls_alloca)
    {
      insn = emit_move_insn (stack_pointer_rtx,
			     hard_frame_pointer_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;
      
      if (size > 0)
	{
	  insn = emit_insn (gen_rtx_SET (stack_pointer_rtx,
					 gen_rtx_PLUS (Pmode,
						       stack_pointer_rtx,
						       GEN_INT (-size))));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
    }
}

/* Expand to the instructions in a function epilogue.  This function
   is called when expanding the 'epilogue' pattern in bpf.md.  */

void
bpf_expand_epilogue (void)
{
  /* See note in bpf_expand_prologue for an explanation on why we are
     not restoring callee-saved registers in BPF.  */
  if (TARGET_XBPF)
    {
      rtx insn;
      int regno;
      int fp_offset = -cfun->machine->local_vars_size;

      /* Restore callee-saved hard registes from the stack.  */
      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	{
	  if ((df_regs_ever_live_p (regno)
	       && !call_used_or_fixed_reg_p (regno))
	      || (cfun->calls_alloca
		  && regno == STACK_POINTER_REGNUM))
	    {
	      rtx mem;

	      if (!IN_RANGE (fp_offset, -1 - 0x7fff, 0x7fff))
		/* This has been already reported as an error in
		   bpf_compute_frame_layout. */
		break;
	      else
		{
		  mem = gen_frame_mem (DImode,
				       plus_constant (DImode,
						      hard_frame_pointer_rtx,
						      fp_offset - 8));
		  insn = emit_move_insn (gen_rtx_REG (DImode, regno), mem);
		  RTX_FRAME_RELATED_P (insn) = 1;
		  fp_offset -= 8;
		}
	    }
	}
    }

  emit_jump_insn (gen_exit ());
}

/* Return the initial difference between the specified pair of
   registers.  The registers that can figure in FROM, and TO, are
   specified by ELIMINABLE_REGS in bpf.h.

   This function is used in the definition of
   INITIAL_ELIMINATION_OFFSET in bpf.h  */

HOST_WIDE_INT
bpf_initial_elimination_offset (int from, int to)
{
  HOST_WIDE_INT ret;

  if (from == ARG_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    ret = (cfun->machine->local_vars_size
	   + cfun->machine->callee_saved_reg_size);
  else if (from == ARG_POINTER_REGNUM && to == FRAME_POINTER_REGNUM)
    ret = 0;
  else
    gcc_unreachable ();

  return ret;
}

/* Return the number of consecutive hard registers, starting at
   register number REGNO, required to hold a value of mode MODE.  */

static unsigned int
bpf_hard_regno_nregs (unsigned int regno ATTRIBUTE_UNUSED,
		      enum machine_mode mode)
{
  return CEIL (GET_MODE_SIZE (mode), UNITS_PER_WORD);
}

#undef TARGET_HARD_REGNO_NREGS
#define TARGET_HARD_REGNO_NREGS bpf_hard_regno_nregs

/* Return true if it is permissible to store a value of mode MODE in
   hard register number REGNO, or in several registers starting with
   that one.  */

static bool
bpf_hard_regno_mode_ok (unsigned int regno ATTRIBUTE_UNUSED,
			enum machine_mode mode)
{
  switch (mode)
    {
    case E_SImode:
    case E_DImode:
    case E_HImode:
    case E_QImode:
    case E_TImode:
    case E_SFmode:
    case E_DFmode:
      return true;
    default:
      return false;
    }
}

#undef TARGET_HARD_REGNO_MODE_OK
#define TARGET_HARD_REGNO_MODE_OK bpf_hard_regno_mode_ok

/* Return true if a function must have and use a frame pointer.  */

static bool
bpf_frame_pointer_required (void)
{
  /* We do not have a stack pointer, so we absolutely depend on the
     frame-pointer in order to access the stack... and fishes walk and
     pigs fly glglgl */
  return true;
}

#undef TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED bpf_frame_pointer_required

/* Return `true' if the given RTX X is a valid base for an indirect
   memory access.  STRICT has the same meaning than in
   bpf_legitimate_address_p.  */

static inline bool
bpf_address_base_p (rtx x, bool strict)
{
  return (GET_CODE (x) == REG
	  && (REGNO (x) < 11
	      || (!strict && REGNO (x) >= FIRST_PSEUDO_REGISTER)));
}

/* Return true if X (a RTX) is a legitimate memory address on the
   target machine for a memory operand of mode MODE.  */

static bool
bpf_legitimate_address_p (machine_mode mode ATTRIBUTE_UNUSED,
			  rtx x,
			  bool strict)
{
  switch (GET_CODE (x))
    {
    case REG:
      return bpf_address_base_p (x, strict);

    case PLUS:
      {
	/* Accept (PLUS ADDR_BASE CONST_INT), provided CONST_INT fits
	   in a signed 16-bit.

	   Note that LABEL_REF and SYMBOL_REF are not allowed in
	   REG+IMM addresses, because it is almost certain they will
	   overload the offset field.  */

	rtx x0 = XEXP (x, 0);
	rtx x1 = XEXP (x, 1);
	
	if (bpf_address_base_p (x0, strict) && GET_CODE (x1) == CONST_INT)
	  return IN_RANGE (INTVAL (x1), -1 - 0x7fff, 0x7fff);

	break;
      }
    default:
      break;
    }

  return false;
}

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P bpf_legitimate_address_p

/* Describe the relative costs of RTL expressions.  Return true when
   all subexpressions of X have been processed, and false when
   `rtx_cost' should recurse.  */

static bool
bpf_rtx_costs (rtx x ATTRIBUTE_UNUSED,
	       enum machine_mode mode ATTRIBUTE_UNUSED,
	       int outer_code ATTRIBUTE_UNUSED,
	       int opno ATTRIBUTE_UNUSED,
               int *total ATTRIBUTE_UNUSED,
	       bool speed ATTRIBUTE_UNUSED)
{
  /* To be written.  */
  return false;
}

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS bpf_rtx_costs

/* Return true if an argument at the position indicated by CUM should
   be passed by reference.  If the hook returns true, a copy of that
   argument is made in memory and a pointer to the argument is passed
   instead of the argument itself.  */

static bool
bpf_pass_by_reference (cumulative_args_t cum ATTRIBUTE_UNUSED,
		       const function_arg_info &arg)
{
  unsigned num_bytes = arg.type_size_in_bytes ();

  /* Pass aggregates and values bigger than 5 words by reference.
     Everything else is passed by copy.  */
  return (arg.aggregate_type_p () || (num_bytes > 8*5));
}

#undef TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE bpf_pass_by_reference

/* Return a RTX indicating whether a function argument is passed in a
   register and if so, which register.  */

static rtx
bpf_function_arg (cumulative_args_t ca, const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (ca);

  if (*cum < 5)
    return gen_rtx_REG (arg.mode, *cum + 1);
  else
    /* An error will be emitted for this in
       bpf_function_arg_advance.  */
    return NULL_RTX;
}

#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG bpf_function_arg

/* Update the summarizer variable pointed by CA to advance past an
   argument in the argument list.  */

static void
bpf_function_arg_advance (cumulative_args_t ca,
			  const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (ca);
  unsigned num_bytes = arg.type_size_in_bytes ();
  unsigned num_words = CEIL (num_bytes, UNITS_PER_WORD);

  if (*cum <= 5 && *cum + num_words > 5)
    error ("too many function arguments for eBPF");

  *cum += num_words;
}

#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE bpf_function_arg_advance

/* Output the assembly code for a constructor.  Since eBPF doesn't
   support indirect calls, constructors are not supported.  */

static void
bpf_output_constructor (rtx symbol, int priority ATTRIBUTE_UNUSED)
{
  tree decl = SYMBOL_REF_DECL (symbol);

  if (decl)
    sorry_at (DECL_SOURCE_LOCATION (decl),
	      "no constructors");
  else
    sorry ("no constructors");
}

#undef TARGET_ASM_CONSTRUCTOR
#define TARGET_ASM_CONSTRUCTOR bpf_output_constructor

/* Output the assembly code for a destructor.  Since eBPF doesn't
   support indirect calls, destructors are not supported.  */

static void
bpf_output_destructor (rtx symbol, int priority ATTRIBUTE_UNUSED)
{
  tree decl = SYMBOL_REF_DECL (symbol);

  if (decl)
    sorry_at (DECL_SOURCE_LOCATION (decl),
	      "no destructors");
  else
    sorry ("no destructors");
}

#undef TARGET_ASM_DESTRUCTOR
#define TARGET_ASM_DESTRUCTOR bpf_output_destructor

/* Return the appropriate instruction to CALL to a function.  TARGET
   is an RTX denoting the address of the called function.

   The main purposes of this function are:
   - To reject indirect CALL instructions, which are not supported by
     eBPF.
   - To recognize calls to kernel helper functions and emit the
     corresponding CALL N instruction.

   This function is called from the expansion of the 'call' pattern in
   bpf.md.  */

const char *
bpf_output_call (rtx target)
{
  rtx xops[1];

  switch (GET_CODE (target))
    {
    case CONST_INT:
      output_asm_insn ("call\t%0", &target);
      break;
    case SYMBOL_REF:
      {
	const char *function_name = XSTR (target, 0);
	int code;
      
	if (strncmp (function_name, "__builtin_bpf_helper_", 21) == 0
	    && ((code = bpf_helper_code (function_name + 21)) != 0))
	  {
	    xops[0] = GEN_INT (code);
	    output_asm_insn ("call\t%0", xops);
	  }
	else
	  output_asm_insn ("call\t%0", &target);

	break;
      }
    default:
      error ("indirect call in function, which are not supported by eBPF");
      output_asm_insn ("call 0", NULL);
      break;
    }

  return "";
}

/* Print an instruction operand.  This function is called in the macro
   PRINT_OPERAND defined in bpf.h */

void
bpf_print_operand (FILE *file, rtx op, int code ATTRIBUTE_UNUSED)
{
  switch (GET_CODE (op))
    {
    case REG:
      fprintf (file, "%s", reg_names[REGNO (op)]);
      break;
    case MEM:
      output_address (GET_MODE (op), XEXP (op, 0));
      break;
    case CONST_DOUBLE:
      if (CONST_DOUBLE_HIGH (op))
	fprintf (file, HOST_WIDE_INT_PRINT_DOUBLE_HEX,
		 CONST_DOUBLE_HIGH (op), CONST_DOUBLE_LOW (op));
      else if (CONST_DOUBLE_LOW (op) < 0)
	fprintf (file, HOST_WIDE_INT_PRINT_HEX, CONST_DOUBLE_LOW (op));
      else
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, CONST_DOUBLE_LOW (op));
      break;
    default:
      output_addr_const (file, op);
    }
}

/* Print an operand which is an address.  This function should handle
   any legit address, as accepted by bpf_legitimate_address_p, and
   also addresses that are valid in CALL instructions.

   This function is called in the PRINT_OPERAND_ADDRESS macro defined
   in bpf.h */

void
bpf_print_operand_address (FILE *file, rtx addr)
{
  switch (GET_CODE (addr))
    {
    case REG:
      fprintf (file, "[%s+0]", reg_names[REGNO (addr)]);
      break;
    case PLUS:
      {
	rtx op0 = XEXP (addr, 0);
	rtx op1 = XEXP (addr, 1);

	if (GET_CODE (op0) == REG && GET_CODE (op1) == CONST_INT)
	  {
	    fprintf (file, "[%s+", reg_names[REGNO (op0)]);
	    output_addr_const (file, op1);
	    fputs ("]", file);
	  }
	else
	  fatal_insn ("invalid address in operand", addr);
	break;
      }
    case MEM:
      /* Fallthrough.  */
    case LABEL_REF:
      /* Fallthrough.  */
      fatal_insn ("unsupported operand", addr);
      break;
    default:
      output_addr_const (file, addr);
      break;
    }
}

/* Add a BPF builtin function with NAME, CODE and TYPE.  Return
   the function decl or NULL_TREE if the builtin was not added.  */

static tree
def_builtin (const char *name, enum bpf_builtins code, tree type)
{
  tree t
    = add_builtin_function (name, type, code, BUILT_IN_MD, NULL, NULL_TREE);

  bpf_builtins[code] = t;
  return t;
}

/* Define machine-specific built-in functions.  */

static void
bpf_init_builtins (void)
{
  /* Built-ins for calling kernel helpers.  */

  tree pt = build_pointer_type (void_type_node);
  tree const_void_type
    = build_qualified_type (void_type_node, TYPE_QUAL_CONST);
  tree cpt = build_pointer_type (const_void_type);
  tree st = short_integer_type_node;
  tree ust = uint16_type_node;
  tree it = integer_type_node;
  tree ut = unsigned_type_node;
  tree const_char_type
    = build_qualified_type (char_type_node, TYPE_QUAL_CONST);
  tree cst = build_pointer_type (const_char_type);
  tree vt = void_type_node;
  tree ult = long_unsigned_type_node;
  tree u32t = uint32_type_node;
  tree u64t = uint64_type_node;
  tree llt = long_long_integer_type_node;
  tree ullt = long_long_unsigned_type_node;
  
#define TYPES build_function_type_list
#define VTYPES build_varargs_function_type_list
#define DEF_HELPER(V,D,N,T)				\
  do							\
    {							\
      if (bpf_kernel >= (V))				\
	def_builtin ("__builtin_bpf_helper_" #N,	\
		     BPF_BUILTIN_HELPER_##D,		\
		     T);				\
    } while (0);
#  include "bpf-helpers.def"
#undef TYPES
#undef VTYPES
#undef DEF_HELPER

  /* Built-ins for BPF_LD_ABS and BPF_LD_IND instructions.  */

  def_builtin ("__builtin_bpf_load_byte", BPF_BUILTIN_LOAD_BYTE,
	       build_function_type_list (ullt, ullt, 0));
  def_builtin ("__builtin_bpf_load_half", BPF_BUILTIN_LOAD_HALF,
	       build_function_type_list (ullt, ullt, 0));
  def_builtin ("__builtin_bpf_load_word", BPF_BUILTIN_LOAD_WORD,
	       build_function_type_list (ullt, ullt, 0));
}

#undef TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS bpf_init_builtins

/* Expand a call to a BPF-specific built-in function that was set up
   with bpf_init_builtins.  */

static rtx
bpf_expand_builtin (tree exp, rtx target,
		    rtx subtarget ATTRIBUTE_UNUSED,
		    machine_mode mode ATTRIBUTE_UNUSED,
		    int ignore)
{
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  int code = DECL_MD_FUNCTION_CODE (fndecl);

  if (code >= 1 && code < BPF_BUILTIN_HELPER_MAX)
    {
      /* This is a builtin to call a kernel helper function.

	 For these builtins, we just expand the function call normally
	 with expand_call like we would do for a libcall. The function
	 bpf_output_call below will then do The Right Thing (TM),
	 recognizing the name of the called __builtin_helper_* symbol
	 and emitting the corresponding CALL N instruction whenever
	 necessary.  */

      return expand_call (exp, target, ignore);
    }
  else if (code == BPF_BUILTIN_LOAD_BYTE
	   || code == BPF_BUILTIN_LOAD_HALF
	   || code == BPF_BUILTIN_LOAD_WORD)
    {
      /* Expand an indirect load from the sk_buff in the context.
	 There is just one argument to the builtin, which is the
	 offset.

	 We try first to expand a ldabs* instruction.  In case this
	 fails, we try a ldind* instruction.  */

      enum insn_code abs_icode
	= (code == BPF_BUILTIN_LOAD_BYTE ? CODE_FOR_ldabsb
	   : code == BPF_BUILTIN_LOAD_HALF ? CODE_FOR_ldabsh
	   : CODE_FOR_ldabsw);

      enum insn_code ind_icode
	= (code == BPF_BUILTIN_LOAD_BYTE ? CODE_FOR_ldindb
	   : code == BPF_BUILTIN_LOAD_HALF ? CODE_FOR_ldindh
	   : CODE_FOR_ldindw);

      tree offset_arg = CALL_EXPR_ARG (exp, 0);
      struct expand_operand ops[2];

      create_input_operand (&ops[0], expand_normal (offset_arg),
			    TYPE_MODE (TREE_TYPE (offset_arg)));
      create_input_operand (&ops[1], const0_rtx, SImode);

      if (!maybe_expand_insn (abs_icode, 2, ops)
	  && !maybe_expand_insn (ind_icode, 2, ops))
	{
	  error ("invalid argument to built-in function");
	  return gen_rtx_REG (ops[0].mode, BPF_R0);
	}

      /* The result of the load is in R0.  */
      return gen_rtx_REG (ops[0].mode, BPF_R0);
    }

  gcc_unreachable ();
}

#undef TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN bpf_expand_builtin

/* Initialize target-specific function library calls.  This is mainly
   used to call library-provided soft-fp operations, since eBPF
   doesn't support floating-point in "hardware".  */

static void
bpf_init_libfuncs (void)
{
  set_conv_libfunc (sext_optab, DFmode, SFmode,
		    "__bpf_extendsfdf2");
  set_conv_libfunc (trunc_optab, SFmode, DFmode,
		    "__bpf_truncdfsf2");
  set_conv_libfunc (sfix_optab, SImode, DFmode,
		    "__bpf_fix_truncdfsi");
  set_conv_libfunc (sfloat_optab, DFmode, SImode,
		    "__bpf_floatsidf");
  set_conv_libfunc (ufloat_optab, DFmode, SImode,
		    "__bpf_floatunsidf");
}

#undef TARGET_INIT_LIBFUNCS
#define TARGET_INIT_LIBFUNCS bpf_init_libfuncs

/* Define the mechanism that will be used for describing frame unwind
   information to the debugger.  In eBPF it is not possible to unwind
   frames.  */

static enum unwind_info_type
bpf_debug_unwind_info ()
{
  return UI_NONE;
}

#undef TARGET_DEBUG_UNWIND_INFO
#define TARGET_DEBUG_UNWIND_INFO bpf_debug_unwind_info

/* Output assembly directives to assemble data of various sized and
   alignments.  */

#undef TARGET_ASM_BYTE_OP
#define TARGET_ASM_BYTE_OP "\t.byte\t"
#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.half\t"
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.word\t"
#undef TARGET_ASM_ALIGNED_DI_OP
#define TARGET_ASM_ALIGNED_DI_OP "\t.dword\t"

/* Finally, build the GCC target.  */

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-bpf.h"
