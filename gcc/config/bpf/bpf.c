/* Subroutines used for code generation for eBPF.
   Copyright (C) 2019-2021 Free Software Foundation, Inc.

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
#include "flags.h"

#include "cfg.h" /* needed for struct control_flow_graph used in BB macros */
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "tree-pass.h"
#include "tree-iterator.h"

#include "context.h"
#include "pass_manager.h"

#include "gimplify.h"
#include "gimplify-me.h"

#include "ctfc.h"
#include "btf.h"

#include "coreout.h"

/* Per-function machine data.  */
struct GTY(()) machine_function
{
  /* Number of bytes saved on the stack for local variables.  */
  int local_vars_size;

  /* Number of bytes saved on the stack for callee-saved
     registers.  */
  int callee_saved_reg_size;
};

/* Handle an attribute requiring a FUNCTION_DECL;
   arguments as in struct attribute_spec.handler.  */

static tree
bpf_handle_fndecl_attribute (tree *node, tree name,
			     tree args,
			     int flags ATTRIBUTE_UNUSED,
			     bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute only applies to functions",
	       name);
      *no_add_attrs = true;
    }

  if (is_attribute_p ("kernel_helper", name))
    {
      if (args)
	{
	  tree cst = TREE_VALUE (args);
	  if (TREE_CODE (cst) != INTEGER_CST)
	    {
	      warning (OPT_Wattributes, "%qE attribute requires an integer argument",
		       name);
	      *no_add_attrs = true;
	    }
	}
      else
	{
	  warning (OPT_Wattributes, "%qE requires an argument", name);
	  *no_add_attrs = true;
	}
    }

  return NULL_TREE;
}

/* Handle preserve_access_index attribute, which can be applied to structs,
   unions and classes. Actually adding the attribute to the TYPE_DECL is
   taken care of for us, so just warn for types that aren't supported.  */

static tree
bpf_handle_preserve_access_index_attribute (tree *node, tree name,
					    tree args,
					    int flags,
					    bool *no_add_attrs)
{
  if (TREE_CODE (*node) != RECORD_TYPE && TREE_CODE (*node) != UNION_TYPE)
    {
      warning (OPT_Wattributes,
	       "%qE attribute only applies to structure, union and class types",
	       name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Target-specific attributes.  */

static const struct attribute_spec bpf_attribute_table[] =
{
  /* Syntax: { name, min_len, max_len, decl_required, type_required,
	       function_type_required, affects_type_identity, handler,
	       exclude } */

 /* Attribute to mark function prototypes as kernel helpers.  */
 { "kernel_helper", 1, 1, true, false, false, false,
   bpf_handle_fndecl_attribute, NULL },

 /* CO-RE support: attribute to mark that all accesses to the declared
    struct/union/array should be recorded.  */
 { "preserve_access_index", 0, -1, false, true, false, true,
   bpf_handle_preserve_access_index_attribute, NULL },

 /* The last attribute spec is set to be NULL.  */
 { NULL,	0,  0, false, false, false, false, NULL, NULL }
};

#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE bpf_attribute_table

/* Data structures for the eBPF specific built-ins.  */

/* Maximum number of arguments taken by a builtin function, plus
   one.  */
#define BPF_BUILTIN_MAX_ARGS 5

enum bpf_builtins
{
  BPF_BUILTIN_UNUSED = 0,
  /* Built-ins for non-generic loads and stores.  */
  BPF_BUILTIN_LOAD_BYTE,
  BPF_BUILTIN_LOAD_HALF,
  BPF_BUILTIN_LOAD_WORD,

  /* Compile Once - Run Everywhere (CO-RE) support.  */
  BPF_BUILTIN_PRESERVE_ACCESS_INDEX,

  BPF_BUILTIN_MAX,
};

static GTY (()) tree bpf_builtins[(int) BPF_BUILTIN_MAX];


void bpf_register_coreattr_pass (void);

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

  /* BPF CO-RE support requires BTF debug info generation.  */
  if (TARGET_BPF_CORE && !btf_debuginfo_p ())
    error ("BPF CO-RE requires BTF debugging information, use %<-gbtf%>");

  /* To support the portability needs of BPF CO-RE approach, BTF debug
     information includes the BPF CO-RE relocations.  */
  if (TARGET_BPF_CORE)
    write_symbols |= BTF_WITH_CORE_DEBUG;

  /* Unlike much of the other BTF debug information, the information necessary
     for CO-RE relocations is added to the CTF container by the BPF backend.
     Enabling LTO adds some complications in the generation of the BPF CO-RE
     relocations because if LTO is in effect, the relocations need to be
     generated late in the LTO link phase.  This poses a new challenge for the
     compiler to now provide means to combine the early BTF and late BTF CO-RE
     debug info, similar to DWARF debug info.  BTF/CO-RE debug info is not
     amenable to such a split generation and a later merging.

     In any case, in absence of linker support for BTF sections at this time,
     it is acceptable to simply disallow LTO for BPF CO-RE compilations.  */

  if (flag_lto && TARGET_BPF_CORE)
    sorry ("BPF CO-RE does not support LTO");

  /* -gbtf implies -mcore when using the BPF backend, unless -mno-co-re
     is specified.  */
  if (btf_debuginfo_p () && !(target_flags_explicit & MASK_BPF_CORE))
    {
      target_flags |= MASK_BPF_CORE;
      write_symbols |= BTF_WITH_CORE_DEBUG;
    }

  /* Determine available features from ISA setting (-mcpu=).  */
  if (bpf_has_jmpext == -1)
    bpf_has_jmpext = (bpf_isa >= ISA_V2);

  if (bpf_has_alu32 == -1)
    bpf_has_alu32 = (bpf_isa >= ISA_V3);

  if (bpf_has_jmp32 == -1)
    bpf_has_jmp32 = (bpf_isa >= ISA_V3);

}

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE bpf_option_override

/* Return FALSE iff -mcore has been specified.  */

static bool
ctfc_debuginfo_early_finish_p (void)
{
  if (TARGET_BPF_CORE)
    return false;
  else
    return true;
}

#undef TARGET_CTFC_DEBUGINFO_EARLY_FINISH_P
#define TARGET_CTFC_DEBUGINFO_EARLY_FINISH_P ctfc_debuginfo_early_finish_p

/* Implement TARGET_ASM_INIT_SECTIONS.  */

static void
bpf_asm_init_sections (void)
{
  if (TARGET_BPF_CORE)
    btf_ext_init ();
}

#undef TARGET_ASM_INIT_SECTIONS
#define TARGET_ASM_INIT_SECTIONS bpf_asm_init_sections

/* Implement TARGET_ASM_FILE_END.  */

static void
bpf_file_end (void)
{
  if (TARGET_BPF_CORE)
    btf_ext_output ();
}

#undef TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END bpf_file_end

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

      if (size > 0)
	{
	  insn = emit_insn (gen_rtx_SET (stack_pointer_rtx,
					 gen_rtx_PLUS (Pmode,
						       stack_pointer_rtx,
						       GEN_INT (-size))));
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
		  fp_offset -= 8;
		}
	    }
	}
    }

  emit_jump_insn (gen_exit ());
}

/* Expand to the instructions for a conditional branch. This function
   is called when expanding the 'cbranch<mode>4' pattern in bpf.md.  */

void
bpf_expand_cbranch (machine_mode mode, rtx *operands)
{
  /* If all jump instructions are available, nothing special to do here.  */
  if (bpf_has_jmpext)
    return;

  enum rtx_code code = GET_CODE (operands[0]);

  /* Without the conditional branch instructions jslt, jsle, jlt, jle, we need
     to convert conditional branches that would use them to an available
     operation instead by reversing the comparison.  */
  if ((code == LT || code == LE || code == LTU || code == LEU))
    {
      /* Reverse the condition.  */
      PUT_CODE (operands[0], reverse_condition (code));

      /* Swap the operands, and ensure that the first is a register.  */
      if (!register_operand (operands[2], mode))
	operands[2] = force_reg (mode, operands[2]);

      rtx tmp = operands[1];
      operands[1] = operands[2];
      operands[2] = tmp;
    }
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
	tree decl = SYMBOL_REF_DECL (target);
	tree attr;

	if (decl
	    && (attr = lookup_attribute ("kernel_helper",
					 DECL_ATTRIBUTES (decl))))
	  {
	    tree attr_args = TREE_VALUE (attr);

	    xops[0] = GEN_INT (TREE_INT_CST_LOW (TREE_VALUE (attr_args)));
	    output_asm_insn ("call\t%0", xops);
	  }
	else
	  output_asm_insn ("call\t%0", &target);

	break;
      }
    default:
      if (TARGET_XBPF)
	output_asm_insn ("call\t%0", &target);
      else
	{
	  error ("indirect call in function, which are not supported by eBPF");
	  output_asm_insn ("call 0", NULL);
	}
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
  tree ullt = long_long_unsigned_type_node;

  /* Built-ins for BPF_LD_ABS and BPF_LD_IND instructions.  */

  def_builtin ("__builtin_bpf_load_byte", BPF_BUILTIN_LOAD_BYTE,
	       build_function_type_list (ullt, ullt, 0));
  def_builtin ("__builtin_bpf_load_half", BPF_BUILTIN_LOAD_HALF,
	       build_function_type_list (ullt, ullt, 0));
  def_builtin ("__builtin_bpf_load_word", BPF_BUILTIN_LOAD_WORD,
	       build_function_type_list (ullt, ullt, 0));
  def_builtin ("__builtin_preserve_access_index",
	       BPF_BUILTIN_PRESERVE_ACCESS_INDEX,
	       build_function_type_list (ptr_type_node, ptr_type_node, 0));
}

#undef TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS bpf_init_builtins

static tree bpf_core_compute (tree, vec<unsigned int> *);
static int bpf_core_get_index (const tree);
static bool is_attr_preserve_access (tree);

/* Expand a call to a BPF-specific built-in function that was set up
   with bpf_init_builtins.  */

static rtx
bpf_expand_builtin (tree exp, rtx target ATTRIBUTE_UNUSED,
		    rtx subtarget ATTRIBUTE_UNUSED,
		    machine_mode mode ATTRIBUTE_UNUSED,
		    int ignore ATTRIBUTE_UNUSED)
{
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  int code = DECL_MD_FUNCTION_CODE (fndecl);

  if (code == BPF_BUILTIN_LOAD_BYTE
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
  else if (code == -1)
    {
      /* A resolved overloaded builtin, e.g. __bpf_preserve_access_index_si */
      tree arg = CALL_EXPR_ARG (exp, 0);

      if (arg == NULL_TREE)
	return NULL_RTX;

      auto_vec<unsigned int, 16> accessors;
      tree container;

      if (TREE_CODE (arg) == SSA_NAME)
	{
	  gimple *def_stmt = SSA_NAME_DEF_STMT (arg);

	  if (is_gimple_assign (def_stmt))
	    arg = gimple_assign_rhs1 (def_stmt);
	  else
	    return expand_normal (arg);
	}

      /* Avoid double-recording information if the argument is an access to
	 a struct/union marked __attribute__((preserve_access_index)). This
	 Will be handled by the attribute handling pass.  */
      if (is_attr_preserve_access (arg))
	return expand_normal (arg);

      container = bpf_core_compute (arg, &accessors);

      /* Any valid use of the builtin must have at least one access. Otherwise,
	 there is nothing to record and nothing to do. This is primarily a
	 guard against optimizations leading to unexpected expressions in the
	 argument of the builtin. For example, if the builtin is used to read
	 a field of a structure which can be statically determined to hold a
	 constant value, the argument to the builtin will be optimized to that
	 constant. This is OK, and means the builtin call is superfluous.
	 e.g.
	   struct S foo;
	   foo.a = 5;
	   int x = __preserve_access_index (foo.a);
	   ... do stuff with x
	 'foo.a' in the builtin argument will be optimized to '5' with -01+.
	 This sequence does not warrant recording a CO-RE relocation.  */

      if (accessors.length () < 1)
	return expand_normal (arg);

      accessors.reverse ();

      container = TREE_TYPE (container);

      rtx_code_label *label = gen_label_rtx ();
      LABEL_PRESERVE_P (label) = 1;
      emit_label (label);

      /* Determine what output section this relocation will apply to.
	 If this function is associated with a section, use that. Otherwise,
	 fall back on '.text'.  */
      const char * section_name;
      if (current_function_decl && DECL_SECTION_NAME (current_function_decl))
	section_name = DECL_SECTION_NAME (current_function_decl);
      else
	section_name = ".text";

      /* Add the CO-RE relocation information to the BTF container.  */
      bpf_core_reloc_add (container, section_name, &accessors, label);

      return expand_normal (arg);
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


/* BPF Compile Once - Run Everywhere (CO-RE) support routines.

   BPF CO-RE is supported in two forms:
   - A target builtin, __builtin_preserve_access_index

     This builtin accepts a single argument. Any access to an aggregate data
     structure (struct, union or array) within the argument will be recorded by
     the CO-RE machinery, resulting in a relocation record being placed in the
     .BTF.ext section of the output.

     It is implemented in bpf_resolve_overloaded_builtin () and
     bpf_expand_builtin (), using the supporting routines below.

   - An attribute, __attribute__((preserve_access_index))

     This attribute can be applied to struct and union types. Any access to a
     type with this attribute will be recorded by the CO-RE machinery.

     The pass pass_bpf_core_attr, below, implements support for
     this attribute.  */

/* Traverse the subtree under NODE, which is expected to be some form of
   aggregate access the CO-RE machinery cares about (like a read of a member of
   a struct or union), collecting access indices for the components and storing
   them in the vector referenced by ACCESSORS.

   Return the ultimate (top-level) container of the aggregate access. In general,
   this will be a VAR_DECL or some kind of REF.

   Note that the accessors are computed *in reverse order* of how the BPF
   CO-RE machinery defines them. The vector needs to be reversed (or simply
   output in reverse order) for the .BTF.ext relocation information.  */

static tree
bpf_core_compute (tree node, vec<unsigned int> *accessors)
{

  if (TREE_CODE (node) == ADDR_EXPR)
    node = TREE_OPERAND (node, 0);

  else if (TREE_CODE (node) == INDIRECT_REF
	   || TREE_CODE (node) == POINTER_PLUS_EXPR)
    {
      accessors->safe_push (0);
      return TREE_OPERAND (node, 0);
    }

  while (1)
    {
      switch (TREE_CODE (node))
	{
	case COMPONENT_REF:
	  accessors->safe_push (bpf_core_get_index (TREE_OPERAND (node, 1)));
	  break;

	case ARRAY_REF:
	case ARRAY_RANGE_REF:
	  accessors->safe_push (bpf_core_get_index (node));
	  break;

	case MEM_REF:
	  accessors->safe_push (bpf_core_get_index (node));
	  if (TREE_CODE (TREE_OPERAND (node, 0)) == ADDR_EXPR)
	    node = TREE_OPERAND (TREE_OPERAND (node, 0), 0);
	  goto done;

	default:
	  goto done;
	}
      node = TREE_OPERAND (node, 0);
    }
 done:
  return node;

}

/* Compute the index of the NODE in its immediate container.
   NODE should be a FIELD_DECL (i.e. of struct or union), or an ARRAY_REF. */
static int
bpf_core_get_index (const tree node)
{
  enum tree_code code = TREE_CODE (node);

  if (code == FIELD_DECL)
    {
      /* Lookup the index from the BTF information.  Some struct/union members
	 may not be emitted in BTF; only the BTF container has enough
	 information to compute the correct index.  */
      int idx = bpf_core_get_sou_member_index (ctf_get_tu_ctfc (), node);
      if (idx >= 0)
	return idx;
    }

  else if (code == ARRAY_REF || code == ARRAY_RANGE_REF || code == MEM_REF)
    {
      /* For array accesses, the index is operand 1.  */
      tree index = TREE_OPERAND (node, 1);

      /* If the indexing operand is a constant, extracting is trivial.  */
      if (TREE_CODE (index) == INTEGER_CST && tree_fits_shwi_p (index))
	return tree_to_shwi (index);
    }

  return -1;
}

/* Synthesize a new builtin function declaration at LOC with signature TYPE.
   Used by bpf_resolve_overloaded_builtin to resolve calls to
   __builtin_preserve_access_index.  */

static tree
bpf_core_newdecl (location_t loc, tree type)
{
  tree rettype = build_function_type_list (type, type, NULL);
  tree newdecl = NULL_TREE;
  char name[80];
  int len = snprintf (name, sizeof (name), "%s", "__builtin_pai_");

  static unsigned long cnt = 0;
  len = snprintf (name + len, sizeof (name) - len, "%lu", cnt++);

  return add_builtin_function_ext_scope (name, rettype, -1, BUILT_IN_MD, NULL,
					 NULL_TREE);
}

/* Return whether EXPR could access some aggregate data structure that
   BPF CO-RE support needs to know about.  */

static int
bpf_core_is_maybe_aggregate_access (tree expr)
{
  enum tree_code code = TREE_CODE (expr);
  if (code == COMPONENT_REF || code == ARRAY_REF)
    return 1;

  if (code == ADDR_EXPR)
      return bpf_core_is_maybe_aggregate_access (TREE_OPERAND (expr, 0));

  return 0;
}

/* Callback function used with walk_tree from bpf_resolve_overloaded_builtin.  */

static tree
bpf_core_walk (tree *tp, int *walk_subtrees, void *data)
{
  location_t loc = *((location_t *) data);

  /* If this is a type, don't do anything. */
  if (TYPE_P (*tp))
    {
      *walk_subtrees = 0;
      return NULL_TREE;
    }

  if (bpf_core_is_maybe_aggregate_access (*tp))
    {
      tree newdecl = bpf_core_newdecl (loc, TREE_TYPE (*tp));
      tree newcall = build_call_expr_loc (loc, newdecl, 1, *tp);
      *tp = newcall;
      *walk_subtrees = 0;
    }

  return NULL_TREE;
}


/* Implement TARGET_RESOLVE_OVERLOADED_BUILTIN (see gccint manual section
   Target Macros::Misc.).
   We use this for the __builtin_preserve_access_index builtin for CO-RE
   support.

   FNDECL is the declaration of the builtin, and ARGLIST is the list of
   arguments passed to it, and is really a vec<tree,_> *.

   In this case, the 'operation' implemented by the builtin is a no-op;
   the builtin is just a marker. So, the result is simply the argument.  */

static tree
bpf_resolve_overloaded_builtin (location_t loc, tree fndecl, void *arglist)
{
  if (DECL_MD_FUNCTION_CODE (fndecl) != BPF_BUILTIN_PRESERVE_ACCESS_INDEX)
    return NULL_TREE;

  /* We only expect one argument, but it may be an arbitrarily-complicated
     statement-expression. */
  vec<tree, va_gc> *params = static_cast<vec<tree, va_gc> *> (arglist);
  unsigned n_params = params ? params->length() : 0;

  if (n_params != 1)
    {
      error_at (loc, "expected exactly 1 argument");
      return NULL_TREE;
    }

  tree param = (*params)[0];

  /* If not generating BPF_CORE information, the builtin does nothing.  */
  if (!TARGET_BPF_CORE)
    return param;

  /* Do remove_c_maybe_const_expr for the arg.
     TODO: WHY do we have to do this here? Why doesn't c-typeck take care
     of it before or after this hook? */
  if (TREE_CODE (param) == C_MAYBE_CONST_EXPR)
    param = C_MAYBE_CONST_EXPR_EXPR (param);

  /* Construct a new function declaration with the correct type, and return
     a call to it.

     Calls with statement-expressions, for example:
     _(({ foo->a = 1; foo->u[2].b = 2; }))
     require special handling.

     We rearrange this into a new block scope in which each statement
     becomes a unique builtin call:
     {
       _ ({ foo->a = 1;});
       _ ({ foo->u[2].b = 2;});
     }

     This ensures that all the relevant information remains within the
     expression trees the builtin finally gets.  */

  walk_tree (&param, bpf_core_walk, (void *) &loc, NULL);

  return param;
}

#undef TARGET_RESOLVE_OVERLOADED_BUILTIN
#define TARGET_RESOLVE_OVERLOADED_BUILTIN bpf_resolve_overloaded_builtin


/* Handling for __attribute__((preserve_access_index)) for BPF CO-RE support.

   This attribute marks a structure/union/array type as "preseve", so that
   every access to that type should be recorded and replayed by the BPF loader;
   this is just the same functionality as __builtin_preserve_access_index,
   but in the form of an attribute for an entire aggregate type.

   Note also that nested structs behave as though they all have the attribute.
   For example:
     struct X { int a; };
     struct Y { struct X bar} __attribute__((preserve_access_index));
     struct Y foo;
     foo.bar.a;
   will record access all the way to 'a', even though struct X does not have
   the preserve_access_index attribute.

   This is to follow LLVM behavior.

   This pass finds all accesses to objects of types marked with the attribute,
   and wraps them in the same "low-level" builtins used by the builtin version.
   All logic afterwards is therefore identical to the builtin version of
   preserve_access_index.  */

/* True iff tree T accesses any member of a struct/union/class which is marked
   with the PRESERVE_ACCESS_INDEX attribute.  */

static bool
is_attr_preserve_access (tree t)
{
  if (t == NULL_TREE)
    return false;

  poly_int64 bitsize, bitpos;
  tree var_off;
  machine_mode mode;
  int sign, reverse, vol;

  tree base = get_inner_reference (t, &bitsize, &bitpos, &var_off, &mode,
				   &sign, &reverse, &vol);

  if (TREE_CODE (base) == MEM_REF)
    {
      return lookup_attribute ("preserve_access_index",
			       TYPE_ATTRIBUTES (TREE_TYPE (base)));
    }

  if (TREE_CODE (t) == COMPONENT_REF)
    {
      /* preserve_access_index propegates into nested structures,
	 so check whether this is a component of another component
	 which in turn is part of such a struct.  */

      const tree op = TREE_OPERAND (t, 0);

      if (TREE_CODE (op) == COMPONENT_REF)
	return is_attr_preserve_access (op);

      const tree container = DECL_CONTEXT (TREE_OPERAND (t, 1));

      return lookup_attribute ("preserve_access_index",
			       TYPE_ATTRIBUTES (container));
    }

  else if (TREE_CODE (t) == ADDR_EXPR)
    return is_attr_preserve_access (TREE_OPERAND (t, 0));

  return false;
}

/* The body of pass_bpf_core_attr. Scan RTL for accesses to structs/unions
   marked with __attribute__((preserve_access_index)) and generate a CO-RE
   relocation for any such access.  */

static void
handle_attr_preserve (function *fn)
{
  basic_block bb;
  rtx_insn *insn;
  rtx_code_label *label;
  FOR_EACH_BB_FN (bb, fn)
    {
      FOR_BB_INSNS (bb, insn)
	{
	  if (!NONJUMP_INSN_P (insn))
	    continue;
	  rtx pat = PATTERN (insn);
	  if (GET_CODE (pat) != SET)
	    continue;

	  start_sequence();

	  for (int i = 0; i < 2; i++)
	    {
	      rtx mem = XEXP (pat, i);
	      if (MEM_P (mem))
		{
		  tree expr = MEM_EXPR (mem);
		  if (!expr)
		    continue;

		  if (TREE_CODE (expr) == MEM_REF
		      && TREE_CODE (TREE_OPERAND (expr, 0)) == SSA_NAME)
		    {
		      gimple *def_stmt = SSA_NAME_DEF_STMT (TREE_OPERAND (expr, 0));
		      if (is_gimple_assign (def_stmt))
			expr = gimple_assign_rhs1 (def_stmt);
		    }

		  if (is_attr_preserve_access (expr))
		    {
		      auto_vec<unsigned int, 16> accessors;
		      tree container = bpf_core_compute (expr, &accessors);
		      if (accessors.length () < 1)
			continue;
		      accessors.reverse ();

		      container = TREE_TYPE (container);
		      const char * section_name;
		      if (DECL_SECTION_NAME (fn->decl))
			section_name = DECL_SECTION_NAME (fn->decl);
		      else
			section_name = ".text";

		      label = gen_label_rtx ();
		      LABEL_PRESERVE_P (label) = 1;
		      emit_label (label);

		      /* Add the CO-RE relocation information to the BTF container.  */
		      bpf_core_reloc_add (container, section_name, &accessors, label);
		    }
		}
	    }
	  rtx_insn *seq = get_insns ();
	  end_sequence ();
	  emit_insn_before (seq, insn);
	}
    }
}


/* This pass finds accesses to structures marked with the BPF target attribute
   __attribute__((preserve_access_index)). For every such access, a CO-RE
   relocation record is generated, to be output in the .BTF.ext section.  */

namespace {

const pass_data pass_data_bpf_core_attr =
{
  RTL_PASS, /* type */
  "bpf_core_attr", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_bpf_core_attr : public rtl_opt_pass
{
public:
  pass_bpf_core_attr (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_bpf_core_attr, ctxt)
  {}

  virtual bool gate (function *) { return TARGET_BPF_CORE; }
  virtual unsigned int execute (function *);
};

unsigned int
pass_bpf_core_attr::execute (function *fn)
{
  handle_attr_preserve (fn);
  return 0;
}

} /* Anonymous namespace.  */

rtl_opt_pass *
make_pass_bpf_core_attr (gcc::context *ctxt)
{
  return new pass_bpf_core_attr (ctxt);
}

/* Finally, build the GCC target.  */

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-bpf.h"
