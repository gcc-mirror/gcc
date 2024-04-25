/* Subroutines used for code generation for eBPF.
   Copyright (C) 2019-2024 Free Software Foundation, Inc.

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

#include "core-builtins.h"
#include "opts.h"

/* Per-function machine data.  */
struct GTY(()) machine_function
{
  /* Number of bytes saved on the stack for local variables.  */
  int local_vars_size;
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
					    tree args ATTRIBUTE_UNUSED,
					    int flags ATTRIBUTE_UNUSED,
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

TARGET_GNU_ATTRIBUTES (bpf_attribute_table,
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

 /* Support for `naked' function attribute.  */
 { "naked", 0, 1, false, false, false, false,
   bpf_handle_fndecl_attribute, NULL }
});

#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE bpf_attribute_table

/* Data structures for the eBPF specific built-ins.  */

/* Maximum number of arguments taken by a builtin function, plus
   one.  */
#define BPF_BUILTIN_MAX_ARGS 5

GTY (()) tree bpf_builtins[(int) BPF_BUILTIN_MAX];

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
  if (TARGET_BPF_CORE
      && (!btf_debuginfo_p () || (debug_info_level < DINFO_LEVEL_NORMAL)))
    error ("BPF CO-RE requires BTF debugging information, use %<-gbtf%>");

  /* BPF applications always generate .BTF.ext.  */
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
  if (btf_debuginfo_p ()
      && (debug_info_level >= DINFO_LEVEL_NORMAL)
      && !(target_flags_explicit & MASK_BPF_CORE))
    target_flags |= MASK_BPF_CORE;

  /* Determine available features from ISA setting (-mcpu=).  */
  if (bpf_has_jmpext == -1)
    bpf_has_jmpext = (bpf_isa >= ISA_V2);

  if (bpf_has_alu32 == -1)
    bpf_has_alu32 = (bpf_isa >= ISA_V3);

  if (bpf_has_jmp32 == -1)
    bpf_has_jmp32 = (bpf_isa >= ISA_V3);

  if (bpf_has_v3_atomics == -1)
    bpf_has_v3_atomics = (bpf_isa >= ISA_V3);

  if (bpf_has_bswap == -1)
    bpf_has_bswap = (bpf_isa >= ISA_V4);

  if (bpf_has_sdiv == -1)
    bpf_has_sdiv = (bpf_isa >= ISA_V4);

  if (bpf_has_smov == -1)
    bpf_has_smov = (bpf_isa >= ISA_V4);

  /* Disable -fstack-protector as it is not supported in BPF.  */
  if (flag_stack_protect)
    {
      if (!flag_stack_protector_set_by_fhardened_p)
	inform (input_location,
		"%<-fstack-protector%> does not work "
		"on this architecture");
      flag_stack_protect = 0;
    }

  /* The BPF target does not support tail call optimization.  */
  flag_optimize_sibling_calls = 0;
}

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE bpf_option_override

/* Implement TARGET_ASM_INIT_SECTIONS.  */

static void
bpf_asm_init_sections (void)
{
  if (btf_debuginfo_p () && btf_with_core_debuginfo_p ())
    btf_ext_init ();
}

#undef TARGET_ASM_INIT_SECTIONS
#define TARGET_ASM_INIT_SECTIONS bpf_asm_init_sections

/* Implement TARGET_ASM_FILE_END.  */

static void
bpf_file_end (void)
{
  if (btf_debuginfo_p () && btf_with_core_debuginfo_p ())
    {
      btf_ext_output ();
      btf_finalize ();
    }
}

#undef TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END bpf_file_end

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


/* Determine whether to warn about lack of return statement in a
   function.  */

static bool
bpf_warn_func_return (tree decl)
{
  /* Naked functions are implemented entirely in assembly, including
     the return instructions.  */
  return lookup_attribute ("naked", DECL_ATTRIBUTES (decl)) == NULL_TREE;
}

#undef TARGET_WARN_FUNC_RETURN
#define TARGET_WARN_FUNC_RETURN bpf_warn_func_return

/* Compute the size of the function's stack frame, including the local
   area and the register-save area.  */

static void
bpf_compute_frame_layout (void)
{
  int stack_alignment = STACK_BOUNDARY / BITS_PER_UNIT;
  int padding_locals;

  /* Set the space used in the stack by local variables.  This is
     rounded up to respect the minimum stack alignment.  */
  cfun->machine->local_vars_size = get_frame_size ();

  padding_locals = cfun->machine->local_vars_size % stack_alignment;
  if (padding_locals)
    padding_locals = stack_alignment - padding_locals;

  cfun->machine->local_vars_size += padding_locals;

  /* Check that the total size of the frame doesn't exceed the limit
     imposed by eBPF.  */
  if (cfun->machine->local_vars_size > bpf_frame_limit)
    {
      static int stack_limit_exceeded = 0;

      if (!stack_limit_exceeded)
	error ("eBPF stack limit exceeded");
      stack_limit_exceeded = 1;
    }
}

#undef TARGET_COMPUTE_FRAME_LAYOUT
#define TARGET_COMPUTE_FRAME_LAYOUT bpf_compute_frame_layout

/* Defined to initialize data for func_info region in .BTF.ext section.  */

static void
bpf_function_prologue (FILE *f ATTRIBUTE_UNUSED)
{
  if (btf_debuginfo_p ())
    btf_add_func_info_for (cfun->decl, current_function_func_begin_label);
}

#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE bpf_function_prologue

/* Expand to the instructions in a function prologue.  This function
   is called when expanding the 'prologue' pattern in bpf.md.  */

void
bpf_expand_prologue (void)
{
  /* The BPF "hardware" provides a fresh new set of registers for each
     called function, some of which are initialized to the values of
     the arguments passed in the first five registers.  In doing so,
     it saves the values of the registers of the caller, and restores
     them upon returning.  Therefore, there is no need to save the
     callee-saved registers here.  In fact, the kernel implementation
     refuses to run programs in which registers are referred before
     being initialized.  */

  /* BPF does not support functions that allocate stack space
     dynamically.  This should have been checked already and an error
     emitted.  */
  gcc_assert (!cfun->calls_alloca);

  /* If we ever need to have a proper prologue here, please mind the
     `naked' function attribute.  */
}

/* Expand to the instructions in a function epilogue.  This function
   is called when expanding the 'epilogue' pattern in bpf.md.  */

void
bpf_expand_epilogue (void)
{
  /* See note in bpf_expand_prologue for an explanation on why we are
     not restoring callee-saved registers in BPF.  */

  if (lookup_attribute ("naked", DECL_ATTRIBUTES (cfun->decl)) != NULL_TREE)
    return;
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

  if (from == ARG_POINTER_REGNUM && to == FRAME_POINTER_REGNUM)
    ret = 0;
  else if (from == STACK_POINTER_REGNUM && to == FRAME_POINTER_REGNUM)
    ret = -(cfun->machine->local_vars_size);
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
bpf_legitimate_address_p (machine_mode mode,
			  rtx x,
			  bool strict,
			  code_helper = ERROR_MARK)
{
  switch (GET_CODE (x))
    {
    case CONST_INT:
      return (mode == FUNCTION_MODE);

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

	/* Check if any of the PLUS operation operands is a CORE unspec, and at
	   least the local value for the offset fits in the 16 bits available
	   in the encoding.  */
	if (bpf_address_base_p (x1, strict)
	    && GET_CODE (x0) == UNSPEC && XINT (x0, 1) == UNSPEC_CORE_RELOC)
	      return IN_RANGE (INTVAL (XVECEXP (x0, 0, 0)), -1 - 0x7fff, 0x7fff);
	if (bpf_address_base_p (x0, strict)
	    && GET_CODE (x1) == UNSPEC && XINT (x1, 1) == UNSPEC_CORE_RELOC)
	      return IN_RANGE (INTVAL (XVECEXP (x1, 0, 0)), -1 - 0x7fff, 0x7fff);

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

static int
bpf_insn_cost (rtx_insn *insn, bool speed ATTRIBUTE_UNUSED)
{
  rtx pat = PATTERN (insn);
  if(GET_CODE (pat) == SET
     && GET_CODE (XEXP (pat, 1)) == UNSPEC
     && XINT (XEXP (pat, 1), 1) == UNSPEC_CORE_RELOC)
    return COSTS_N_INSNS (100);

  return COSTS_N_INSNS (1);
}

#undef TARGET_INSN_COST
#define TARGET_INSN_COST bpf_insn_cost

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
    {
      /* Too many arguments for BPF.  However, if the function is
         gonna be inline for sure, we let it pass.  Otherwise, issue
         an error.  */
      if (!lookup_attribute ("always_inline",
                             DECL_ATTRIBUTES (cfun->decl)))
        error ("too many function arguments for eBPF");
    }

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

const char *
bpf_output_move (rtx *operands, const char *templ)
{
  bpf_output_core_reloc (operands, 2);
  return templ;
}

/* Print register name according to assembly dialect.  In normal
   syntax registers are printed like %rN where N is the register
   number.

   In pseudoc syntax, the register names do not feature a '%' prefix.
   Additionally, the code 'w' denotes that the register should be
   printed as wN instead of rN, where N is the register number, but
   only when the value stored in the operand OP is 32-bit wide.
   Finally, the code 'W' denotes that the register should be printed
   as wN instead of rN, in all cases, regardless of the mode of the
   value stored in the operand.  */

static void
bpf_print_register (FILE *file, rtx op, int code)
{
  if(asm_dialect == ASM_NORMAL)
    fprintf (file, "%s", reg_names[REGNO (op)]);
  else
    {
      if (code == 'W' || (code == 'w' && GET_MODE_SIZE (GET_MODE (op)) <= 4))
	{
	  if (REGNO (op) == BPF_FP)
	    fprintf (file, "w10");
	  else
	    fprintf (file, "w%s", reg_names[REGNO (op)]+2);
	}
      else
	{
	  if (REGNO (op) == BPF_FP)
	    fprintf (file, "r10");
	  else
	    fprintf (file, "%s", reg_names[REGNO (op)]+1);
	}
    }
}

/* Print an instruction operand.  This function is called in the macro
   PRINT_OPERAND defined in bpf.h */

void
bpf_print_operand (FILE *file, rtx op, int code)
{
  switch (GET_CODE (op))
    {
    case REG:
      bpf_print_register (file, op, code);
      break;
    case MEM:
      output_address (GET_MODE (op), XEXP (op, 0));
      break;
    case CONST_DOUBLE:
      if (GET_MODE (op) == VOIDmode)
	{
	  if (CONST_DOUBLE_HIGH (op))
	    fprintf (file, HOST_WIDE_INT_PRINT_DOUBLE_HEX,
		     CONST_DOUBLE_HIGH (op), CONST_DOUBLE_LOW (op));
	  else if (CONST_DOUBLE_LOW (op) < 0)
	    fprintf (file, HOST_WIDE_INT_PRINT_HEX, CONST_DOUBLE_LOW (op));
	  else
	    fprintf (file, HOST_WIDE_INT_PRINT_DEC, CONST_DOUBLE_LOW (op));
	}
      else
	{
	  long vals[2];
	  real_to_target (vals, CONST_DOUBLE_REAL_VALUE (op), GET_MODE (op));
	  vals[0] &= 0xffffffff;
	  vals[1] &= 0xffffffff;
	  if (GET_MODE (op) == SFmode)
	    fprintf (file, "0x%08lx", vals[0]);
	  else if (GET_MODE (op) == DFmode)
	    {
	      /* Note: real_to_target puts vals in target word order.  */
	      if (WORDS_BIG_ENDIAN)
		fprintf (file, "0x%08lx%08lx", vals[0], vals[1]);
	      else
		fprintf (file, "0x%08lx%08lx", vals[1], vals[0]);
	    }
	  else
	    gcc_unreachable ();
	}
      break;
    case UNSPEC:
      if (XINT (op, 1) == UNSPEC_CORE_RELOC)
	bpf_print_operand (file, XVECEXP (op, 0, 0), code);
      else
	gcc_unreachable ();
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
      if (asm_dialect == ASM_NORMAL)
	fprintf (file, "[");
      bpf_print_register (file, addr, 0);
      fprintf (file, asm_dialect == ASM_NORMAL ? "+0]" : "+0");
      break;
    case PLUS:
      {
	rtx op0 = XEXP (addr, 0);
	rtx op1 = XEXP (addr, 1);

	if (GET_CODE (op1) == REG) {
	  op0 = op1;
	  op1 = XEXP (addr, 0);
	}

	if (GET_CODE (op0) == REG
	    && (GET_CODE (op1) == CONST_INT
		|| (GET_CODE (op1) == UNSPEC
		    && XINT (op1, 1) == UNSPEC_CORE_RELOC)))
	  {
	    if (asm_dialect == ASM_NORMAL)
	      fprintf (file, "[");
	    bpf_print_register (file, op0, 0);
	    fprintf (file, "+");
	    if (GET_CODE (op1) == UNSPEC)
	      output_addr_const (file, XVECEXP (op1, 0, 0));
	    else
	      output_addr_const (file, op1);
	    if (asm_dialect == ASM_NORMAL)
	      fprintf (file, "]");
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

static inline tree
def_builtin (const char *name, enum bpf_builtins code, tree type)
{
  tree t
    = add_builtin_function (name, type, code, BUILT_IN_MD, NULL, NULL);

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
  def_builtin ("__builtin_preserve_field_info",
	       BPF_BUILTIN_PRESERVE_FIELD_INFO,
	       build_function_type_list (unsigned_type_node, ptr_type_node,
					 unsigned_type_node, 0));
  def_builtin ("__builtin_btf_type_id",
	       BPF_BUILTIN_BTF_TYPE_ID,
	       build_function_type_list (integer_type_node, ptr_type_node,
					 integer_type_node, 0));
  def_builtin ("__builtin_preserve_type_info",
	       BPF_BUILTIN_PRESERVE_TYPE_INFO,
	       build_function_type_list (integer_type_node, ptr_type_node,
					 integer_type_node, 0));
  def_builtin ("__builtin_preserve_enum_value",
	       BPF_BUILTIN_PRESERVE_ENUM_VALUE,
	       build_function_type_list (integer_type_node, ptr_type_node,
					 integer_type_node, integer_type_node,
					 0));

  def_builtin ("__builtin_core_reloc",
	       BPF_BUILTIN_CORE_RELOC,
	       build_function_type_list (integer_type_node,integer_type_node,
					 0));
  DECL_PURE_P (bpf_builtins[BPF_BUILTIN_CORE_RELOC]) = 1;
  TREE_NOTHROW (bpf_builtins[BPF_BUILTIN_CORE_RELOC]) = 1;

  bpf_init_core_builtins ();
}

#undef TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS bpf_init_builtins

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
  else
    {
      rtx ret = bpf_expand_core_builtin (exp, (enum bpf_builtins) code);
      if (ret != NULL_RTX)
	return ret;
    }

  error ("invalid built-in function at expansion");
  gcc_unreachable ();
}

#undef TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN bpf_expand_builtin

static tree
bpf_resolve_overloaded_builtin (location_t loc, tree fndecl, void *arglist)
{
  int code = DECL_MD_FUNCTION_CODE (fndecl);
  if (code > BPF_CORE_BUILTINS_MARKER)
    return bpf_resolve_overloaded_core_builtin (loc, fndecl, arglist);
  else
    return NULL_TREE;
}

#undef TARGET_RESOLVE_OVERLOADED_BUILTIN
#define TARGET_RESOLVE_OVERLOADED_BUILTIN bpf_resolve_overloaded_builtin

static rtx
bpf_delegitimize_address (rtx rtl)
{
  if (GET_CODE (rtl) == UNSPEC
      && XINT (rtl, 1) == UNSPEC_CORE_RELOC)
    return XVECEXP (rtl, 0, 0);

  return rtl;
}

#undef TARGET_DELEGITIMIZE_ADDRESS
#define TARGET_DELEGITIMIZE_ADDRESS bpf_delegitimize_address

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

/* Implement target hook TARGET_ASM_NAMED_SECTION.  */

static void
bpf_asm_named_section (const char *name, unsigned int flags,
                       tree decl)
{
  /* In BPF section names are used to encode the kind of BPF program
     and other metadata, involving all sort of non alphanumeric
     characters.  This includes for example names like /foo//bar/baz.
     This makes it necessary to quote section names to make sure the
     assembler doesn't get confused.  For example, the example above
     would be interpreted unqouted as a section name "/foo" followed
     by a line comment "//bar/baz".

     Note that we only quote the section name if it contains any
     character not in the set [0-9a-zA-Z_].  This is because
     default_elf_asm_named_section generally expects unquoted names
     and checks for particular names like
     __patchable_function_entries.  */

  bool needs_quoting = false;

  for (const char *p = name; *p != '\0'; ++p)
    if (!(*p == '_'
          || (*p >= '0' && *p <= '9')
          || (*p >= 'a' && *p <= 'z')
          || (*p >= 'A' && *p <= 'Z')))
      needs_quoting = true;

  if (needs_quoting)
    {
      char *quoted_name
        = (char *) xcalloc (1, strlen (name) * 2 + 2);
      char *q = quoted_name;

      *(q++) = '"';
      for (const char *p = name; *p != '\0'; ++p)
        {
          if (*p == '"' || *p == '\\')
            *(q++) = '\\';
          *(q++) = *p;
        }
      *(q++) = '"';
      *(q++) = '\0';

      default_elf_asm_named_section (quoted_name, flags, decl);
      free (quoted_name);
    }
  else
    default_elf_asm_named_section (name, flags, decl);
}

#undef TARGET_ASM_NAMED_SECTION
#define TARGET_ASM_NAMED_SECTION bpf_asm_named_section

/* Implement target hook small_register_classes_for_mode_p.  */

static bool
bpf_small_register_classes_for_mode_p (machine_mode mode)
{
  if (TARGET_XBPF)
    return 1;
  else
    /* Avoid putting function addresses in registers, as calling these
       is not supported in eBPF.  */
    return (mode != FUNCTION_MODE);
}

#undef TARGET_SMALL_REGISTER_CLASSES_FOR_MODE_P
#define TARGET_SMALL_REGISTER_CLASSES_FOR_MODE_P \
  bpf_small_register_classes_for_mode_p

static bool
bpf_use_by_pieces_infrastructure_p (unsigned HOST_WIDE_INT size,
				    unsigned int align ATTRIBUTE_UNUSED,
				    enum by_pieces_operation op,
				    bool speed_p)
{
  if (op != COMPARE_BY_PIECES)
    return default_use_by_pieces_infrastructure_p (size, align, op, speed_p);

  return size <= COMPARE_MAX_PIECES;
}

#undef TARGET_USE_BY_PIECES_INFRASTRUCTURE_P
#define TARGET_USE_BY_PIECES_INFRASTRUCTURE_P \
  bpf_use_by_pieces_infrastructure_p

/* Helper for bpf_expand_cpymem.  Emit an unrolled loop moving the bytes
   from SRC to DST.  */

static void
emit_move_loop (rtx src, rtx dst, machine_mode mode, int offset, int inc,
		unsigned iters, unsigned remainder)
{
  rtx reg = gen_reg_rtx (mode);

  /* First copy in chunks as large as alignment permits.  */
  for (unsigned int i = 0; i < iters; i++)
    {
      emit_move_insn (reg, adjust_address (src, mode, offset));
      emit_move_insn (adjust_address (dst, mode, offset), reg);
      offset += inc;
    }

  /* Handle remaining bytes which might be smaller than the chunks
     used above.  */
  if (remainder & 4)
    {
      emit_move_insn (reg, adjust_address (src, SImode, offset));
      emit_move_insn (adjust_address (dst, SImode, offset), reg);
      offset += (inc < 0 ? -4 : 4);
      remainder -= 4;
    }
  if (remainder & 2)
    {
      emit_move_insn (reg, adjust_address (src, HImode, offset));
      emit_move_insn (adjust_address (dst, HImode, offset), reg);
      offset += (inc < 0 ? -2 : 2);
      remainder -= 2;
    }
  if (remainder & 1)
    {
      emit_move_insn (reg, adjust_address (src, QImode, offset));
      emit_move_insn (adjust_address (dst, QImode, offset), reg);
    }
}

/* Expand cpymem/movmem, as from __builtin_memcpy/memmove.
   OPERANDS are the same as the cpymem/movmem patterns.
   IS_MOVE is true if this is a memmove, false for memcpy.
   Return true if we successfully expanded, or false if we cannot
   and must punt to a libcall.  */

bool
bpf_expand_cpymem (rtx *operands, bool is_move)
{
  /* Size must be constant for this expansion to work.  */
  const char *name = is_move ? "memmove" : "memcpy";
  if (!CONST_INT_P (operands[2]))
    {
      if (flag_building_libgcc)
	warning (0, "could not inline call to %<__builtin_%s%>: "
		 "size must be constant", name);
      else
	error ("could not inline call to %<__builtin_%s%>: "
	       "size must be constant", name);
      return false;
    }

  /* Alignment is a CONST_INT.  */
  gcc_assert (CONST_INT_P (operands[3]));

  rtx dst = operands[0];
  rtx src = operands[1];
  rtx size = operands[2];
  unsigned HOST_WIDE_INT size_bytes = UINTVAL (size);
  unsigned align = UINTVAL (operands[3]);
  enum machine_mode mode;
  switch (align)
    {
    case 1: mode = QImode; break;
    case 2: mode = HImode; break;
    case 4: mode = SImode; break;
    case 8: mode = DImode; break;
    default:
      gcc_unreachable ();
    }

  /* For sizes above threshold, always use a libcall.  */
  if (size_bytes > (unsigned HOST_WIDE_INT) bpf_inline_memops_threshold)
    {
      if (flag_building_libgcc)
	warning (0, "could not inline call to %<__builtin_%s%>: "
		 "too many bytes, use %<-minline-memops-threshold%>", name);
      else
	error ("could not inline call to %<__builtin_%s%>: "
	       "too many bytes, use %<-minline-memops-threshold%>", name);
      return false;
    }

  unsigned iters = size_bytes >> ceil_log2 (align);
  unsigned remainder = size_bytes & (align - 1);

  int inc = GET_MODE_SIZE (mode);
  rtx_code_label *fwd_label, *done_label;
  if (is_move)
    {
      /* For memmove, be careful of overlap.  It is not a concern for memcpy.
	 To handle overlap, we check (at runtime) if SRC < DST, and if so do
	 the move "backwards" starting from SRC + SIZE.  */
      fwd_label = gen_label_rtx ();
      done_label = gen_label_rtx ();

      rtx dst_addr = copy_to_mode_reg (Pmode, XEXP (dst, 0));
      rtx src_addr = copy_to_mode_reg (Pmode, XEXP (src, 0));
      emit_cmp_and_jump_insns (src_addr, dst_addr, GEU, NULL_RTX, Pmode,
			       true, fwd_label, profile_probability::even ());

      /* Emit the "backwards" unrolled loop.  */
      emit_move_loop (src, dst, mode, size_bytes, -inc, iters, remainder);
      emit_jump_insn (gen_jump (done_label));
      emit_barrier ();

      emit_label (fwd_label);
    }

  emit_move_loop (src, dst, mode, 0, inc, iters, remainder);

  if (is_move)
    emit_label (done_label);

  return true;
}

/* Expand setmem, as from __builtin_memset.
   OPERANDS are the same as the setmem pattern.
   Return true if the expansion was successful, false otherwise.  */

bool
bpf_expand_setmem (rtx *operands)
{
  /* Size must be constant for this expansion to work.  */
  if (!CONST_INT_P (operands[1]))
    {
      if (flag_building_libgcc)
	warning (0, "could not inline call to %<__builtin_memset%>: "
		 "size must be constant");
      else
	error ("could not inline call to %<__builtin_memset%>: "
	       "size must be constant");
      return false;
    }

  /* Alignment is a CONST_INT.  */
  gcc_assert (CONST_INT_P (operands[3]));

  rtx dst = operands[0];
  rtx size = operands[1];
  rtx val = operands[2];
  unsigned HOST_WIDE_INT size_bytes = UINTVAL (size);
  unsigned align = UINTVAL (operands[3]);
  enum machine_mode mode;
  switch (align)
    {
    case 1: mode = QImode; break;
    case 2: mode = HImode; break;
    case 4: mode = SImode; break;
    case 8: mode = DImode; break;
    default:
      gcc_unreachable ();
    }

  /* For sizes above threshold, always use a libcall.  */
  if (size_bytes > (unsigned HOST_WIDE_INT) bpf_inline_memops_threshold)
    {
      if (flag_building_libgcc)
	warning (0, "could not inline call to %<__builtin_memset%>: "
		 "too many bytes, use %<-minline-memops-threshold%>");
      else
	error ("could not inline call to %<__builtin_memset%>: "
	       "too many bytes, use %<-minline-memops-threshold%>");
      return false;
    }

  unsigned iters = size_bytes >> ceil_log2 (align);
  unsigned remainder = size_bytes & (align - 1);
  unsigned inc = GET_MODE_SIZE (mode);
  unsigned offset = 0;

  for (unsigned int i = 0; i < iters; i++)
    {
      emit_move_insn (adjust_address (dst, mode, offset), val);
      offset += inc;
    }
  if (remainder & 4)
    {
      emit_move_insn (adjust_address (dst, SImode, offset), val);
      offset += 4;
      remainder -= 4;
    }
  if (remainder & 2)
    {
      emit_move_insn (adjust_address (dst, HImode, offset), val);
      offset += 2;
      remainder -= 2;
    }
  if (remainder & 1)
    emit_move_insn (adjust_address (dst, QImode, offset), val);

  return true;
}

/* Finally, build the GCC target.  */

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-bpf.h"
