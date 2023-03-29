/* Target Code for ft32
   Copyright (C) 2015-2023 Free Software Foundation, Inc.
   Contributed by FTDI <support@ftdi.com>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

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
#include "stringpool.h"
#include "attribs.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "regs.h"
#include "emit-rtl.h"
#include "diagnostic-core.h"
#include "output.h"
#include "stor-layout.h"
#include "calls.h"
#include "expr.h"
#include "builtins.h"
#include "print-tree.h"

/* This file should be included last.  */
#include "target-def.h"

#include <stdint.h>

#define LOSE_AND_RETURN(msgid, x)               \
  do                                            \
    {                                           \
      ft32_operand_lossage (msgid, x);            \
      return;                                   \
    } while (0)

/* Worker function for TARGET_RETURN_IN_MEMORY.  */

static bool
ft32_return_in_memory (const_tree type, const_tree fntype ATTRIBUTE_UNUSED)
{
  const HOST_WIDE_INT size = int_size_in_bytes (type);
  return (size == -1 || size > 2 * UNITS_PER_WORD);
}

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its
   FUNCTION_DECL; otherwise, FUNC is 0.

   We always return values in register $r0 for ft32.  */

static rtx
ft32_function_value (const_tree valtype,
                     const_tree fntype_or_decl ATTRIBUTE_UNUSED,
                     bool outgoing ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (TYPE_MODE (valtype), FT32_R0);
}

/* Define how to find the value returned by a library function.

   We always return values in register $r0 for ft32.  */

static rtx
ft32_libcall_value (machine_mode mode, const_rtx fun ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (mode, FT32_R0);
}

/* Handle TARGET_FUNCTION_VALUE_REGNO_P.

   We always return values in register $r0 for ft32.  */

static bool
ft32_function_value_regno_p (const unsigned int regno)
{
  return (regno == FT32_R0);
}

/* Emit an error message when we're in an asm, and a fatal error for
   "normal" insns.  Formatted output isn't easily implemented, since we
   use output_operand_lossage to output the actual message and handle the
   categorization of the error.  */

static void
ft32_operand_lossage (const char *msgid, rtx op)
{
  debug_rtx (op);
  output_operand_lossage ("%s", msgid);
}

/* The PRINT_OPERAND_ADDRESS worker.  */

void
ft32_print_operand_address (FILE * file, rtx x)
{
  switch (GET_CODE (x))
    {
    case REG:
      fprintf (file, "%s,0", reg_names[REGNO (x)]);
      break;

    case PLUS:
      switch (GET_CODE (XEXP (x, 1)))
        {
        case CONST_INT:
          fprintf (file, "%s,%ld",
                   reg_names[REGNO (XEXP (x, 0))], INTVAL (XEXP (x, 1)));
          break;
        case SYMBOL_REF:
          output_addr_const (file, XEXP (x, 1));
          fprintf (file, "(%s)", reg_names[REGNO (XEXP (x, 0))]);
          break;
        case CONST:
          {
            rtx plus = XEXP (XEXP (x, 1), 0);
            if (GET_CODE (XEXP (plus, 0)) == SYMBOL_REF
                && CONST_INT_P (XEXP (plus, 1)))
              {
                output_addr_const (file, XEXP (plus, 0));
                fprintf (file, "+%ld(%s)", INTVAL (XEXP (plus, 1)),
                         reg_names[REGNO (XEXP (x, 0))]);
              }
            else
              abort ();
          }
          break;
        default:
          abort ();
        }
      break;

    default:
      output_addr_const (file, x);
      break;
    }
}

/* The PRINT_OPERAND worker.  */

void
ft32_print_operand (FILE * file, rtx x, int code)
{
  rtx operand = x;

  /* New code entries should just be added to the switch below.  If
     handling is finished, just return.  If handling was just a
     modification of the operand, the modified operand should be put in
     "operand", and then do a break to let default handling
     (zero-modifier) output the operand.  */

  switch (code)
    {
    case 0:
      /* No code, print as usual.  */
      break;

    case 'h':
      if (GET_CODE (operand) != REG)
	internal_error ("%<h%> applied to non-register operand");
      fprintf (file, "%s", reg_names[REGNO (operand) + 1]);
      return;

    case 'm':
      fprintf (file, "%ld", (long) (- INTVAL(x)));
      return;

    case 'd':                   // a DW spec, from an integer alignment (for BLKmode insns)
      {
        int i = INTVAL (x);
        char dwspec;
        switch (i)
          {
          case 1:
            dwspec = 'b';
            break;
          case 2:
            dwspec = 's';
            break;
          case 4:
            dwspec = 'l';
            break;
          default:
            if ((i % 4) != 0)
              internal_error ("bad alignment: %d", i);
            else
              dwspec = 'l';
            break;
          }
        fprintf (file, "%c", dwspec);
        return;
      }

    case 'f':
      {
        int bf = ft32_as_bitfield (INTVAL (x));
        fprintf (file, "512|(%d<<5)|%d", bf >> 5, bf & 31);
        return;
      }

    case 'g':
      {
        int bf = ft32_as_bitfield (0xffffffff ^ INTVAL (x));
        fprintf (file, "(%d<<5)|%d", bf >> 5, bf & 31);
        return;
      }

    case 'b':
      {
        ft32_print_operand (file, XEXP (x, 0), 0);
        return;
      }

    default:
      LOSE_AND_RETURN ("invalid operand modifier letter", x);
    }

  /* Print an operand as without a modifier letter.  */
  switch (GET_CODE (operand))
    {
    case REG:
      fprintf (file, "%s", reg_names[REGNO (operand)]);
      return;

    case MEM:
      output_address (GET_MODE (XEXP (operand, 0)), XEXP (operand, 0));
      return;

    default:
      /* No need to handle all strange variants, let output_addr_const
         do it for us.  */
      if (CONSTANT_P (operand))
        {
          output_addr_const (file, operand);
          return;
        }

      LOSE_AND_RETURN ("unexpected operand", x);
    }
}

const char *
ft32_load_immediate (rtx dst, int32_t i)
{
  char pattern[100];

  if (i >= -524288 && i <= 524287)
    {
      sprintf (pattern, "ldk.l  %%0,%d", i);
      output_asm_insn (pattern, &dst);
    }
  else if (i >= -536870912 && i <= 536870911)
    {
      ft32_load_immediate (dst, i >> 10);
      sprintf (pattern, "ldl.l  %%0,%%0,%d", i & 1023);
      output_asm_insn (pattern, &dst);
    }
  else
    {
      int rd;                   // rotate distance
      uint32_t u = i;
      for (rd = 1; rd < 32; rd++)
        {
          u = ((u >> 31) & 1) | (u << 1);
	  if ((int32_t) u >= -524288 && (int32_t) u <= 524287)
            {
              ft32_load_immediate (dst, (int32_t) u);
              sprintf (pattern, "ror.l  %%0,%%0,%d", rd);
              output_asm_insn (pattern, &dst);
              return "";
            }
        }
      ft32_load_immediate (dst, i >> 10);
      sprintf (pattern, "ldl.l  %%0,%%0,%d", i & 1023);
      output_asm_insn (pattern, &dst);
    }

  return "";
}

// x is a bit mask, for example:
//    00000000000000000000001111111110
// If x contains a single bit mask, return the bitfield spec.
// in the above case it returns ((9 << 5) | 1)
// Otherwise return -1.
//

#define NBITS(n)  ((1U << (n)) - 1U)

int
ft32_as_bitfield (unsigned int x)
{
  int lobit, hibit;

  if (x == 0)
    return -1;

  for (lobit = 0; lobit < 32; lobit++)
    if (x & (1 << lobit))
      break;
  for (hibit = 31; hibit >= 0; hibit--)
    if (x & (1 << hibit))
      break;

  int width = 1 + hibit - lobit;
  if (width > 16)
    return -1;

  if (x != (NBITS (width) << lobit))
    return -1;                  // not a clean bitfield

  return ((width & 15) << 5) | lobit;
}

/* Per-function machine data.  */
struct GTY (()) machine_function
{
  /* Number of bytes saved on the stack for callee saved registers.  */
  int callee_saved_reg_size;

  /* Number of bytes saved on the stack for local variables.  */
  int local_vars_size;

  /* The sum of 2 sizes: locals vars and padding byte for saving the
   * registers.  Used in expand_prologue () and expand_epilogue ().  */
  int size_for_adjusting_sp;
};

/* Zero initialization is OK for all current fields.  */

static struct machine_function *
ft32_init_machine_status (void)
{
  return ggc_cleared_alloc < machine_function > ();
}


/* The TARGET_OPTION_OVERRIDE worker.
   All this curently does is set init_machine_status.  */
static void
ft32_option_override (void)
{
  /* Set the per-function-data initializer.  */
  init_machine_status = ft32_init_machine_status;
}

/* Implement targetm.select_section.  */
static section *
ft32_select_section (tree decl, int reloc, unsigned HOST_WIDE_INT align)
{
  /* Variables and constants defined in the __ea address space
     go into a special section named "._ea".  */
  if (TREE_TYPE (decl) != error_mark_node
      && TYPE_ADDR_SPACE (TREE_TYPE (decl)) == ADDR_SPACE_PM)
    {
      /* We might get called with string constants, but get_named_section
         doesn't like them as they are not DECLs.  Also, we need to set
         flags in that case.  */
      if (!DECL_P (decl))
        return get_section ("._pm", SECTION_WRITE | SECTION_DEBUG, NULL);

      return get_named_section (decl, "._pm", reloc);
    }

  return default_elf_select_section (decl, reloc, align);
}

/* Compute the size of the local area and the size to be adjusted by the
 * prologue and epilogue.  */

static void
ft32_compute_frame (void)
{
  /* For aligning the local variables.  */
  int stack_alignment = STACK_BOUNDARY / BITS_PER_UNIT;
  int padding_locals;
  int regno;

  /* Padding needed for each element of the frame.  */
  cfun->machine->local_vars_size = get_frame_size ();

  /* Align to the stack alignment.  */
  padding_locals = cfun->machine->local_vars_size % stack_alignment;
  if (padding_locals)
    padding_locals = stack_alignment - padding_locals;

  cfun->machine->local_vars_size += padding_locals;

  cfun->machine->callee_saved_reg_size = 0;

  /* Save callee-saved registers.  */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (df_regs_ever_live_p (regno) && !call_used_or_fixed_reg_p (regno))
      cfun->machine->callee_saved_reg_size += 4;

  cfun->machine->size_for_adjusting_sp =
    0 // crtl->args.pretend_args_size
    + cfun->machine->local_vars_size
    + (ACCUMULATE_OUTGOING_ARGS
       ? (HOST_WIDE_INT) crtl->outgoing_args_size : 0);
}

// Must use LINK/UNLINK when...
// the frame is bigger than 512 bytes so cannot just "SUB" from SP
// the function actually uses $fp

static int
must_link (void)
{
  int bigframe = (cfun->machine->size_for_adjusting_sp >= 512);
  return (bigframe || frame_pointer_needed || df_regs_ever_live_p (FT32_FP)
          || df_regs_ever_live_p (FT32_FP));
}

void
ft32_expand_prologue (void)
{
  int regno;
  rtx insn;

  ft32_compute_frame ();

  int args_to_push = crtl->args.pretend_args_size;
  if (args_to_push)
    {
      int i;

      insn = emit_insn (gen_movsi_pop ((gen_rtx_REG (Pmode, FT32_R29))));

      for (i = 0; i < (args_to_push / 4); i++)
	{
	  insn =
	    emit_insn (gen_movsi_push ((gen_rtx_REG (Pmode, FT32_R5 - i))));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}

      insn = emit_insn (gen_movsi_push ((gen_rtx_REG (Pmode, FT32_R29))));
    }

  if (flag_stack_usage_info)
    current_function_static_stack_size = cfun->machine->size_for_adjusting_sp;

  if (!must_link () && (cfun->machine->callee_saved_reg_size == 4))
    {
      insn =
	emit_insn (gen_link
		   (gen_rtx_REG (Pmode, FT32_R13),
		    GEN_INT (-cfun->machine->size_for_adjusting_sp)));
      RTX_FRAME_RELATED_P (insn) = 1;
      return;
    }
  /* Save callee-saved registers.  */
  if (optimize_size)
    {
      for (regno = FIRST_PSEUDO_REGISTER; regno-- > 0;)
	{
	  if (!call_used_or_fixed_reg_p (regno)
	      && df_regs_ever_live_p (regno))
	    {
	      rtx preg = gen_rtx_REG (Pmode, regno);
	      emit_insn (gen_call_prolog (preg));
	      break;
	    }
	}
    }
  else
    {
      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	{
	  if (df_regs_ever_live_p (regno)
	      && !call_used_or_fixed_reg_p (regno))
	    {
	      insn = emit_insn (gen_movsi_push (gen_rtx_REG (Pmode, regno)));
	      RTX_FRAME_RELATED_P (insn) = 1;
	    }
	}
    }

  if (cfun->machine->size_for_adjusting_sp >= 65536)
    {
      error ("stack frame must be smaller than 64K");
      return;
    }
  if (must_link ())
    {
      insn =
	emit_insn (gen_link
		   (gen_rtx_REG (Pmode, FT32_FP),
		    GEN_INT (-cfun->machine->size_for_adjusting_sp)));
      RTX_FRAME_RELATED_P (insn) = 1;
    }
  else if (cfun->machine->size_for_adjusting_sp > 0)
    {
      int adj = cfun->machine->size_for_adjusting_sp;
      insn = emit_insn (gen_addsi3 (gen_rtx_REG (SImode, FT32_SP),
				    gen_rtx_REG (SImode, FT32_SP),
				    GEN_INT (-adj)));
      RTX_FRAME_RELATED_P (insn) = 1;
    }
}

void
ft32_expand_epilogue (void)
{
  int regno;
  int pretend = crtl->args.pretend_args_size;

  if (!must_link ()
      && (cfun->machine->size_for_adjusting_sp == 24)
      && (cfun->machine->callee_saved_reg_size == 0))
    {
      emit_jump_insn (gen_returner24 ());
      return;
    }

  // Set when the epilog code will also add 24 to $sp
  int epilog24 = (!must_link ()
                  && (cfun->machine->size_for_adjusting_sp == 24)
                  && optimize_size);

  if (must_link ())
    {
      emit_insn (gen_unlink ());
    }
  else if (!epilog24 && (cfun->machine->size_for_adjusting_sp > 0))
    {
      emit_insn (gen_addsi3 (gen_rtx_REG (SImode, FT32_SP),
                             gen_rtx_REG (SImode, FT32_SP),
                             GEN_INT (cfun->machine->size_for_adjusting_sp)));
    }

  if (cfun->machine->callee_saved_reg_size != 0)
    {
      for (regno = FIRST_PSEUDO_REGISTER; regno-- > 0;)
        {
          if (!call_used_or_fixed_reg_p (regno)
              && df_regs_ever_live_p (regno))
            {
              rtx preg = gen_rtx_REG (Pmode, regno);
              if (optimize_size && (pretend == 0))
                {
                  if (epilog24)
                    emit_insn (gen_jump_epilog24 (preg));
                  else
                    emit_insn (gen_jump_epilog (preg));
                  return;
                }
              emit_insn (gen_movsi_pop (preg));
            }
        }
    }

  if (pretend != 0)
    emit_jump_insn (gen_pretend_returner (GEN_INT (pretend)));
  else
    emit_jump_insn (gen_returner ());
}

#undef TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED ft32_frame_pointer_required
static bool
ft32_frame_pointer_required (void)
{
  return cfun->calls_alloca;
}

#undef  TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE ft32_can_eliminate

/* Return true if register FROM can be eliminated via register TO.  */

static bool
ft32_can_eliminate (const int from ATTRIBUTE_UNUSED, const int to)
{
  return 1;
  return (to == FRAME_POINTER_REGNUM) || !ft32_frame_pointer_required ();
}

/* Implements the macro INITIAL_ELIMINATION_OFFSET, return the OFFSET.  */

int
ft32_initial_elimination_offset (int from, int to)
{
  ft32_compute_frame ();

  if (from == ARG_POINTER_REGNUM && to == FRAME_POINTER_REGNUM)
    {
      return cfun->machine->callee_saved_reg_size + 2 * UNITS_PER_WORD;
    }

  if (from == ARG_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    {
      int arg_offset;
      arg_offset = must_link ()? 2 : 1;
      return ((cfun->machine->callee_saved_reg_size
               + arg_offset * UNITS_PER_WORD)
              + cfun->machine->size_for_adjusting_sp);
    }

  if ((from == FRAME_POINTER_REGNUM) && (to == STACK_POINTER_REGNUM))
    {
      return cfun->machine->size_for_adjusting_sp;
    }

  gcc_unreachable ();
}

/* Worker function for TARGET_SETUP_INCOMING_VARARGS.  */

static void
ft32_setup_incoming_varargs (cumulative_args_t cum_v,
			     const function_arg_info &arg,
			     int *pretend_size, int no_rtl ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int named_size = 0;
  if (!TYPE_NO_NAMED_ARGS_STDARG_P (TREE_TYPE (current_function_decl)))
    named_size =
      GET_MODE_SIZE (SImode) * (*cum - FT32_R0) + GET_MODE_SIZE (arg.mode);

  if (named_size < 24)
    *pretend_size = 24 - named_size;
  else
    *pretend_size = 0;
}

/* Return the fixed registers used for condition codes.  */

static bool
ft32_fixed_condition_code_regs (unsigned int *p1, unsigned int *p2)
{
  *p1 = CC_REG;
  *p2 = INVALID_REGNUM;
  return true;
}

/* Return the next register to be used to hold a function argument or
   NULL_RTX if there's no more space.  */

static rtx
ft32_function_arg (cumulative_args_t cum_v, const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  if (*cum < 8)
    return gen_rtx_REG (arg.mode, *cum);
  else
    return NULL_RTX;
}

#define FT32_FUNCTION_ARG_SIZE(MODE, TYPE)      \
  ((MODE) != BLKmode ? GET_MODE_SIZE (MODE)     \
   : (unsigned) int_size_in_bytes (TYPE))

static void
ft32_function_arg_advance (cumulative_args_t cum_v,
			   const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  *cum = (*cum < FT32_R6
	  ? *cum + ((3 + FT32_FUNCTION_ARG_SIZE (arg.mode, arg.type)) / 4)
	  : *cum);
}

/* Return non-zero if the function argument described by ARG is to be
   passed by reference.  */

static bool
ft32_pass_by_reference (cumulative_args_t, const function_arg_info &arg)
{
  if (arg.aggregate_type_p ())
    return true;
  unsigned HOST_WIDE_INT size = arg.type_size_in_bytes ();
  return size > 4 * 6;
}

/* Some function arguments will only partially fit in the registers
   that hold arguments.  Given a new arg, return the number of bytes
   that fit in argument passing registers.  */

static int
ft32_arg_partial_bytes (cumulative_args_t cum_v, const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int bytes_left, size;

  if (*cum >= 8)
    return 0;

  if (ft32_pass_by_reference (cum_v, arg))
    size = 4;
  else if (arg.type)
    {
      if (AGGREGATE_TYPE_P (arg.type))
        return 0;
      size = int_size_in_bytes (arg.type);
    }
  else
    size = GET_MODE_SIZE (arg.mode);

  bytes_left = (4 * 6) - ((*cum - 2) * 4);

  if (size > bytes_left)
    return bytes_left;
  else
    return 0;
}

/* Used by constraints.md to distinguish between GENERIC and PM
   memory addresses.  */

int
ft32_is_mem_pm (rtx o)
{
  return (MEM_P (o)
          && !ADDR_SPACE_GENERIC_P (MEM_ADDR_SPACE (o)));
}

/* The Global `targetm' Variable.  */

/* Initialize the GCC target structure.  */

#undef  TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES       hook_bool_const_tree_true

#undef  TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY         ft32_return_in_memory
#undef  TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK       must_pass_in_stack_var_size
#undef  TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE        ft32_pass_by_reference
#undef  TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES        ft32_arg_partial_bytes
#undef  TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG             ft32_function_arg
#undef  TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE     ft32_function_arg_advance


#undef  TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS   ft32_setup_incoming_varargs

#undef  TARGET_FIXED_CONDITION_CODE_REGS
#define TARGET_FIXED_CONDITION_CODE_REGS ft32_fixed_condition_code_regs

/* Define this to return an RTX representing the place where a
   function returns or receives a value of data type RET_TYPE, a tree
   node representing a data type.  */
#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE ft32_function_value
#undef TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE ft32_libcall_value
#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P ft32_function_value_regno_p

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE ft32_option_override

#undef TARGET_ASM_SELECT_SECTION
#define TARGET_ASM_SELECT_SECTION  ft32_select_section

#undef TARGET_VALID_POINTER_MODE
#define TARGET_VALID_POINTER_MODE ft32_valid_pointer_mode
static bool
ft32_valid_pointer_mode (scalar_int_mode mode)
{
  if (mode == SImode)
    return 1;
  return 0;
}

#undef TARGET_ADDR_SPACE_POINTER_MODE
#define TARGET_ADDR_SPACE_POINTER_MODE ft32_addr_space_pointer_mode
static scalar_int_mode
ft32_addr_space_pointer_mode (addr_space_t addrspace ATTRIBUTE_UNUSED)
{
  return Pmode;
}

#undef TARGET_ADDR_SPACE_ADDRESS_MODE
#define TARGET_ADDR_SPACE_ADDRESS_MODE ft32_addr_space_address_mode
static scalar_int_mode
ft32_addr_space_address_mode (addr_space_t addrspace ATTRIBUTE_UNUSED)
{
  return Pmode;
}

#undef TARGET_ADDR_SPACE_SUBSET_P
#define TARGET_ADDR_SPACE_SUBSET_P ft32_addr_space_subset_p
static bool
ft32_addr_space_subset_p (addr_space_t subset ATTRIBUTE_UNUSED,
                          addr_space_t superset ATTRIBUTE_UNUSED)
{
  return false;
}

#undef TARGET_CASE_VALUES_THRESHOLD
#define TARGET_CASE_VALUES_THRESHOLD ft32_target_case_values_threshold

static unsigned int
ft32_target_case_values_threshold (void)
{
  return 4;
}

#undef TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P
#define TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P \
  ft32_addr_space_legitimate_address_p


// Enabling LRA gives the infamous
//    internal compiler error: Max. number of generated reload insns per insn is achieved (90)
// errors e.g. when compiling sieve.c

static bool
ft32_lra_p (void)
{
  return ft32_lra_flag;
}

#undef TARGET_LRA_P
#define TARGET_LRA_P ft32_lra_p

static bool
reg_ok_for_base_p (rtx r, bool strict)
{
  int NUM = REGNO (r);
  if (strict)
    return (HARD_REGNO_OK_FOR_BASE_P (NUM)
            || HARD_REGNO_OK_FOR_BASE_P (reg_renumber[(NUM)]));
  else
    return ((NUM) >= FIRST_PSEUDO_REGISTER || HARD_REGNO_OK_FOR_BASE_P (NUM));
}

static bool
ft32_addr_space_legitimate_address_p (machine_mode mode, rtx x, bool strict,
                                      addr_space_t as ATTRIBUTE_UNUSED)
{
  int max_offset = TARGET_FT32B ? 16384 : 128;

  if (mode != BLKmode)
    {
      if (GET_CODE (x) == PLUS)
        {
          rtx op1, op2;
          op1 = XEXP (x, 0);
          op2 = XEXP (x, 1);
          if (GET_CODE (op1) == REG
              && CONST_INT_P (op2)
              && (-max_offset <= INTVAL (op2))
              && (INTVAL (op2) < max_offset)
              && reg_ok_for_base_p (op1, strict))
            goto yes;
          if (GET_CODE (op1) == SYMBOL_REF && CONST_INT_P (op2))
            goto yes;
        }
      if (REG_P (x) && reg_ok_for_base_p (x, strict))
        goto yes;
      if (GET_CODE (x) == SYMBOL_REF
          || GET_CODE (x) == LABEL_REF || CONST_INT_P (x))
        goto yes;
    }
  else
    {
      if (REG_P (x) && reg_ok_for_base_p (x, strict))
        goto yes;
    }

  return 0;
yes:
  return 1;
}

#undef TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO  ft32_elf_encode_section_info

void
ft32_elf_encode_section_info (tree decl, rtx rtl, int first)
{
  enum tree_code code;
  rtx symbol;

  /* Careful not to prod global register variables.  */
  if (!MEM_P (rtl))
    return;
  symbol = XEXP (rtl, 0);
  if (GET_CODE (symbol) != SYMBOL_REF)
    return;

  default_encode_section_info (decl, rtl, first);

  code = TREE_CODE (decl);
  switch (TREE_CODE_CLASS (code))
    {
    case tcc_declaration:
      {
	tree type = TREE_TYPE (decl);
	int is_flash = (type && TYPE_P (type)
			&& !ADDR_SPACE_GENERIC_P (TYPE_ADDR_SPACE (type)));
	if ((code == VAR_DECL) && !is_flash)
	  SYMBOL_REF_FLAGS (symbol) |= 0x1000;
      }
      break;

    case tcc_constant:
    case tcc_exceptional:
      if (code == STRING_CST)
	SYMBOL_REF_FLAGS (symbol) |= 0x1000;
      break;

    default:
      break;
    }
}

#undef TARGET_CONSTANT_ALIGNMENT
#define TARGET_CONSTANT_ALIGNMENT constant_alignment_word_strings

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-ft32.h"
