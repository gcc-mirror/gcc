/* Output routines for Sunplus S+CORE processor
   Copyright (C) 2005, 2007, 2008, 2009, 2010 Free Software Foundation, Inc.
   Contributed by Sunnorth.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-attr.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "output.h"
#include "tree.h"
#include "function.h"
#include "expr.h"
#include "optabs.h"
#include "flags.h"
#include "reload.h"
#include "tm_p.h"
#include "ggc.h"
#include "gstab.h"
#include "hashtab.h"
#include "debug.h"
#include "target.h"
#include "target-def.h"
#include "integrate.h"
#include "langhooks.h"
#include "score7.h"
#include "df.h"

static void score_option_override (void);

/* Implement TARGET_OPTION_OPTIMIZATION_TABLE.  */
static const struct default_options score_option_optimization_table[] =
  {
    { OPT_LEVELS_1_PLUS, OPT_fomit_frame_pointer, NULL, 1 },
    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };

#undef  TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START           score_asm_file_start

#undef  TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END             score_asm_file_end

#undef  TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE    score_function_prologue

#undef  TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE    score_function_epilogue

#undef TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS     TARGET_DEFAULT

#undef TARGET_HANDLE_OPTION
#define TARGET_HANDLE_OPTION            score_handle_option

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE          score_option_override

#undef TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE score_option_optimization_table

#undef TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS	score_legitimize_address

#undef  TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE         score_issue_rate

#undef TARGET_ASM_SELECT_RTX_SECTION
#define TARGET_ASM_SELECT_RTX_SECTION   score_select_rtx_section

#undef  TARGET_IN_SMALL_DATA_P
#define TARGET_IN_SMALL_DATA_P          score_in_small_data_p

#undef  TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL  score_function_ok_for_sibcall

#undef TARGET_STRICT_ARGUMENT_NAMING
#define TARGET_STRICT_ARGUMENT_NAMING   hook_bool_CUMULATIVE_ARGS_true

#undef TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK      score_output_mi_thunk

#undef TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE    default_promote_function_mode_always_promote

#undef TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES       hook_bool_const_tree_true

#undef TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK       must_pass_in_stack_var_size

#undef TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES        score_arg_partial_bytes

#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG             score_function_arg

#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE     score_function_arg_advance

#undef TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE        score_pass_by_reference

#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY         score_return_in_memory

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS                score_rtx_costs

#undef TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST             score_address_cost

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P	score_legitimate_address_p

#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE            score_can_eliminate

#undef TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE score_conditional_register_usage

#undef TARGET_ASM_TRAMPOLINE_TEMPLATE
#define TARGET_ASM_TRAMPOLINE_TEMPLATE	score_asm_trampoline_template
#undef TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT		score_trampoline_init

struct extern_list *extern_head = 0;

/* default 0 = NO_REGS  */
enum reg_class score_char_to_class[256];

/* Implement TARGET_RETURN_IN_MEMORY.  In S+core,
   small structures are returned in a register.
   Objects with varying size must still be returned in memory.  */
static bool
score_return_in_memory (const_tree type, const_tree fndecl ATTRIBUTE_UNUSED)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_return_in_memory (type, fndecl);
  else
    gcc_unreachable ();
}

/* Return nonzero when an argument must be passed by reference.  */
static bool
score_pass_by_reference (CUMULATIVE_ARGS *cum ATTRIBUTE_UNUSED,
                         enum machine_mode mode, const_tree type,
                         bool named ATTRIBUTE_UNUSED)
{
  /* If we have a variable-sized parameter, we have no choice.  */
  return targetm.calls.must_pass_in_stack (mode, type);
}

/* Implement TARGET_ASM_OUTPUT_MI_THUNK.  Generate rtl rather than asm text
   in order to avoid duplicating too much logic from elsewhere.  */
static void
score_output_mi_thunk (FILE *file, tree thunk_fndecl ATTRIBUTE_UNUSED,
                       HOST_WIDE_INT delta, HOST_WIDE_INT vcall_offset,
                       tree function)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    score7_output_mi_thunk (file, thunk_fndecl, delta, vcall_offset, function);
  else
    gcc_unreachable ();
}

/* Implement TARGET_FUNCTION_OK_FOR_SIBCALL.  */
static bool
score_function_ok_for_sibcall (ATTRIBUTE_UNUSED tree decl,
                               ATTRIBUTE_UNUSED tree exp)
{
  return true;
}

/* Set up the stack and frame (if desired) for the function.  */
static void
score_function_prologue (FILE *file, HOST_WIDE_INT size ATTRIBUTE_UNUSED)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    score7_function_prologue (file, size);
  else
    gcc_unreachable ();
}

/* Do any necessary cleanup after a function to restore stack, frame,
   and regs.  */
static void
score_function_epilogue (FILE *file,
                         HOST_WIDE_INT size ATTRIBUTE_UNUSED)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    score7_function_epilogue (file, size);
  else
    gcc_unreachable ();
}

/* Implement TARGET_SCHED_ISSUE_RATE.  */
static int
score_issue_rate (void)
{
  return 1;
}

/* Choose the section to use for the constant rtx expression X that has
   mode MODE.  */
static section *
score_select_rtx_section (enum machine_mode mode, rtx x,
                          unsigned HOST_WIDE_INT align)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_select_rtx_section (mode, x, align);
  else
    gcc_unreachable ();
}

/* Implement TARGET_IN_SMALL_DATA_P.  */
static bool
score_in_small_data_p (const_tree decl)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_in_small_data_p (decl);
  else
    gcc_unreachable ();
}

/* Implement TARGET_ASM_FILE_START.  */
static void
score_asm_file_start (void)
{
  if (TARGET_SCORE7D)
    fprintf (asm_out_file, "# Sunplus S+core7d %s rev=%s\n",
             TARGET_LITTLE_ENDIAN ? "el" : "eb", SCORE_GCC_VERSION);
  else if (TARGET_SCORE7)
    fprintf (asm_out_file, "# Sunplus S+core7 %s rev=%s\n",
             TARGET_LITTLE_ENDIAN ? "el" : "eb", SCORE_GCC_VERSION);
  else
    fprintf (asm_out_file, "# Sunplus S+core unknown %s rev=%s\n",
             TARGET_LITTLE_ENDIAN ? "el" : "eb", SCORE_GCC_VERSION);

  default_file_start ();

  if (flag_pic)
    fprintf (asm_out_file, "\t.set pic\n");
}

/* Implement TARGET_ASM_FILE_END.  When using assembler macros, emit
   .externs for any small-data variables that turned out to be external.  */
static void
score_asm_file_end (void)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    score7_asm_file_end ();
  else
    gcc_unreachable ();
}

#define MASK_ALL_CPU_BITS	(MASK_SCORE7 | MASK_SCORE7D)

/* Implement TARGET_HANDLE_OPTION.  */
static bool
score_handle_option (size_t code, const char *arg, int value ATTRIBUTE_UNUSED)
{
  switch (code)
    {
    case OPT_mscore7d:
      target_flags &= ~(MASK_ALL_CPU_BITS);
      target_flags |= MASK_SCORE7 | MASK_SCORE7D;
      return true;

    case OPT_march_:
      if (strcmp (arg, "score7") == 0)
        {
          target_flags &= ~(MASK_ALL_CPU_BITS);
          target_flags |= MASK_SCORE7;
          return true;
        }
      else if (strcmp (arg, "score7d") == 0)
        {
          target_flags &= ~(MASK_ALL_CPU_BITS);
          target_flags |= MASK_SCORE7 | MASK_SCORE7D;
          return true;
        }
      else
        return false;

    default:
      return true;
    }
}

/* Implement TARGET_OPTION_OVERRIDE hook.  */
static void
score_option_override (void)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    score7_option_override ();
}

/* Implement REGNO_REG_CLASS macro.  */
int
score_reg_class (int regno)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_reg_class (regno);
  else
    gcc_unreachable ();
}

/* Implement PREFERRED_RELOAD_CLASS macro.  */
enum reg_class
score_preferred_reload_class (rtx x ATTRIBUTE_UNUSED, enum reg_class rclass)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_preferred_reload_class (x, rclass);
  else
    gcc_unreachable ();
}

/* Implement SECONDARY_INPUT_RELOAD_CLASS
   and SECONDARY_OUTPUT_RELOAD_CLASS macro.  */
enum reg_class
score_secondary_reload_class (enum reg_class rclass,
                              enum machine_mode mode ATTRIBUTE_UNUSED,
                              rtx x)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_secondary_reload_class (rclass, mode, x);
  else
    gcc_unreachable ();
}


/* Return truth value on whether or not a given hard register
   can support a given mode.  */
int
score_hard_regno_mode_ok (unsigned int regno, enum machine_mode mode)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_hard_regno_mode_ok (regno, mode);
  else
    gcc_unreachable ();
}

/* We can always eliminate to the hard frame pointer.  We can eliminate
   to the stack pointer unless a frame pointer is needed.  */

static bool
score_can_eliminate (const int from ATTRIBUTE_UNUSED, const int to)
{
  return (to == HARD_FRAME_POINTER_REGNUM
          || (to  == STACK_POINTER_REGNUM && !frame_pointer_needed));
}

/* Implement INITIAL_ELIMINATION_OFFSET.  FROM is either the frame
   pointer or argument pointer.  TO is either the stack pointer or
   hard frame pointer.  */
HOST_WIDE_INT
score_initial_elimination_offset (int from,
                                  int to ATTRIBUTE_UNUSED)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_initial_elimination_offset (from, to);
  else
    gcc_unreachable ();
}

/* Argument support functions.  */

/* Initialize CUMULATIVE_ARGS for a function.  */
void
score_init_cumulative_args (CUMULATIVE_ARGS *cum,
                            tree fntype ATTRIBUTE_UNUSED,
                            rtx libname ATTRIBUTE_UNUSED)
{
  memset (cum, 0, sizeof (CUMULATIVE_ARGS));
}

/* Implement TARGET_FUNCTION_ARG_ADVANCE hook.  */
static void
score_function_arg_advance (CUMULATIVE_ARGS *cum, enum machine_mode mode,
                            const_tree type, bool named)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    score7_function_arg_advance (cum, mode, type, named);
  else
    gcc_unreachable ();
}

/* Implement TARGET_ARG_PARTIAL_BYTES macro.  */
int
score_arg_partial_bytes (CUMULATIVE_ARGS *cum,
                         enum machine_mode mode, tree type, bool named)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_arg_partial_bytes (cum, mode, type, named);
  else
    gcc_unreachable ();
}

/* Implement TARGET_FUNCTION_ARG hook.  */
static rtx
score_function_arg (CUMULATIVE_ARGS *cum, enum machine_mode mode,
                    const_tree type, bool named)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_function_arg (cum, mode, type, named);
  else
    gcc_unreachable ();
}

/* Implement FUNCTION_VALUE and LIBCALL_VALUE.  For normal calls,
   VALTYPE is the return type and MODE is VOIDmode.  For libcalls,
   VALTYPE is null and MODE is the mode of the return value.  */
rtx
score_function_value (const_tree valtype, const_tree func ATTRIBUTE_UNUSED,
                      enum machine_mode mode)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_function_value (valtype, func, mode);
  else
    gcc_unreachable ();
}

/* Implement TARGET_ASM_TRAMPOLINE_TEMPLATE.  */
static void
score_asm_trampoline_template (FILE *f)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    score7_asm_trampoline_template (f);
  else
    gcc_unreachable ();
}

/* Implement TARGET_TRAMPOLINE_INIT.  */
static void
score_trampoline_init (rtx m_tramp, tree fndecl, rtx chain_value)
{
  if ( TARGET_SCORE7 || TARGET_SCORE7D)
    score7_trampoline_init (m_tramp, fndecl, chain_value);
  else  
    gcc_unreachable ();
}

/* This function is used to implement REG_MODE_OK_FOR_BASE_P macro.  */
int
score_regno_mode_ok_for_base_p (int regno, int strict)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_regno_mode_ok_for_base_p (regno, strict);
  else
    gcc_unreachable ();
}

/* Implement TARGET_LEGITIMIZE_ADDRESS_P.  */
static bool
score_legitimate_address_p (enum machine_mode mode, rtx x, bool strict)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_legitimate_address_p (mode, x, strict);
  else
    gcc_unreachable ();
}

/* This function is used to implement LEGITIMIZE_ADDRESS.  If X can
   be legitimized in a way that the generic machinery might not expect,
   return the new address, else return X.  */
static rtx
score_legitimize_address (rtx x, rtx oldx ATTRIBUTE_UNUSED,
			  enum machine_mode mode ATTRIBUTE_UNUSED)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_legitimize_address (x);
  else
    gcc_unreachable ();
}

/* Return a number assessing the cost of moving a register in class
   FROM to class TO. */
int
score_register_move_cost (enum machine_mode mode ATTRIBUTE_UNUSED,
                          enum reg_class from, enum reg_class to)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_register_move_cost (mode, from, to);
  else
    gcc_unreachable ();
}

/* Implement TARGET_RTX_COSTS macro.  */
bool
score_rtx_costs (rtx x, int code, int outer_code, int *total,
		 bool speed ATTRIBUTE_UNUSED)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_rtx_costs (x, code, outer_code, total, speed);
  else
    gcc_unreachable ();
}

/* Implement TARGET_ADDRESS_COST macro.  */
int
score_address_cost (rtx addr,
		    bool speed ATTRIBUTE_UNUSED)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_address_cost (addr);
  else
    gcc_unreachable ();
}

/* Implement ASM_OUTPUT_EXTERNAL macro.  */
int
score_output_external (FILE *file ATTRIBUTE_UNUSED,
                       tree decl, const char *name)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_output_external (file, decl, name);
  else
    gcc_unreachable ();
}

/* Implement RETURN_ADDR_RTX.  Note, we do not support moving
   back to a previous frame.  */
rtx
score_return_addr (int count, rtx frame ATTRIBUTE_UNUSED)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_return_addr (count, frame);
  else
    gcc_unreachable ();
}

/* Implement PRINT_OPERAND macro.  */
void
score_print_operand (FILE *file, rtx op, int c)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    score7_print_operand (file, op, c);
  else
    gcc_unreachable ();
}

/* Implement PRINT_OPERAND_ADDRESS macro.  */
void
score_print_operand_address (FILE *file, rtx x)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    score7_print_operand_address (file, x);
  else
    gcc_unreachable ();
}

/* Implement SELECT_CC_MODE macro.  */
enum machine_mode
score_select_cc_mode (enum rtx_code op, rtx x, rtx y)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_select_cc_mode (op, x, y);
  else
    gcc_unreachable ();
}

/* Return true if X is a symbolic constant that can be calculated in
   the same way as a bare symbol.  If it is, store the type of the
   symbol in *SYMBOL_TYPE.  */
int
score_symbolic_constant_p (rtx x, enum score_symbol_type *symbol_type)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_symbolic_constant_p (x, symbol_type);
  else
    gcc_unreachable ();
}

/* Generate the prologue instructions for entry into a S+core function.  */
void
score_prologue (void)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    score7_prologue ();
  else
    gcc_unreachable ();
}

/* Generate the epilogue instructions in a S+core function.  */
void
score_epilogue (int sibcall_p)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    score7_epilogue (sibcall_p);
  else
    gcc_unreachable ();
}

/* Call and sibcall pattern all need call this function.  */
void
score_call (rtx *ops, bool sib)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    score7_call (ops, sib);
  else
    gcc_unreachable ();
}

/* Call value and sibcall value pattern all need call this function.  */
void
score_call_value (rtx *ops, bool sib)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    score7_call_value (ops, sib);
  else
    gcc_unreachable ();
}

void
score_movsicc (rtx *ops)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    score7_movsicc (ops);
  else
    gcc_unreachable ();
}

/* Machine Split  */
void
score_movdi (rtx *ops)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    score7_movdi (ops);
  else
    gcc_unreachable ();
}

void
score_zero_extract_andi (rtx *ops)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    score7_zero_extract_andi (ops);
  else
    gcc_unreachable ();
}

/* Output asm insn for move.  */
const char *
score_move (rtx *ops)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_move (ops);
  else
    gcc_unreachable ();
}

/* Output asm insn for load.  */
const char *
score_linsn (rtx *ops, enum score_mem_unit unit, bool sign)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_linsn (ops, unit, sign);
  else
    gcc_unreachable ();
}

/* Output asm insn for store.  */
const char *
score_sinsn (rtx *ops, enum score_mem_unit unit)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_sinsn (ops, unit);
  else
    gcc_unreachable ();
}

/* Output asm insn for load immediate.  */
const char *
score_limm (rtx *ops)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_limm (ops);
  else
    gcc_unreachable ();
}


/* Generate add insn.  */
const char *
score_select_add_imm (rtx *ops, bool set_cc)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_select_add_imm (ops, set_cc);
  else
    gcc_unreachable ();
}

/* Output arith insn.  */
const char *
score_select (rtx *ops, const char *inst_pre,
            bool commu, const char *letter, bool set_cc)
{
  if (TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_select (ops, inst_pre, commu, letter, set_cc);
  else
    gcc_unreachable ();
}

static void
score_conditional_register_usage (void)
{
   if (!flag_pic)
     fixed_regs[PIC_OFFSET_TABLE_REGNUM] =
     call_used_regs[PIC_OFFSET_TABLE_REGNUM] = 0;
}

struct gcc_target targetm = TARGET_INITIALIZER;
