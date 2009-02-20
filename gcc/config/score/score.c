/* Output routines for Sunplus S+CORE processor
   Copyright (C) 2005, 2007, 2008 Free Software Foundation, Inc.
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
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-attr.h"
#include "recog.h"
#include "toplev.h"
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
#include "score3.h"
#include "df.h"

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

#undef TARGET_PROMOTE_FUNCTION_ARGS
#define TARGET_PROMOTE_FUNCTION_ARGS    hook_bool_tree_true

#undef TARGET_PROMOTE_FUNCTION_RETURN
#define TARGET_PROMOTE_FUNCTION_RETURN  hook_bool_tree_true

#undef TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES       hook_bool_tree_true

#undef TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK       must_pass_in_stack_var_size

#undef TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES        score_arg_partial_bytes

#undef TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE        score_pass_by_reference

#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY         score_return_in_memory

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS                score_rtx_costs

#undef TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST             score_address_cost

struct extern_list *extern_head = 0;
rtx cmp_op0, cmp_op1;

/* default 0 = NO_REGS  */
enum reg_class score_char_to_class[256];

/* Implement TARGET_RETURN_IN_MEMORY.  In S+core,
   small structures are returned in a register.
   Objects with varying size must still be returned in memory.  */
static bool
score_return_in_memory (tree type, tree fndecl ATTRIBUTE_UNUSED)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_return_in_memory (type, fndecl);
  else if (TARGET_SCORE3)
    return score3_return_in_memory (type, fndecl);

  gcc_unreachable ();
}

/* Return nonzero when an argument must be passed by reference.  */
static bool
score_pass_by_reference (CUMULATIVE_ARGS *cum ATTRIBUTE_UNUSED,
                         enum machine_mode mode, tree type,
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
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_output_mi_thunk (file, thunk_fndecl, delta,
                                   vcall_offset, function);
  else if (TARGET_SCORE3)
    return score3_output_mi_thunk (file, thunk_fndecl, delta,
                                   vcall_offset, function);
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
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_function_prologue (file, size);
  else if (TARGET_SCORE3)
    return score3_function_prologue (file, size);

  gcc_unreachable ();
}

/* Do any necessary cleanup after a function to restore stack, frame,
   and regs.  */
static void
score_function_epilogue (FILE *file,
                         HOST_WIDE_INT size ATTRIBUTE_UNUSED)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_function_epilogue (file, size);
  else if (TARGET_SCORE3)
    return score3_function_epilogue (file, size);

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
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_select_rtx_section (mode, x, align);
  else if (TARGET_SCORE3)
    return score3_select_rtx_section (mode, x, align);

  gcc_unreachable ();
}

/* Implement TARGET_IN_SMALL_DATA_P.  */
static bool
score_in_small_data_p (tree decl)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_in_small_data_p (decl);
  else if (TARGET_SCORE3)
    return score3_in_small_data_p (decl);

  gcc_unreachable ();
}

/* Implement TARGET_ASM_FILE_START.  */
static void
score_asm_file_start (void)
{
  if (TARGET_SCORE5)
    fprintf (asm_out_file, "# Sunplus S+core5 %s rev=%s\n",
             TARGET_LITTLE_ENDIAN ? "el" : "eb", SCORE_GCC_VERSION);
  else if (TARGET_SCORE5U)
    fprintf (asm_out_file, "# Sunplus S+core5u %s rev=%s\n",
             TARGET_LITTLE_ENDIAN ? "el" : "eb", SCORE_GCC_VERSION);
  else if (TARGET_SCORE7D)
    fprintf (asm_out_file, "# Sunplus S+core7d %s rev=%s\n",
             TARGET_LITTLE_ENDIAN ? "el" : "eb", SCORE_GCC_VERSION);
  else if (TARGET_SCORE7)
    fprintf (asm_out_file, "# Sunplus S+core7 %s rev=%s\n",
             TARGET_LITTLE_ENDIAN ? "el" : "eb", SCORE_GCC_VERSION);
  else if (TARGET_SCORE3D)
    fprintf (asm_out_file, "# Sunplus S+core3d %s rev=%s\n",
             TARGET_LITTLE_ENDIAN ? "el" : "eb", SCORE_GCC_VERSION);
  else if (TARGET_SCORE3)
    fprintf (asm_out_file, "# Sunplus S+core3 %s rev=%s\n",
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
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_asm_file_end ();
  else if (TARGET_SCORE3)
    return score3_asm_file_end ();

  gcc_unreachable ();
}

#define MASK_ALL_CPU_BITS \
  (MASK_SCORE5 | MASK_SCORE5U | MASK_SCORE7 | MASK_SCORE7D \
   | MASK_SCORE3 | MASK_SCORE3D)

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

    case OPT_mscore3d:
      target_flags &= ~(MASK_ALL_CPU_BITS);
      target_flags |= MASK_SCORE3 | MASK_SCORE3D;
      return true;

    case OPT_march_:
      if (strcmp (arg, "score5") == 0)
        {
          target_flags &= ~(MASK_ALL_CPU_BITS);
          target_flags |= MASK_SCORE5;
          return true;
        }
      else if (strcmp (arg, "score5u") == 0)
        {
          target_flags &= ~(MASK_ALL_CPU_BITS);
          target_flags |= MASK_SCORE5U;
          return true;
        }
      else if (strcmp (arg, "score7") == 0)
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
      else if (strcmp (arg, "score3") == 0)
        {
          target_flags &= ~(MASK_ALL_CPU_BITS);
          target_flags |= MASK_SCORE3;
          return true;
        }
      else if (strcmp (arg, "score3d") == 0)
        {
          target_flags &= ~(MASK_ALL_CPU_BITS);
          target_flags |= MASK_SCORE3 | MASK_SCORE3D;
          return true;
        }
      else
        return false;

    default:
      return true;
    }
}

/* Implement OVERRIDE_OPTIONS macro.  */
void
score_override_options (void)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_override_options ();
  else if (TARGET_SCORE3)
    return score3_override_options ();

  return score7_override_options ();
}

/* Implement REGNO_REG_CLASS macro.  */
int
score_reg_class (int regno)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_reg_class (regno);
  else if (TARGET_SCORE3)
    return score3_reg_class (regno);

  gcc_unreachable ();
}

/* Implement PREFERRED_RELOAD_CLASS macro.  */
enum reg_class
score_preferred_reload_class (rtx x ATTRIBUTE_UNUSED, enum reg_class rclass)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_preferred_reload_class (x, rclass);
  else if (TARGET_SCORE3)
    return score3_preferred_reload_class (x, rclass);

  gcc_unreachable ();
}

/* Implement SECONDARY_INPUT_RELOAD_CLASS
   and SECONDARY_OUTPUT_RELOAD_CLASS macro.  */
enum reg_class
score_secondary_reload_class (enum reg_class rclass,
                              enum machine_mode mode ATTRIBUTE_UNUSED,
                              rtx x)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_secondary_reload_class (rclass, mode, x);
  else if (TARGET_SCORE3)
    return score3_secondary_reload_class (rclass, mode, x);

  gcc_unreachable ();
}

/* Implement CONST_OK_FOR_LETTER_P macro.  */
int
score_const_ok_for_letter_p (HOST_WIDE_INT value, char c)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_const_ok_for_letter_p (value, c);
  else if (TARGET_SCORE3)
    return score3_const_ok_for_letter_p (value, c);

  gcc_unreachable ();
}

/* Implement EXTRA_CONSTRAINT macro.  */
int
score_extra_constraint (rtx op, char c)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_extra_constraint (op, c);
  else if (TARGET_SCORE3)
    return score3_extra_constraint (op, c);

  gcc_unreachable ();
}

/* Return truth value on whether or not a given hard register
   can support a given mode.  */
int
score_hard_regno_mode_ok (unsigned int regno, enum machine_mode mode)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_hard_regno_mode_ok (regno, mode);
  else if (TARGET_SCORE3)
    return score3_hard_regno_mode_ok (regno, mode);

  gcc_unreachable ();
}

/* Implement INITIAL_ELIMINATION_OFFSET.  FROM is either the frame
   pointer or argument pointer.  TO is either the stack pointer or
   hard frame pointer.  */
HOST_WIDE_INT
score_initial_elimination_offset (int from,
                                  int to ATTRIBUTE_UNUSED)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_initial_elimination_offset (from, to);
  else if (TARGET_SCORE3)
    return score3_initial_elimination_offset (from, to);

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

/* Implement FUNCTION_ARG_ADVANCE macro.  */
void
score_function_arg_advance (CUMULATIVE_ARGS *cum, enum machine_mode mode,
                            tree type, int named)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_function_arg_advance (cum, mode, type, named);
  else if (TARGET_SCORE3)
    return score3_function_arg_advance (cum, mode, type, named);

  gcc_unreachable ();
}

/* Implement TARGET_ARG_PARTIAL_BYTES macro.  */
int
score_arg_partial_bytes (CUMULATIVE_ARGS *cum,
                         enum machine_mode mode, tree type, bool named)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_arg_partial_bytes (cum, mode, type, named);
  else if (TARGET_SCORE3)
    return score3_arg_partial_bytes (cum, mode, type, named);

  gcc_unreachable ();
}

/* Implement FUNCTION_ARG macro.  */
rtx
score_function_arg (const CUMULATIVE_ARGS *cum, enum machine_mode mode,
                    tree type, int named)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_function_arg (cum, mode, type, named);
  else if (TARGET_SCORE3)
    return score3_function_arg (cum, mode, type, named);

  gcc_unreachable ();
}

/* Implement FUNCTION_VALUE and LIBCALL_VALUE.  For normal calls,
   VALTYPE is the return type and MODE is VOIDmode.  For libcalls,
   VALTYPE is null and MODE is the mode of the return value.  */
rtx
score_function_value (tree valtype, tree func ATTRIBUTE_UNUSED,
                      enum machine_mode mode)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_function_value (valtype, func, mode);
  else if (TARGET_SCORE3)
    return score3_function_value (valtype, func, mode);

  gcc_unreachable ();
}

/* Implement INITIALIZE_TRAMPOLINE macro.  */
void
score_initialize_trampoline (rtx ADDR, rtx FUNC, rtx CHAIN)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_initialize_trampoline (ADDR, FUNC, CHAIN);
  else if (TARGET_SCORE3)
    return score3_initialize_trampoline (ADDR, FUNC, CHAIN);

  gcc_unreachable ();
}

/* This function is used to implement REG_MODE_OK_FOR_BASE_P macro.  */
int
score_regno_mode_ok_for_base_p (int regno, int strict)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_regno_mode_ok_for_base_p (regno, strict);
  else if (TARGET_SCORE3)
    return score3_regno_mode_ok_for_base_p (regno, strict);

  gcc_unreachable ();
}

/* Implement GO_IF_LEGITIMATE_ADDRESS macro.  */
int
score_address_p (enum machine_mode mode, rtx x, int strict)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_address_p (mode, x, strict);
  else if (TARGET_SCORE3)
    return score3_address_p (mode, x, strict);

  gcc_unreachable ();
}

/* This function is used to implement LEGITIMIZE_ADDRESS.  If *XLOC can
   be legitimized in a way that the generic machinery might not expect,
   put the new address in *XLOC and return true.  */
int
score_legitimize_address (rtx *xloc)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_legitimize_address (xloc);
  else if (TARGET_SCORE3)
    return score3_legitimize_address (xloc);

  gcc_unreachable ();
}

/* Return a number assessing the cost of moving a register in class
   FROM to class TO. */
int
score_register_move_cost (enum machine_mode mode ATTRIBUTE_UNUSED,
                          enum reg_class from, enum reg_class to)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_register_move_cost (mode, from, to);
  else if (TARGET_SCORE3)
    return score3_register_move_cost (mode, from, to);

  gcc_unreachable ();
}

/* Implement TARGET_RTX_COSTS macro.  */
bool
score_rtx_costs (rtx x, int code, int outer_code, int *total,
		 bool speed ATTRIBUTE_UNUSED)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_rtx_costs (x, code, outer_code, total, speed);
  else if (TARGET_SCORE3)
    return score3_rtx_costs (x, code, outer_code, total, speed);

  gcc_unreachable ();
}

/* Implement TARGET_ADDRESS_COST macro.  */
int
score_address_cost (rtx addr,
		    bool speed ATTRIBUTE_UNUSED)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_address_cost (addr);
  else if (TARGET_SCORE3)
    return score3_address_cost (addr);

  gcc_unreachable ();
}

/* Implement ASM_OUTPUT_EXTERNAL macro.  */
int
score_output_external (FILE *file ATTRIBUTE_UNUSED,
                       tree decl, const char *name)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_output_external (file, decl, name);
  else if (TARGET_SCORE3)
    return score3_output_external (file, decl, name);

  gcc_unreachable ();
}

/* Implement RETURN_ADDR_RTX.  Note, we do not support moving
   back to a previous frame.  */
rtx
score_return_addr (int count, rtx frame ATTRIBUTE_UNUSED)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_return_addr (count, frame);
  else if (TARGET_SCORE3)
    return score3_return_addr (count, frame);

  gcc_unreachable ();
}

/* Implement PRINT_OPERAND macro.  */
void
score_print_operand (FILE *file, rtx op, int c)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_print_operand (file, op, c);
  else if (TARGET_SCORE3)
    return score3_print_operand (file, op, c);

  gcc_unreachable ();
}

/* Implement PRINT_OPERAND_ADDRESS macro.  */
void
score_print_operand_address (FILE *file, rtx x)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_print_operand_address (file, x);
  else if (TARGET_SCORE3)
    return score3_print_operand_address (file, x);

  gcc_unreachable ();
}

/* Implement SELECT_CC_MODE macro.  */
enum machine_mode
score_select_cc_mode (enum rtx_code op, rtx x, rtx y)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_select_cc_mode (op, x, y);
  else if (TARGET_SCORE3)
    return score3_select_cc_mode (op, x, y);

  gcc_unreachable ();
}

/* Return true if X is a symbolic constant that can be calculated in
   the same way as a bare symbol.  If it is, store the type of the
   symbol in *SYMBOL_TYPE.  */
int
score_symbolic_constant_p (rtx x, enum score_symbol_type *symbol_type)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_symbolic_constant_p (x, symbol_type);
  else if (TARGET_SCORE3)
    return score3_symbolic_constant_p (x, symbol_type);

  gcc_unreachable ();
}

/* Generate the prologue instructions for entry into a S+core function.  */
void
score_prologue (void)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_prologue ();
  else if (TARGET_SCORE3)
    return score3_prologue ();

  gcc_unreachable ();
}

/* Generate the epilogue instructions in a S+core function.  */
void
score_epilogue (int sibcall_p)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_epilogue (sibcall_p);
  else if (TARGET_SCORE3)
    return score3_epilogue (sibcall_p);

  gcc_unreachable ();
}

void
score_gen_cmp (enum machine_mode mode)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_gen_cmp (mode);
  else if (TARGET_SCORE3)
    return score3_gen_cmp (mode);

  gcc_unreachable ();
}

/* Call and sibcall pattern all need call this function.  */
void
score_call (rtx *ops, bool sib)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_call (ops, sib);
  else if (TARGET_SCORE3)
    return score3_call (ops, sib);

  gcc_unreachable ();
}

/* Call value and sibcall value pattern all need call this function.  */
void
score_call_value (rtx *ops, bool sib)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_call_value (ops, sib);
  else if (TARGET_SCORE3)
    return score3_call_value (ops, sib);

  gcc_unreachable ();
}

void
score_movsicc (rtx *ops)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_movsicc (ops);
  else if (TARGET_SCORE3)
    return score3_movsicc (ops);

  gcc_unreachable ();
}

/* Machine Split  */
void
score_movdi (rtx *ops)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_movdi (ops);
  else if (TARGET_SCORE3)
    return score3_movdi (ops);

  gcc_unreachable ();
}

void
score_zero_extract_andi (rtx *ops)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_zero_extract_andi (ops);
  else if (TARGET_SCORE3)
    return score3_zero_extract_andi (ops);

  gcc_unreachable ();
}

/* Output asm insn for move.  */
const char *
score_move (rtx *ops)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_move (ops);
  else if (TARGET_SCORE3)
    return score3_move (ops);

  gcc_unreachable ();
}

/* Output asm insn for load.  */
const char *
score_linsn (rtx *ops, enum score_mem_unit unit, bool sign)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_linsn (ops, unit, sign);
  else if (TARGET_SCORE3)
    return score3_linsn (ops, unit, sign);

  gcc_unreachable ();
}

/* Output asm insn for store.  */
const char *
score_sinsn (rtx *ops, enum score_mem_unit unit)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_sinsn (ops, unit);
  else if (TARGET_SCORE3)
    return score3_sinsn (ops, unit);

  gcc_unreachable ();
}

/* Output asm insn for load immediate.  */
const char *
score_limm (rtx *ops)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_limm (ops);
  else if (TARGET_SCORE3)
    return score3_limm (ops);

  gcc_unreachable ();
}


/* Generate add insn.  */
const char *
score_select_add_imm (rtx *ops, bool set_cc)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_select_add_imm (ops, set_cc);
  else if (TARGET_SCORE3)
    return score3_select_add_imm (ops, set_cc);

  gcc_unreachable ();
}

/* Output arith insn.  */
const char *
score_select (rtx *ops, const char *inst_pre,
            bool commu, const char *letter, bool set_cc)
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    return score7_select (ops, inst_pre, commu, letter, set_cc);
  else if (TARGET_SCORE3)
    return score3_select (ops, inst_pre, commu, letter, set_cc);

  gcc_unreachable ();
}

/* Output switch case insn, only supported in score3.  */
const char *
score_output_casesi (rtx *operands)
{
  if (TARGET_SCORE3)
    return score3_output_casesi (operands);

  gcc_unreachable ();
}

/* Output rpush insn, only supported in score3.  */
const char *
score_rpush (rtx *operands)
{
  if (TARGET_SCORE3)
    return score3_rpush (operands);

  gcc_unreachable ();
}

/* Output rpop insn, only supported in score3.  */
const char *
score_rpop (rtx *operands)
{
  if (TARGET_SCORE3)
    return score3_rpop (operands);

  gcc_unreachable ();
}

/* Emit lcb/lce insns.  */
bool
score_unaligned_load (rtx *ops)
{
  rtx dst = ops[0];
  rtx src = ops[1];
  rtx len = ops[2];
  rtx off = ops[3];
  rtx addr_reg;

  if (INTVAL (len) != BITS_PER_WORD
      || (INTVAL (off) % BITS_PER_UNIT) != 0)
    return false;

  gcc_assert (GET_MODE_SIZE (GET_MODE (dst)) == GET_MODE_SIZE (SImode));

  addr_reg = copy_addr_to_reg (XEXP (src, 0));
  emit_insn (gen_move_lcb (addr_reg, addr_reg));
  emit_insn (gen_move_lce (addr_reg, addr_reg, dst));

  return true;
}

/* Emit scb/sce insns.  */
bool
score_unaligned_store (rtx *ops)
{
  rtx dst = ops[0];
  rtx len = ops[1];
  rtx off = ops[2];
  rtx src = ops[3];
  rtx addr_reg;

  if (INTVAL(len) != BITS_PER_WORD
      || (INTVAL(off) % BITS_PER_UNIT) != 0)
    return false;

  gcc_assert (GET_MODE_SIZE (GET_MODE (src)) == GET_MODE_SIZE (SImode));

  addr_reg = copy_addr_to_reg (XEXP (dst, 0));
  emit_insn (gen_move_scb (addr_reg, addr_reg, src));
  emit_insn (gen_move_sce (addr_reg, addr_reg));

  return true;
}

/* If length is short, generate move insns straight.  */
static void
score_block_move_straight (rtx dst, rtx src, HOST_WIDE_INT length)
{
  HOST_WIDE_INT leftover;
  int i, reg_count;
  rtx *regs;

  leftover = length % UNITS_PER_WORD;
  length -= leftover;
  reg_count = length / UNITS_PER_WORD;

  regs = XALLOCAVEC (rtx, reg_count);
  for (i = 0; i < reg_count; i++)
    regs[i] = gen_reg_rtx (SImode);

  /* Load from src to regs.  */
  if (MEM_ALIGN (src) >= BITS_PER_WORD)
    {
      HOST_WIDE_INT offset = 0;
      for (i = 0; i < reg_count; offset += UNITS_PER_WORD, i++)
        emit_move_insn (regs[i], adjust_address (src, SImode, offset));
    }
  else if (reg_count >= 1)
    {
      rtx src_reg = copy_addr_to_reg (XEXP (src, 0));

      emit_insn (gen_move_lcb (src_reg, src_reg));
      for (i = 0; i < (reg_count - 1); i++)
        emit_insn (gen_move_lcw (src_reg, src_reg, regs[i]));
      emit_insn (gen_move_lce (src_reg, src_reg, regs[i]));
    }

  /* Store regs to dest.  */
  if (MEM_ALIGN (dst) >= BITS_PER_WORD)
    {
      HOST_WIDE_INT offset = 0;
      for (i = 0; i < reg_count; offset += UNITS_PER_WORD, i++)
        emit_move_insn (adjust_address (dst, SImode, offset), regs[i]);
    }
  else if (reg_count >= 1)
    {
      rtx dst_reg = copy_addr_to_reg (XEXP (dst, 0));

      emit_insn (gen_move_scb (dst_reg, dst_reg, regs[0]));
      for (i = 1; i < reg_count; i++)
        emit_insn (gen_move_scw (dst_reg, dst_reg, regs[i]));
      emit_insn (gen_move_sce (dst_reg, dst_reg));
    }

  /* Mop up any left-over bytes.  */
  if (leftover > 0)
    {
      src = adjust_address (src, BLKmode, length);
      dst = adjust_address (dst, BLKmode, length);
      move_by_pieces (dst, src, leftover,
                      MIN (MEM_ALIGN (src), MEM_ALIGN (dst)), 0);
    }
}

/* Generate loop head when dst or src is unaligned.  */
static void
score_block_move_loop_head (rtx dst_reg, HOST_WIDE_INT dst_align,
                            rtx src_reg, HOST_WIDE_INT src_align,
                            HOST_WIDE_INT length)
{
  bool src_unaligned = (src_align < BITS_PER_WORD);
  bool dst_unaligned = (dst_align < BITS_PER_WORD);

  rtx temp = gen_reg_rtx (SImode);

  gcc_assert (length == UNITS_PER_WORD);

  if (src_unaligned)
    {
      emit_insn (gen_move_lcb (src_reg, src_reg));
      emit_insn (gen_move_lcw (src_reg, src_reg, temp));
    }
  else
    emit_insn (gen_move_lw_a (src_reg,
                              src_reg, gen_int_mode (4, SImode), temp));

  if (dst_unaligned)
    emit_insn (gen_move_scb (dst_reg, dst_reg, temp));
  else
    emit_insn (gen_move_sw_a (dst_reg,
                              dst_reg, gen_int_mode (4, SImode), temp));
}

/* Generate loop body, copy length bytes per iteration.  */
static void
score_block_move_loop_body (rtx dst_reg, HOST_WIDE_INT dst_align,
                            rtx src_reg, HOST_WIDE_INT src_align,
                            HOST_WIDE_INT length)
{
  int reg_count = length / UNITS_PER_WORD;
  rtx *regs = XALLOCAVEC (rtx, reg_count);
  int i;
  bool src_unaligned = (src_align < BITS_PER_WORD);
  bool dst_unaligned = (dst_align < BITS_PER_WORD);

  for (i = 0; i < reg_count; i++)
    regs[i] = gen_reg_rtx (SImode);

  if (src_unaligned)
    {
      for (i = 0; i < reg_count; i++)
        emit_insn (gen_move_lcw (src_reg, src_reg, regs[i]));
    }
  else
    {
      for (i = 0; i < reg_count; i++)
        emit_insn (gen_move_lw_a (src_reg,
                                  src_reg, gen_int_mode (4, SImode), regs[i]));
    }

  if (dst_unaligned)
    {
      for (i = 0; i < reg_count; i++)
        emit_insn (gen_move_scw (dst_reg, dst_reg, regs[i]));
    }
  else
    {
      for (i = 0; i < reg_count; i++)
        emit_insn (gen_move_sw_a (dst_reg,
                                  dst_reg, gen_int_mode (4, SImode), regs[i]));
    }
}

/* Generate loop foot, copy the leftover bytes.  */
static void
score_block_move_loop_foot (rtx dst_reg, HOST_WIDE_INT dst_align,
                            rtx src_reg, HOST_WIDE_INT src_align,
                            HOST_WIDE_INT length)
{
  bool src_unaligned = (src_align < BITS_PER_WORD);
  bool dst_unaligned = (dst_align < BITS_PER_WORD);

  HOST_WIDE_INT leftover;

  leftover = length % UNITS_PER_WORD;
  length -= leftover;

  if (length > 0)
    score_block_move_loop_body (dst_reg, dst_align,
                              src_reg, src_align, length);

  if (dst_unaligned)
    emit_insn (gen_move_sce (dst_reg, dst_reg));

  if (leftover > 0)
    {
      HOST_WIDE_INT src_adj = src_unaligned ? -4 : 0;
      HOST_WIDE_INT dst_adj = dst_unaligned ? -4 : 0;
      rtx temp;

      gcc_assert (leftover < UNITS_PER_WORD);

      if (leftover >= UNITS_PER_WORD / 2
          && src_align >= BITS_PER_WORD / 2
          && dst_align >= BITS_PER_WORD / 2)
        {
          temp = gen_reg_rtx (HImode);
          emit_insn (gen_move_lhu_b (src_reg, src_reg,
                                     gen_int_mode (src_adj, SImode), temp));
          emit_insn (gen_move_sh_b (dst_reg, dst_reg,
                                    gen_int_mode (dst_adj, SImode), temp));
          leftover -= UNITS_PER_WORD / 2;
          src_adj = UNITS_PER_WORD / 2;
          dst_adj = UNITS_PER_WORD / 2;
        }

      while (leftover > 0)
        {
          temp = gen_reg_rtx (QImode);
          emit_insn (gen_move_lbu_b (src_reg, src_reg,
                                     gen_int_mode (src_adj, SImode), temp));
          emit_insn (gen_move_sb_b (dst_reg, dst_reg,
                                    gen_int_mode (dst_adj, SImode), temp));
          leftover--;
          src_adj = 1;
          dst_adj = 1;
        }
    }
}

#define MIN_MOVE_REGS 3
#define MIN_MOVE_BYTES (MIN_MOVE_REGS * UNITS_PER_WORD)
#define MAX_MOVE_REGS 4
#define MAX_MOVE_BYTES (MAX_MOVE_REGS * UNITS_PER_WORD)

/* The length is large, generate a loop if necessary.
   The loop is consisted by loop head/body/foot.  */
static void
score_block_move_loop (rtx dst, rtx src, HOST_WIDE_INT length)
{
  HOST_WIDE_INT src_align = MEM_ALIGN (src);
  HOST_WIDE_INT dst_align = MEM_ALIGN (dst);
  HOST_WIDE_INT loop_mov_bytes;
  HOST_WIDE_INT iteration = 0;
  HOST_WIDE_INT head_length = 0, leftover;
  rtx label, src_reg, dst_reg, final_dst;

  bool gen_loop_head = (src_align < BITS_PER_WORD
                        || dst_align < BITS_PER_WORD);

  if (gen_loop_head)
    head_length += UNITS_PER_WORD;

  for (loop_mov_bytes = MAX_MOVE_BYTES;
       loop_mov_bytes >= MIN_MOVE_BYTES;
       loop_mov_bytes -= UNITS_PER_WORD)
    {
      iteration = (length - head_length) / loop_mov_bytes;
      if (iteration > 1)
        break;
    }
  if (iteration <= 1)
    {
      score_block_move_straight (dst, src, length);
      return;
    }

  leftover = (length - head_length) % loop_mov_bytes;
  length -= leftover;

  src_reg = copy_addr_to_reg (XEXP (src, 0));
  dst_reg = copy_addr_to_reg (XEXP (dst, 0));
  final_dst = expand_simple_binop (Pmode, PLUS, dst_reg, GEN_INT (length),
                                   0, 0, OPTAB_WIDEN);

  if (gen_loop_head)
    score_block_move_loop_head (dst_reg, dst_align,
                              src_reg, src_align, head_length);

  label = gen_label_rtx ();
  emit_label (label);

  score_block_move_loop_body (dst_reg, dst_align,
                            src_reg, src_align, loop_mov_bytes);

  emit_insn (gen_cmpsi (dst_reg, final_dst));
  emit_jump_insn (gen_bne (label));

  score_block_move_loop_foot (dst_reg, dst_align,
                            src_reg, src_align, leftover);
}

/* Generate block move, for misc.md: "movmemsi".  */
bool
score_block_move (rtx *ops)
{
  rtx dst = ops[0];
  rtx src = ops[1];
  rtx length = ops[2];

  if (TARGET_LITTLE_ENDIAN
      && (MEM_ALIGN (src) < BITS_PER_WORD || MEM_ALIGN (dst) < BITS_PER_WORD)
      && INTVAL (length) >= UNITS_PER_WORD)
    return false;

  if (GET_CODE (length) == CONST_INT)
    {
      if (INTVAL (length) <= 2 * MAX_MOVE_BYTES)
        {
          score_block_move_straight (dst, src, INTVAL (length));
          return true;
        }
      else if (optimize &&
               !(flag_unroll_loops || flag_unroll_all_loops))
        {
          score_block_move_loop (dst, src, INTVAL (length));
          return true;
        }
    }
  return false;
}

struct gcc_target targetm = TARGET_INITIALIZER;
