/* Definitions for GCC.  Part of the machine description for CRIS.
   Copyright (C) 1998-2017 Free Software Foundation, Inc.
   Contributed by Axis Communications.  Written by Hans-Peter Nilsson.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "stringpool.h"
#include "attribs.h"
#include "cfghooks.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "cgraph.h"
#include "diagnostic-core.h"
#include "conditions.h"
#include "insn-attr.h"
#include "alias.h"
#include "varasm.h"
#include "stor-layout.h"
#include "calls.h"
#include "explow.h"
#include "expr.h"
#include "reload.h"
#include "output.h"
#include "tm-constrs.h"
#include "builtins.h"

/* This file should be included last.  */
#include "target-def.h"

/* Usable when we have an amount to add or subtract, and want the
   optimal size of the insn.  */
#define ADDITIVE_SIZE_MODIFIER(size) \
 ((size) <= 63 ? "q" : (size) <= 255 ? "u.b" : (size) <= 65535 ? "u.w" : ".d")

#define LOSE_AND_RETURN(msgid, x)			\
  do						\
    {						\
      cris_operand_lossage (msgid, x);		\
      return;					\
    } while (0)

enum cris_retinsn_type
 { CRIS_RETINSN_UNKNOWN = 0, CRIS_RETINSN_RET, CRIS_RETINSN_JUMP };

/* Per-function machine data.  */
struct GTY(()) machine_function
 {
   int needs_return_address_on_stack;

   /* This is the number of registers we save in the prologue due to
      stdarg.  */
   int stdarg_regs;

   enum cris_retinsn_type return_type;
 };

/* This little fix suppresses the 'u' or 's' when '%e' in assembly
   pattern.  */
static char cris_output_insn_is_bound = 0;

/* In code for output macros, this is how we know whether e.g. constant
   goes in code or in a static initializer.  */
static int in_code = 0;

/* Fix for reg_overlap_mentioned_p.  */
static int cris_reg_overlap_mentioned_p (rtx, rtx);

static machine_mode cris_promote_function_mode (const_tree, machine_mode,
						     int *, const_tree, int);

static unsigned int cris_atomic_align_for_mode (machine_mode);

static void cris_print_base (rtx, FILE *);

static void cris_print_index (rtx, FILE *);

static void cris_output_addr_const (FILE *, rtx);

static struct machine_function * cris_init_machine_status (void);

static rtx cris_struct_value_rtx (tree, int);

static void cris_setup_incoming_varargs (cumulative_args_t, machine_mode,
					 tree type, int *, int);

static int cris_initial_frame_pointer_offset (void);

static void cris_operand_lossage (const char *, rtx);

static int cris_reg_saved_in_regsave_area  (unsigned int, bool);

static void cris_print_operand (FILE *, rtx, int);

static void cris_print_operand_address (FILE *, machine_mode, rtx);

static bool cris_print_operand_punct_valid_p (unsigned char code);

static bool cris_output_addr_const_extra (FILE *, rtx);

static void cris_conditional_register_usage (void);

static void cris_asm_output_mi_thunk
  (FILE *, tree, HOST_WIDE_INT, HOST_WIDE_INT, tree);

static void cris_file_start (void);
static void cris_init_libfuncs (void);

static reg_class_t cris_preferred_reload_class (rtx, reg_class_t);

static int cris_register_move_cost (machine_mode, reg_class_t, reg_class_t);
static int cris_memory_move_cost (machine_mode, reg_class_t, bool);
static bool cris_rtx_costs (rtx, machine_mode, int, int, int *, bool);
static int cris_address_cost (rtx, machine_mode, addr_space_t, bool);
static bool cris_pass_by_reference (cumulative_args_t, machine_mode,
				    const_tree, bool);
static int cris_arg_partial_bytes (cumulative_args_t, machine_mode,
				   tree, bool);
static rtx cris_function_arg (cumulative_args_t, machine_mode,
			      const_tree, bool);
static rtx cris_function_incoming_arg (cumulative_args_t,
				       machine_mode, const_tree, bool);
static void cris_function_arg_advance (cumulative_args_t, machine_mode,
				       const_tree, bool);
static rtx_insn *cris_md_asm_adjust (vec<rtx> &, vec<rtx> &,
				     vec<const char *> &,
				     vec<rtx> &, HARD_REG_SET &);
static bool cris_cannot_force_const_mem (machine_mode, rtx);

static void cris_option_override (void);

static bool cris_frame_pointer_required (void);

static void cris_asm_trampoline_template (FILE *);
static void cris_trampoline_init (rtx, tree, rtx);

static rtx cris_function_value(const_tree, const_tree, bool);
static rtx cris_libcall_value (machine_mode, const_rtx);
static bool cris_function_value_regno_p (const unsigned int);
static void cris_file_end (void);
static bool cris_hard_regno_mode_ok (unsigned int, machine_mode);

/* This is the parsed result of the "-max-stack-stackframe=" option.  If
   it (still) is zero, then there was no such option given.  */
int cris_max_stackframe = 0;

/* This is the parsed result of the "-march=" option, if given.  */
int cris_cpu_version = CRIS_DEFAULT_CPU_VERSION;

#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.word\t"
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.dword\t"
#undef TARGET_ASM_ALIGNED_DI_OP
#define TARGET_ASM_ALIGNED_DI_OP "\t.quad\t"

/* We need to define these, since the 2byte, 4byte, 8byte op:s are only
   available in ELF.  These "normal" pseudos do not have any alignment
   constraints or side-effects.  */
#undef TARGET_ASM_UNALIGNED_HI_OP
#define TARGET_ASM_UNALIGNED_HI_OP TARGET_ASM_ALIGNED_HI_OP

#undef TARGET_ASM_UNALIGNED_SI_OP
#define TARGET_ASM_UNALIGNED_SI_OP TARGET_ASM_ALIGNED_SI_OP

#undef TARGET_ASM_UNALIGNED_DI_OP
#define TARGET_ASM_UNALIGNED_DI_OP TARGET_ASM_ALIGNED_DI_OP

#undef TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND cris_print_operand
#undef TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS cris_print_operand_address
#undef TARGET_PRINT_OPERAND_PUNCT_VALID_P
#define TARGET_PRINT_OPERAND_PUNCT_VALID_P cris_print_operand_punct_valid_p
#undef TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA
#define TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA cris_output_addr_const_extra

#undef TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE cris_conditional_register_usage

#undef TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK cris_asm_output_mi_thunk
#undef TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK default_can_output_mi_thunk_no_vcall

#undef TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START cris_file_start
#undef TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END cris_file_end

#undef TARGET_INIT_LIBFUNCS
#define TARGET_INIT_LIBFUNCS cris_init_libfuncs

#undef TARGET_LRA_P
#define TARGET_LRA_P hook_bool_void_false

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P cris_legitimate_address_p

#undef TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P cris_legitimate_constant_p

#undef TARGET_PREFERRED_RELOAD_CLASS
#define TARGET_PREFERRED_RELOAD_CLASS cris_preferred_reload_class

#undef TARGET_REGISTER_MOVE_COST
#define TARGET_REGISTER_MOVE_COST cris_register_move_cost
#undef TARGET_MEMORY_MOVE_COST
#define TARGET_MEMORY_MOVE_COST cris_memory_move_cost
#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS cris_rtx_costs
#undef TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST cris_address_cost

#undef TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE cris_promote_function_mode

#undef TARGET_ATOMIC_ALIGN_FOR_MODE
#define TARGET_ATOMIC_ALIGN_FOR_MODE cris_atomic_align_for_mode

#undef TARGET_STRUCT_VALUE_RTX
#define TARGET_STRUCT_VALUE_RTX cris_struct_value_rtx
#undef TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS cris_setup_incoming_varargs
#undef TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE cris_pass_by_reference
#undef TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES cris_arg_partial_bytes
#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG cris_function_arg
#undef TARGET_FUNCTION_INCOMING_ARG
#define TARGET_FUNCTION_INCOMING_ARG cris_function_incoming_arg
#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE cris_function_arg_advance
#undef TARGET_MD_ASM_ADJUST
#define TARGET_MD_ASM_ADJUST cris_md_asm_adjust

#undef TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM cris_cannot_force_const_mem

#undef TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED cris_frame_pointer_required

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE cris_option_override

#undef TARGET_ASM_TRAMPOLINE_TEMPLATE
#define TARGET_ASM_TRAMPOLINE_TEMPLATE cris_asm_trampoline_template
#undef TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT cris_trampoline_init

#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE cris_function_value
#undef TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE cris_libcall_value
#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P cris_function_value_regno_p

#undef TARGET_HARD_REGNO_MODE_OK
#define TARGET_HARD_REGNO_MODE_OK cris_hard_regno_mode_ok

struct gcc_target targetm = TARGET_INITIALIZER;

/* Helper for cris_load_multiple_op and cris_ret_movem_op.  */

bool
cris_movem_load_rest_p (rtx op, int offs)
{
  unsigned int reg_count = XVECLEN (op, 0) - offs;
  rtx src_addr;
  int i;
  rtx elt;
  int setno;
  int regno_dir = 1;
  unsigned int regno = 0;

  /* Perform a quick check so we don't blow up below.  FIXME: Adjust for
     other than (MEM reg).  */
  if (reg_count <= 1
      || GET_CODE (XVECEXP (op, 0, offs)) != SET
      || !REG_P (SET_DEST (XVECEXP (op, 0, offs)))
      || !MEM_P (SET_SRC (XVECEXP (op, 0, offs))))
    return false;

  /* Check a possible post-inc indicator.  */
  if (GET_CODE (SET_SRC (XVECEXP (op, 0, offs + 1))) == PLUS)
    {
      rtx reg = XEXP (SET_SRC (XVECEXP (op, 0, offs + 1)), 0);
      rtx inc = XEXP (SET_SRC (XVECEXP (op, 0, offs + 1)), 1);

      reg_count--;

      if (reg_count == 1
	  || !REG_P (reg)
	  || !REG_P (SET_DEST (XVECEXP (op, 0, offs + 1)))
	  || REGNO (reg) != REGNO (SET_DEST (XVECEXP (op, 0, offs + 1)))
	  || !CONST_INT_P (inc)
	  || INTVAL (inc) != (HOST_WIDE_INT) reg_count * 4)
	return false;
      i = offs + 2;
    }
  else
    i = offs + 1;

  if (!TARGET_V32)
    {
      regno_dir = -1;
      regno = reg_count - 1;
    }

  elt = XVECEXP (op, 0, offs);
  src_addr = XEXP (SET_SRC (elt), 0);

  if (GET_CODE (elt) != SET
      || !REG_P (SET_DEST (elt))
      || GET_MODE (SET_DEST (elt)) != SImode
      || REGNO (SET_DEST (elt)) != regno
      || !MEM_P (SET_SRC (elt))
      || GET_MODE (SET_SRC (elt)) != SImode
      || !memory_address_p (SImode, src_addr))
    return false;

  for (setno = 1; i < XVECLEN (op, 0); setno++, i++)
    {
      rtx elt = XVECEXP (op, 0, i);
      regno += regno_dir;

      if (GET_CODE (elt) != SET
	  || !REG_P (SET_DEST (elt))
	  || GET_MODE (SET_DEST (elt)) != SImode
	  || REGNO (SET_DEST (elt)) != regno
	  || !MEM_P (SET_SRC (elt))
	  || GET_MODE (SET_SRC (elt)) != SImode
	  || GET_CODE (XEXP (SET_SRC (elt), 0)) != PLUS
	  || ! rtx_equal_p (XEXP (XEXP (SET_SRC (elt), 0), 0), src_addr)
	  || !CONST_INT_P (XEXP (XEXP (SET_SRC (elt), 0), 1))
	  || INTVAL (XEXP (XEXP (SET_SRC (elt), 0), 1)) != setno * 4)
	return false;
    }

  return true;
}

/* Worker function for predicate for the parallel contents in a movem
   to-memory.  */

bool
cris_store_multiple_op_p (rtx op)
{
  int reg_count = XVECLEN (op, 0);
  rtx dest;
  rtx dest_addr;
  rtx dest_base;
  int i;
  rtx elt;
  int setno;
  int regno_dir = 1;
  int regno = 0;
  int offset = 0;

  /* Perform a quick check so we don't blow up below.  FIXME: Adjust for
     other than (MEM reg) and (MEM (PLUS reg const)).  */
  if (reg_count <= 1)
    return false;

  elt = XVECEXP (op, 0, 0);

  if (GET_CODE (elt) != SET)
    return  false;

  dest = SET_DEST (elt);

  if (!REG_P (SET_SRC (elt)) || !MEM_P (dest))
    return false;

  dest_addr = XEXP (dest, 0);

  /* Check a possible post-inc indicator.  */
  if (GET_CODE (SET_SRC (XVECEXP (op, 0, 1))) == PLUS)
    {
      rtx reg = XEXP (SET_SRC (XVECEXP (op, 0, 1)), 0);
      rtx inc = XEXP (SET_SRC (XVECEXP (op, 0, 1)), 1);

      reg_count--;

      if (reg_count == 1
	  || !REG_P (reg)
	  || !REG_P (SET_DEST (XVECEXP (op, 0, 1)))
	  || REGNO (reg) != REGNO (SET_DEST (XVECEXP (op, 0, 1)))
	  || !CONST_INT_P (inc)
	  /* Support increment by number of registers, and by the offset
	     of the destination, if it has the form (MEM (PLUS reg
	     offset)).  */
	  || !((REG_P (dest_addr)
		&& REGNO (dest_addr) == REGNO (reg)
		&& INTVAL (inc) == (HOST_WIDE_INT) reg_count * 4)
	       || (GET_CODE (dest_addr) == PLUS
		   && REG_P (XEXP (dest_addr, 0))
		   && REGNO (XEXP (dest_addr, 0)) == REGNO (reg)
		   && CONST_INT_P (XEXP (dest_addr, 1))
		   && INTVAL (XEXP (dest_addr, 1)) == INTVAL (inc))))
	return false;

      i = 2;
    }
  else
    i = 1;

  if (!TARGET_V32)
    {
      regno_dir = -1;
      regno = reg_count - 1;
    }

  if (GET_CODE (elt) != SET
      || !REG_P (SET_SRC (elt))
      || GET_MODE (SET_SRC (elt)) != SImode
      || REGNO (SET_SRC (elt)) != (unsigned int) regno
      || !MEM_P (SET_DEST (elt))
      || GET_MODE (SET_DEST (elt)) != SImode)
    return false;

  if (REG_P (dest_addr))
    {
      dest_base = dest_addr;
      offset = 0;
    }
  else if (GET_CODE (dest_addr) == PLUS
	   && REG_P (XEXP (dest_addr, 0))
	   && CONST_INT_P (XEXP (dest_addr, 1)))
    {
      dest_base = XEXP (dest_addr, 0);
      offset = INTVAL (XEXP (dest_addr, 1));
    }
  else
    return false;

  for (setno = 1; i < XVECLEN (op, 0); setno++, i++)
    {
      rtx elt = XVECEXP (op, 0, i);
      regno += regno_dir;

      if (GET_CODE (elt) != SET
	  || !REG_P (SET_SRC (elt))
	  || GET_MODE (SET_SRC (elt)) != SImode
	  || REGNO (SET_SRC (elt)) != (unsigned int) regno
	  || !MEM_P (SET_DEST (elt))
	  || GET_MODE (SET_DEST (elt)) != SImode
	  || GET_CODE (XEXP (SET_DEST (elt), 0)) != PLUS
	  || ! rtx_equal_p (XEXP (XEXP (SET_DEST (elt), 0), 0), dest_base)
	  || !CONST_INT_P (XEXP (XEXP (SET_DEST (elt), 0), 1))
	  || INTVAL (XEXP (XEXP (SET_DEST (elt), 0), 1)) != setno * 4 + offset)
	return false;
    }

  return true;
}

/* The TARGET_CONDITIONAL_REGISTER_USAGE worker.  */

static void
cris_conditional_register_usage (void)
{
  /* FIXME: This isn't nice.  We should be able to use that register for
     something else if the PIC table isn't needed.  */
  if (flag_pic)
    fixed_regs[PIC_OFFSET_TABLE_REGNUM]
      = call_used_regs[PIC_OFFSET_TABLE_REGNUM] = 1;

  /* Allow use of ACR (PC in pre-V32) and tweak order.  */
  if (TARGET_V32)
    {
      static const int reg_alloc_order_v32[] = REG_ALLOC_ORDER_V32;
      unsigned int i;

      fixed_regs[CRIS_ACR_REGNUM] = 0;

      for (i = 0;
          i < sizeof (reg_alloc_order_v32)/sizeof (reg_alloc_order_v32[0]);
          i++)
       reg_alloc_order[i] = reg_alloc_order_v32[i];
    }

  if (TARGET_HAS_MUL_INSNS)
    fixed_regs[CRIS_MOF_REGNUM] = 0;

  /* On early versions, we must use the 16-bit condition-code register,
     which has another name.  */
  if (cris_cpu_version < 8)
    reg_names[CRIS_CC0_REGNUM] = "ccr";
}

/* Return crtl->uses_pic_offset_table.  For use in cris.md,
   since some generated files do not include function.h.  */

int
cris_cfun_uses_pic_table (void)
{
  return crtl->uses_pic_offset_table;
}

/* Worker function for TARGET_CANNOT_FORCE_CONST_MEM.
   We can't put PIC addresses in the constant pool, not even the ones that
   can be reached as pc-relative as we can't tell when or how to do that.  */

static bool
cris_cannot_force_const_mem (machine_mode mode ATTRIBUTE_UNUSED, rtx x)
{
  enum cris_symbol_type t = cris_symbol_type_of (x);

  return
    t == cris_unspec
    || t == cris_got_symbol
    || t == cris_rel_symbol;
}

/* Given an rtx, return the text string corresponding to the CODE of X.
   Intended for use in the assembly language output section of a
   define_insn.  */

const char *
cris_op_str (rtx x)
{
  cris_output_insn_is_bound = 0;
  switch (GET_CODE (x))
    {
    case PLUS:
      return "add";

    case MINUS:
      return "sub";

    case MULT:
      /* This function is for retrieving a part of an instruction name for
	 an operator, for immediate output.  If that ever happens for
	 MULT, we need to apply TARGET_MUL_BUG in the caller.  Make sure
	 we notice.  */
      internal_error ("MULT case in cris_op_str");
      break;

    case DIV:
      return "div";

    case AND:
      return "and";

    case IOR:
      return "or";

    case XOR:
      return "xor";

    case NOT:
      return "not";

    case ASHIFT:
      return "lsl";

    case LSHIFTRT:
      return "lsr";

    case ASHIFTRT:
      return "asr";

    case UMIN:
      /* Used to control the sign/zero-extend character for the 'E' modifier.
	 BOUND has none.  */
      cris_output_insn_is_bound = 1;
      return "bound";

    default:
      return "Unknown operator";
  }
}

/* Emit an error message when we're in an asm, and a fatal error for
   "normal" insns.  Formatted output isn't easily implemented, since we
   use output_operand_lossage to output the actual message and handle the
   categorization of the error.  */

static void
cris_operand_lossage (const char *msgid, rtx op)
{
  debug_rtx (op);
  output_operand_lossage ("%s", msgid);
}

/* Print an index part of an address to file.  */

static void
cris_print_index (rtx index, FILE *file)
{
  /* Make the index "additive" unless we'll output a negative number, in
     which case the sign character is free (as in free beer).  */
  if (!CONST_INT_P (index) || INTVAL (index) >= 0)
    putc ('+', file);

  if (REG_P (index))
    fprintf (file, "$%s.b", reg_names[REGNO (index)]);
  else if (CRIS_CONSTANT_P (index))
    cris_output_addr_const (file, index);
  else if (GET_CODE (index) == MULT)
    {
      fprintf (file, "$%s.",
	       reg_names[REGNO (XEXP (index, 0))]);

      putc (INTVAL (XEXP (index, 1)) == 2 ? 'w' : 'd', file);
    }
  else if (GET_CODE (index) == SIGN_EXTEND && MEM_P (XEXP (index, 0)))
    {
      rtx inner = XEXP (index, 0);
      rtx inner_inner = XEXP (inner, 0);

      if (GET_CODE (inner_inner) == POST_INC)
	{
	  fprintf (file, "[$%s+].",
		   reg_names[REGNO (XEXP (inner_inner, 0))]);
	  putc (GET_MODE (inner) == HImode ? 'w' : 'b', file);
	}
      else
	{
	  fprintf (file, "[$%s].", reg_names[REGNO (inner_inner)]);

	  putc (GET_MODE (inner) == HImode ? 'w' : 'b', file);
	}
    }
  else if (MEM_P (index))
    {
      rtx inner = XEXP (index, 0);
      if (GET_CODE (inner) == POST_INC)
	fprintf (file, "[$%s+].d", reg_names[REGNO (XEXP (inner, 0))]);
      else
	fprintf (file, "[$%s].d", reg_names[REGNO (inner)]);
    }
  else
    cris_operand_lossage ("unexpected index-type in cris_print_index",
			  index);
}

/* Print a base rtx of an address to file.  */

static void
cris_print_base (rtx base, FILE *file)
{
  if (REG_P (base))
    fprintf (file, "$%s", reg_names[REGNO (base)]);
  else if (GET_CODE (base) == POST_INC)
    {
      gcc_assert (REGNO (XEXP (base, 0)) != CRIS_ACR_REGNUM);
      fprintf (file, "$%s+", reg_names[REGNO (XEXP (base, 0))]);
    }
  else
    cris_operand_lossage ("unexpected base-type in cris_print_base",
			  base);
}

/* Usable as a guard in expressions.  */

int
cris_fatal (char *arg)
{
  internal_error (arg);

  /* We'll never get here; this is just to appease compilers.  */
  return 0;
}

/* Return nonzero if REGNO is an ordinary register that *needs* to be
   saved together with other registers, possibly by a MOVEM instruction,
   or is saved for target-independent reasons.  There may be
   target-dependent reasons to save the register anyway; this is just a
   wrapper for a complicated conditional.  */

static int
cris_reg_saved_in_regsave_area (unsigned int regno, bool got_really_used)
{
  return
    (((df_regs_ever_live_p (regno)
       && !call_used_regs[regno])
      || (regno == PIC_OFFSET_TABLE_REGNUM
	  && (got_really_used
	      /* It is saved anyway, if there would be a gap.  */
	      || (flag_pic
		  && df_regs_ever_live_p (regno + 1)
		  && !call_used_regs[regno + 1]))))
     && (regno != FRAME_POINTER_REGNUM || !frame_pointer_needed)
     && regno != CRIS_SRP_REGNUM)
    || (crtl->calls_eh_return
	&& (regno == EH_RETURN_DATA_REGNO (0)
	    || regno == EH_RETURN_DATA_REGNO (1)
	    || regno == EH_RETURN_DATA_REGNO (2)
	    || regno == EH_RETURN_DATA_REGNO (3)));
}

/* The PRINT_OPERAND worker.  */

static void
cris_print_operand (FILE *file, rtx x, int code)
{
  rtx operand = x;

  /* Size-strings corresponding to MULT expressions.  */
  static const char *const mults[] = { "BAD:0", ".b", ".w", "BAD:3", ".d" };

  /* New code entries should just be added to the switch below.  If
     handling is finished, just return.  If handling was just a
     modification of the operand, the modified operand should be put in
     "operand", and then do a break to let default handling
     (zero-modifier) output the operand.  */

  switch (code)
    {
    case 'b':
      /* Print the unsigned supplied integer as if it were signed
	 and < 0, i.e print 255 or 65535 as -1, 254, 65534 as -2, etc.  */
      if (!satisfies_constraint_O (x))
	LOSE_AND_RETURN ("invalid operand for 'b' modifier", x);
      fprintf (file, HOST_WIDE_INT_PRINT_DEC,
	       INTVAL (x)| (INTVAL (x) <= 255 ? ~255 : ~65535));
      return;

    case 'x':
      /* Print assembler code for operator.  */
      fprintf (file, "%s", cris_op_str (operand));
      return;

    case 'o':
      {
	/* A movem modifier working on a parallel; output the register
	   name.  */
	int regno;

	if (GET_CODE (x) != PARALLEL)
	  LOSE_AND_RETURN ("invalid operand for 'o' modifier", x);

	/* The second item can be (set reg (plus reg const)) to denote a
	   postincrement.  */
	regno
	  = (GET_CODE (SET_SRC (XVECEXP (x, 0, 1))) == PLUS
	     ? XVECLEN (x, 0) - 2
	     : XVECLEN (x, 0) - 1);

	fprintf (file, "$%s", reg_names [regno]);
      }
      return;

    case 'O':
      {
	/* A similar movem modifier; output the memory operand.  */
	rtx addr;

	if (GET_CODE (x) != PARALLEL)
	  LOSE_AND_RETURN ("invalid operand for 'O' modifier", x);

	/* The lowest mem operand is in the first item, but perhaps it
	   needs to be output as postincremented.  */
	addr = MEM_P (SET_SRC (XVECEXP (x, 0, 0)))
	  ? XEXP (SET_SRC (XVECEXP (x, 0, 0)), 0)
	  : XEXP (SET_DEST (XVECEXP (x, 0, 0)), 0);

	/* The second item can be a (set reg (plus reg const)) to denote
	   a modification.  */
	if (GET_CODE (SET_SRC (XVECEXP (x, 0, 1))) == PLUS)
	  {
	    /* It's a post-increment, if the address is a naked (reg).  */
	    if (REG_P (addr))
	      addr = gen_rtx_POST_INC (SImode, addr);
	    else
	      {
		/* Otherwise, it's a side-effect; RN=RN+M.  */
		fprintf (file, "[$%s=$%s%s%d]",
			 reg_names [REGNO (SET_DEST (XVECEXP (x, 0, 1)))],
			 reg_names [REGNO (XEXP (addr, 0))],
			 INTVAL (XEXP (addr, 1)) < 0 ? "" : "+",
			 (int) INTVAL (XEXP (addr, 1)));
		return;
	      }
	  }
	output_address (VOIDmode, addr);
      }
      return;

    case 'p':
      /* Adjust a power of two to its log2.  */
      if (!CONST_INT_P (x) || exact_log2 (INTVAL (x)) < 0 )
	LOSE_AND_RETURN ("invalid operand for 'p' modifier", x);
      fprintf (file, "%d", exact_log2 (INTVAL (x)));
      return;

    case 's':
      /* For an integer, print 'b' or 'w' if <= 255 or <= 65535
	 respectively.  This modifier also terminates the inhibiting
         effects of the 'x' modifier.  */
      cris_output_insn_is_bound = 0;
      if (GET_MODE (x) == VOIDmode && CONST_INT_P (x))
	{
	  if (INTVAL (x) >= 0)
	    {
	      if (INTVAL (x) <= 255)
		putc ('b', file);
	      else if (INTVAL (x) <= 65535)
		putc ('w', file);
	      else
		putc ('d', file);
	    }
	  else
	    putc ('d', file);
	  return;
	}

      /* For a non-integer, print the size of the operand.  */
      putc ((GET_MODE (x) == SImode || GET_MODE (x) == SFmode)
	    ? 'd' : GET_MODE (x) == HImode ? 'w'
	    : GET_MODE (x) == QImode ? 'b'
	    /* If none of the above, emit an erroneous size letter.  */
	    : 'X',
	    file);
      return;

    case 'z':
      /* Const_int: print b for -127 <= x <= 255,
	 w for -32768 <= x <= 65535, else die.  */
      if (!CONST_INT_P (x)
	  || INTVAL (x) < -32768 || INTVAL (x) > 65535)
	LOSE_AND_RETURN ("invalid operand for 'z' modifier", x);
      putc (INTVAL (x) >= -128 && INTVAL (x) <= 255 ? 'b' : 'w', file);
      return;

    case 'Z':
      /* If this is a GOT-symbol, print the size-letter corresponding to
	 -fpic/-fPIC.  For everything else, print "d".  */
      putc ((flag_pic == 1
	     && GET_CODE (x) == CONST
	     && GET_CODE (XEXP (x, 0)) == UNSPEC
	     && XINT (XEXP (x, 0), 1) == CRIS_UNSPEC_GOTREAD)
	    ? 'w' : 'd', file);
      return;

    case '#':
      /* Output a 'nop' if there's nothing for the delay slot.
	 This method stolen from the sparc files.  */
      if (dbr_sequence_length () == 0)
	fputs ("\n\tnop", file);
      return;

    case '!':
      /* Output directive for alignment padded with "nop" insns.
	 Optimizing for size, it's plain 4-byte alignment, otherwise we
	 align the section to a cache-line (32 bytes) and skip at max 2
	 bytes, i.e. we skip if it's the last insn on a cache-line.  The
	 latter is faster by a small amount (for two test-programs 99.6%
	 and 99.9%) and larger by a small amount (ditto 100.1% and
	 100.2%).  This is supposed to be the simplest yet performance-
	 wise least intrusive way to make sure the immediately following
	 (supposed) muls/mulu insn isn't located at the end of a
	 cache-line.  */
      if (TARGET_MUL_BUG)
	fputs (optimize_size
	       ? ".p2alignw 2,0x050f\n\t"
	       : ".p2alignw 5,0x050f,2\n\t", file);
      return;

    case ':':
      /* The PIC register.  */
      if (! flag_pic)
	internal_error ("invalid use of ':' modifier");
      fprintf (file, "$%s", reg_names [PIC_OFFSET_TABLE_REGNUM]);
      return;

    case 'H':
      /* Print high (most significant) part of something.  */
      switch (GET_CODE (operand))
	{
	case CONST_INT:
	  /* If we're having 64-bit HOST_WIDE_INTs, the whole (DImode)
	     value is kept here, and so may be other than 0 or -1.  */
	  fprintf (file, HOST_WIDE_INT_PRINT_DEC,
		   INTVAL (operand_subword (operand, 1, 0, DImode)));
	  return;

	case CONST_DOUBLE:
	  /* High part of a long long constant.  */
	  if (GET_MODE (operand) == VOIDmode)
	    {
	      fprintf (file, HOST_WIDE_INT_PRINT_HEX, CONST_DOUBLE_HIGH (x));
	      return;
	    }
	  else
	    LOSE_AND_RETURN ("invalid operand for 'H' modifier", x);

	case REG:
	  /* Print reg + 1.  Check that there's not an attempt to print
	     high-parts of registers like stack-pointer or higher, except
	     for SRP (where the "high part" is MOF).  */
	  if (REGNO (operand) > STACK_POINTER_REGNUM - 2
	      && (REGNO (operand) != CRIS_SRP_REGNUM
		  || CRIS_SRP_REGNUM + 1 != CRIS_MOF_REGNUM
		  || fixed_regs[CRIS_MOF_REGNUM] != 0))
	    LOSE_AND_RETURN ("bad register", operand);
	  fprintf (file, "$%s", reg_names[REGNO (operand) + 1]);
	  return;

	case MEM:
	  /* Adjust memory address to high part.  */
	  {
	    rtx adj_mem = operand;
	    int size
	      = GET_MODE_BITSIZE (GET_MODE (operand)) / BITS_PER_UNIT;

	    /* Adjust so we can use two SImode in DImode.
	       Calling adj_offsettable_operand will make sure it is an
	       offsettable address.  Don't do this for a postincrement
	       though; it should remain as it was.  */
	    if (GET_CODE (XEXP (adj_mem, 0)) != POST_INC)
	      adj_mem
		= adjust_address (adj_mem, GET_MODE (adj_mem), size / 2);

	    output_address (VOIDmode, XEXP (adj_mem, 0));
	    return;
	  }

	default:
	  LOSE_AND_RETURN ("invalid operand for 'H' modifier", x);
	}

    case 'L':
      /* Strip the MEM expression.  */
      operand = XEXP (operand, 0);
      break;

    case 'e':
      /* Like 'E', but ignore state set by 'x'.  FIXME: Use code
	 iterators and attributes in cris.md to avoid the need for %x
	 and %E (and %e) and state passed between those modifiers.  */
      cris_output_insn_is_bound = 0;
      /* FALL THROUGH.  */
    case 'E':
      /* Print 's' if operand is SIGN_EXTEND or 'u' if ZERO_EXTEND unless
	 cris_output_insn_is_bound is nonzero.  */
      if (GET_CODE (operand) != SIGN_EXTEND
	  && GET_CODE (operand) != ZERO_EXTEND
	  && !CONST_INT_P (operand))
	LOSE_AND_RETURN ("invalid operand for 'e' modifier", x);

      if (cris_output_insn_is_bound)
	{
	  cris_output_insn_is_bound = 0;
	  return;
	}

      putc (GET_CODE (operand) == SIGN_EXTEND
	    || (CONST_INT_P (operand) && INTVAL (operand) < 0)
	    ? 's' : 'u', file);
      return;

    case 'm':
      /* Print the size letter of the inner element.  We can do it by
	 calling ourselves with the 's' modifier.  */
      if (GET_CODE (operand) != SIGN_EXTEND && GET_CODE (operand) != ZERO_EXTEND)
	LOSE_AND_RETURN ("invalid operand for 'm' modifier", x);
      cris_print_operand (file, XEXP (operand, 0), 's');
      return;

    case 'M':
      /* Print the least significant part of operand.  */
      if (GET_CODE (operand) == CONST_DOUBLE)
	{
	  fprintf (file, HOST_WIDE_INT_PRINT_HEX, CONST_DOUBLE_LOW (x));
	  return;
	}
      else if (HOST_BITS_PER_WIDE_INT > 32 && CONST_INT_P (operand))
	{
	  fprintf (file, HOST_WIDE_INT_PRINT_HEX,
		   INTVAL (x) & ((unsigned int) 0x7fffffff * 2 + 1));
	  return;
	}
      /* Otherwise the least significant part equals the normal part,
	 so handle it normally.  */
      break;

    case 'A':
      /* When emitting an add for the high part of a DImode constant, we
	 want to use addq for 0 and adds.w for -1.  */
      if (!CONST_INT_P (operand))
	LOSE_AND_RETURN ("invalid operand for 'A' modifier", x);
      fprintf (file, INTVAL (operand) < 0 ? "adds.w" : "addq");
      return;

    case 'P':
      /* For const_int operands, print the additive mnemonic and the
	 modified operand (byte-sized operands don't save anything):
          N=MIN_INT..-65536: add.d N
          -65535..-64: subu.w -N
          -63..-1: subq -N
          0..63: addq N
          64..65535: addu.w N
          65536..MAX_INT: add.d N.
	 (Emitted mnemonics are capitalized to simplify testing.)
	 For anything else (N.B: only register is valid), print "add.d".  */
      if (REG_P (operand))
	{
	  fprintf (file, "Add.d ");

	  /* Deal with printing the operand by dropping through to the
	     normal path.  */
	  break;
	}
      else
	{
	  int val;
	  gcc_assert (CONST_INT_P (operand));

	  val = INTVAL (operand);
	  if (!IN_RANGE (val, -65535, 65535))
	      fprintf (file, "Add.d %d", val);
	  else if (val <= -64)
	    fprintf (file, "Subu.w %d", -val);
	  else if (val <= -1)
	    fprintf (file, "Subq %d", -val);
	  else if (val <= 63)
	      fprintf (file, "Addq %d", val);
	  else if (val <= 65535)
	    fprintf (file, "Addu.w %d", val);
	  return;
	}
      break;

    case 'q':
      /* If the operand is an integer -31..31, print "q" else ".d".  */
      if (CONST_INT_P (operand) && IN_RANGE (INTVAL (operand), -31, 31))
	fprintf (file, "q");
      else
	fprintf (file, ".d");
      return;

    case 'd':
      /* If this is a GOT symbol, force it to be emitted as :GOT and
	 :GOTPLT regardless of -fpic (i.e. not as :GOT16, :GOTPLT16).
	 Avoid making this too much of a special case.  */
      if (flag_pic == 1 && CRIS_CONSTANT_P (operand))
	{
	  int flag_pic_save = flag_pic;

	  flag_pic = 2;
	  cris_output_addr_const (file, operand);
	  flag_pic = flag_pic_save;
	  return;
	}
      break;

    case 'D':
      /* When emitting an sub for the high part of a DImode constant, we
	 want to use subq for 0 and subs.w for -1.  */
      if (!CONST_INT_P (operand))
	LOSE_AND_RETURN ("invalid operand for 'D' modifier", x);
      fprintf (file, INTVAL (operand) < 0 ? "subs.w" : "subq");
      return;

    case 'S':
      /* Print the operand as the index-part of an address.
	 Easiest way out is to use cris_print_index.  */
      cris_print_index (operand, file);
      return;

    case 'T':
      /* Print the size letter for an operand to a MULT, which must be a
	 const_int with a suitable value.  */
      if (!CONST_INT_P (operand) || INTVAL (operand) > 4)
	LOSE_AND_RETURN ("invalid operand for 'T' modifier", x);
      fprintf (file, "%s", mults[INTVAL (operand)]);
      return;

    case 'u':
      /* Print "u.w" if a GOT symbol and flag_pic == 1, else ".d".  */
      if (flag_pic == 1
	  && GET_CODE (operand) == CONST
	  && GET_CODE (XEXP (operand, 0)) == UNSPEC
	  && XINT (XEXP (operand, 0), 1) == CRIS_UNSPEC_GOTREAD)
	fprintf (file, "u.w");
      else
	fprintf (file, ".d");
      return;

    case 0:
      /* No code, print as usual.  */
      break;

    default:
      LOSE_AND_RETURN ("invalid operand modifier letter", x);
    }

  /* Print an operand as without a modifier letter.  */
  switch (GET_CODE (operand))
    {
    case REG:
      if (REGNO (operand) > 15
	  && REGNO (operand) != CRIS_MOF_REGNUM
	  && REGNO (operand) != CRIS_SRP_REGNUM
	  && REGNO (operand) != CRIS_CC0_REGNUM)
	internal_error ("internal error: bad register: %d", REGNO (operand));
      fprintf (file, "$%s", reg_names[REGNO (operand)]);
      return;

    case MEM:
      output_address (GET_MODE (operand), XEXP (operand, 0));
      return;

    case CONST_DOUBLE:
      if (GET_MODE (operand) == VOIDmode)
	/* A long long constant.  */
	output_addr_const (file, operand);
      else
	{
	  /* Only single precision is allowed as plain operands the
	     moment.  */
	  long l;

	  /* FIXME:  Perhaps check overflow of the "single".  */
	  REAL_VALUE_TO_TARGET_SINGLE (*CONST_DOUBLE_REAL_VALUE (operand), l);

	  fprintf (file, "0x%lx", l);
	}
      return;

    case UNSPEC:
      /* Fall through.  */
    case CONST:
      cris_output_addr_const (file, operand);
      return;

    case MULT:
    case ASHIFT:
      {
	/* For a (MULT (reg X) const_int) we output "rX.S".  */
	int i = CONST_INT_P (XEXP (operand, 1))
	  ? INTVAL (XEXP (operand, 1)) : INTVAL (XEXP (operand, 0));
	rtx reg = CONST_INT_P (XEXP (operand, 1))
	  ? XEXP (operand, 0) : XEXP (operand, 1);

	if (!REG_P (reg)
	    || (!CONST_INT_P (XEXP (operand, 0))
		&& !CONST_INT_P (XEXP (operand, 1))))
	  LOSE_AND_RETURN ("unexpected multiplicative operand", x);

	cris_print_base (reg, file);
	fprintf (file, ".%c",
		 i == 0 || (i == 1 && GET_CODE (operand) == MULT) ? 'b'
		 : i == 4 ? 'd'
		 : (i == 2 && GET_CODE (operand) == MULT) || i == 1 ? 'w'
		 : 'd');
	return;
      }

    default:
      /* No need to handle all strange variants, let output_addr_const
	 do it for us.  */
      if (CRIS_CONSTANT_P (operand))
	{
	  cris_output_addr_const (file, operand);
	  return;
	}

      LOSE_AND_RETURN ("unexpected operand", x);
    }
}

static bool
cris_print_operand_punct_valid_p (unsigned char code)
{
  return (code == '#' || code == '!' || code == ':');
}

/* The PRINT_OPERAND_ADDRESS worker.  */

static void
cris_print_operand_address (FILE *file, machine_mode /*mode*/, rtx x)
{
  /* All these were inside MEM:s so output indirection characters.  */
  putc ('[', file);

  if (CONSTANT_ADDRESS_P (x))
    cris_output_addr_const (file, x);
  else if (cris_base_or_autoincr_p (x, true))
    cris_print_base (x, file);
  else if (GET_CODE (x) == PLUS)
    {
      rtx x1, x2;

      x1 = XEXP (x, 0);
      x2 = XEXP (x, 1);
      if (cris_base_p (x1, true))
	{
	  cris_print_base (x1, file);
	  cris_print_index (x2, file);
	}
      else if (cris_base_p (x2, true))
	{
	  cris_print_base (x2, file);
	  cris_print_index (x1, file);
	}
      else
	LOSE_AND_RETURN ("unrecognized address", x);
    }
  else if (MEM_P (x))
    {
      /* A DIP.  Output more indirection characters.  */
      putc ('[', file);
      cris_print_base (XEXP (x, 0), file);
      putc (']', file);
    }
  else
    LOSE_AND_RETURN ("unrecognized address", x);

  putc (']', file);
}

/* The RETURN_ADDR_RTX worker.
   We mark that the return address is used, either by EH or
   __builtin_return_address, for use by the function prologue and
   epilogue.  FIXME: This isn't optimal; we just use the mark in the
   prologue and epilogue to say that the return address is to be stored
   in the stack frame.  We could return SRP for leaf-functions and use the
   initial-value machinery.  */

rtx
cris_return_addr_rtx (int count, rtx frameaddr ATTRIBUTE_UNUSED)
{
  cfun->machine->needs_return_address_on_stack = 1;

  /* The return-address is stored just above the saved frame-pointer (if
     present).  Apparently we can't eliminate from the frame-pointer in
     that direction, so use the incoming args (maybe pretended) pointer.  */
  return count == 0
    ? gen_rtx_MEM (Pmode, plus_constant (Pmode, virtual_incoming_args_rtx, -4))
    : NULL_RTX;
}

/* Accessor used in cris.md:return because cfun->machine isn't available
   there.  */

bool
cris_return_address_on_stack (void)
{
  return df_regs_ever_live_p (CRIS_SRP_REGNUM)
    || cfun->machine->needs_return_address_on_stack;
}

/* Accessor used in cris.md:return because cfun->machine isn't available
   there.  */

bool
cris_return_address_on_stack_for_return (void)
{
  return cfun->machine->return_type == CRIS_RETINSN_RET ? false
    : cris_return_address_on_stack ();
}

/* This handles FP -> SP elimination offset.  */

static int
cris_initial_frame_pointer_offset (void)
{
  int regno;

  /* Initial offset is 0 if we don't have a frame pointer.  */
  int offs = 0;
  bool got_really_used = false;

  if (crtl->uses_pic_offset_table)
    {
      push_topmost_sequence ();
      got_really_used
	= reg_used_between_p (pic_offset_table_rtx, get_insns (),
			      NULL);
      pop_topmost_sequence ();
    }

  /* And 4 for each register pushed.  */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (cris_reg_saved_in_regsave_area (regno, got_really_used))
      offs += 4;

  /* And then, last, we add the locals allocated.  */
  offs += get_frame_size ();

  /* And more; the accumulated args size.  */
  offs += crtl->outgoing_args_size;

  /* Then round it off, in case we use aligned stack.  */
  if (TARGET_STACK_ALIGN)
    offs = TARGET_ALIGN_BY_32 ? (offs + 3) & ~3 : (offs + 1) & ~1;

  return offs;
}

/* The INITIAL_ELIMINATION_OFFSET worker.
   Calculate the difference between imaginary registers such as frame
   pointer and the stack pointer.  Used to eliminate the frame pointer
   and imaginary arg pointer.  */

int
cris_initial_elimination_offset (int fromreg, int toreg)
{
  int fp_sp_offset
    = cris_initial_frame_pointer_offset ();

  /* We should be able to use regs_ever_live and related prologue
     information here, or alpha should not as well.  */
  bool return_address_on_stack = cris_return_address_on_stack ();

  /* Here we act as if the frame-pointer were needed.  */
  int ap_fp_offset = 4 + (return_address_on_stack ? 4 : 0);

  if (fromreg == ARG_POINTER_REGNUM
      && toreg == FRAME_POINTER_REGNUM)
    return ap_fp_offset;

  /* Between the frame pointer and the stack are only "normal" stack
     variables and saved registers.  */
  if (fromreg == FRAME_POINTER_REGNUM
      && toreg == STACK_POINTER_REGNUM)
    return fp_sp_offset;

  /* We need to balance out the frame pointer here.  */
  if (fromreg == ARG_POINTER_REGNUM
      && toreg == STACK_POINTER_REGNUM)
    return ap_fp_offset + fp_sp_offset - 4;

  gcc_unreachable ();
}

/* Nonzero if X is a hard reg that can be used as an index.  */
static inline bool
reg_ok_for_base_p (const_rtx x, bool strict)
{
  return ((! strict && ! HARD_REGISTER_P (x))
          || REGNO_OK_FOR_BASE_P (REGNO (x)));
}

/* Nonzero if X is a hard reg that can be used as an index.  */
static inline bool
reg_ok_for_index_p (const_rtx x, bool strict)
{
  return reg_ok_for_base_p (x, strict);
}

/* No symbol can be used as an index (or more correct, as a base) together
   with a register with PIC; the PIC register must be there.  */

bool
cris_constant_index_p (const_rtx x)
{
  return (CRIS_CONSTANT_P (x) && (!flag_pic || cris_valid_pic_const (x, true)));
}

/* True if X is a valid base register.  */

bool
cris_base_p (const_rtx x, bool strict)
{
  return (REG_P (x) && reg_ok_for_base_p (x, strict));
}

/* True if X is a valid index register.  */

static inline bool
cris_index_p (const_rtx x, bool strict)
{
  return (REG_P (x) && reg_ok_for_index_p (x, strict));
}

/* True if X is a valid base register with or without autoincrement.  */

bool
cris_base_or_autoincr_p (const_rtx x, bool strict)
{
  return (cris_base_p (x, strict)
	  || (GET_CODE (x) == POST_INC
	      && cris_base_p (XEXP (x, 0), strict)
	      && REGNO (XEXP (x, 0)) != CRIS_ACR_REGNUM));
}

/* True if X is a valid (register) index for BDAP, i.e. [Rs].S or [Rs+].S.  */

bool
cris_bdap_index_p (const_rtx x, bool strict)
{
  return ((MEM_P (x)
	   && GET_MODE (x) == SImode
	   && cris_base_or_autoincr_p (XEXP (x, 0), strict))
	  || (GET_CODE (x) == SIGN_EXTEND
	      && MEM_P (XEXP (x, 0))
	      && (GET_MODE (XEXP (x, 0)) == HImode
		  || GET_MODE (XEXP (x, 0)) == QImode)
	      && cris_base_or_autoincr_p (XEXP (XEXP (x, 0), 0), strict)));
}

/* True if X is a valid (register) index for BIAP, i.e. Rd.m.  */

bool
cris_biap_index_p (const_rtx x, bool strict)
{
  return (cris_index_p (x, strict)
	  || (GET_CODE (x) == MULT
	      && cris_index_p (XEXP (x, 0), strict)
	      && cris_scale_int_operand (XEXP (x, 1), VOIDmode)));
}

/* Worker function for TARGET_LEGITIMATE_ADDRESS_P.

   A PIC operand looks like a normal symbol here.  At output we dress it
   in "[rPIC+symbol:GOT]" (global symbol) or "rPIC+symbol:GOTOFF" (local
   symbol) so we exclude all addressing modes where we can't replace a
   plain "symbol" with that.  A global PIC symbol does not fit anywhere
   here (but is thankfully a general_operand in itself).  A local PIC
   symbol is valid for the plain "symbol + offset" case.  */

bool
cris_legitimate_address_p (machine_mode mode, rtx x, bool strict)
{
  const_rtx x1, x2;

  if (cris_base_or_autoincr_p (x, strict))
    return true;
  else if (TARGET_V32)
    /* Nothing else is valid then.  */
    return false;
  else if (cris_constant_index_p (x))
    return true;
  /* Indexed?  */
  else if (GET_CODE (x) == PLUS)
    {
      x1 = XEXP (x, 0);
      x2 = XEXP (x, 1);
      /* BDAP o, Rd.  */
      if ((cris_base_p (x1, strict) && cris_constant_index_p (x2))
	  || (cris_base_p (x2, strict) && cris_constant_index_p (x1))
	   /* BDAP Rs[+], Rd.  */
	  || (GET_MODE_SIZE (mode) <= UNITS_PER_WORD
	      && ((cris_base_p (x1, strict)
		   && cris_bdap_index_p (x2, strict))
		  || (cris_base_p (x2, strict)
		      && cris_bdap_index_p (x1, strict))
		  /* BIAP.m Rs, Rd */
		  || (cris_base_p (x1, strict)
		      && cris_biap_index_p (x2, strict))
		  || (cris_base_p (x2, strict)
		      && cris_biap_index_p (x1, strict)))))
	return true;
     }
  else if (MEM_P (x))
    {
      /* DIP (Rs).  Reject [[reg+]] and [[reg]] for DImode (long long).  */
      if (GET_MODE_SIZE (mode) <= UNITS_PER_WORD
	  && cris_base_or_autoincr_p (XEXP (x, 0), strict))
	return true;
    }

  return false;
}

/* Worker function for TARGET_LEGITIMATE_CONSTANT_P.  We have to handle
   PIC constants that aren't legitimized.  FIXME: there used to be a
   guarantee that the target LEGITIMATE_CONSTANT_P didn't have to handle
   PIC constants, but no more (4.7 era); testcase: glibc init-first.c.
   While that may be seen as a bug, that guarantee seems a wart by design,
   so don't bother; fix the documentation instead.  */

bool
cris_legitimate_constant_p (machine_mode mode ATTRIBUTE_UNUSED, rtx x)
{
  enum cris_symbol_type t;

  if (flag_pic)
    return LEGITIMATE_PIC_OPERAND_P (x);

  t = cris_symbol_type_of (x);

  return
    t == cris_no_symbol
    || t == cris_offsettable_symbol
    || t == cris_unspec;
}

/* Worker function for LEGITIMIZE_RELOAD_ADDRESS.  */

bool
cris_reload_address_legitimized (rtx x,
				 machine_mode mode ATTRIBUTE_UNUSED,
				 int opnum ATTRIBUTE_UNUSED,
				 int itype,
				 int ind_levels ATTRIBUTE_UNUSED)
{
  enum reload_type type = (enum reload_type) itype;
  rtx op0, op1;
  rtx *op1p;

  if (GET_CODE (x) != PLUS)
    return false;

  if (TARGET_V32)
    return false;

  op0 = XEXP (x, 0);
  op1 = XEXP (x, 1);
  op1p = &XEXP (x, 1);

  if (!REG_P (op1))
    return false;

  if (GET_CODE (op0) == SIGN_EXTEND && MEM_P (XEXP (op0, 0)))
    {
      rtx op00 = XEXP (op0, 0);
      rtx op000 = XEXP (op00, 0);
      rtx *op000p = &XEXP (op00, 0);

      if ((GET_MODE (op00) == HImode || GET_MODE (op00) == QImode)
	  && (REG_P (op000)
	      || (GET_CODE (op000) == POST_INC && REG_P (XEXP (op000, 0)))))
	{
	  bool something_reloaded = false;

	  if (GET_CODE (op000) == POST_INC
	      && REG_P (XEXP (op000, 0))
	      && REGNO (XEXP (op000, 0)) > CRIS_LAST_GENERAL_REGISTER)
	    /* No, this gets too complicated and is too rare to care
	       about trying to improve on the general code Here.
	       As the return-value is an all-or-nothing indicator, we
	       punt on the other register too.  */
	    return false;

	  if ((REG_P (op000)
	       && REGNO (op000) > CRIS_LAST_GENERAL_REGISTER))
	    {
	      /* The address of the inner mem is a pseudo or wrong
		 reg: reload that.  */
	      push_reload (op000, NULL_RTX, op000p, NULL, GENERAL_REGS,
			   GET_MODE (x), VOIDmode, 0, 0, opnum, type);
	      something_reloaded = true;
	    }

	  if (REGNO (op1) > CRIS_LAST_GENERAL_REGISTER)
	    {
	      /* Base register is a pseudo or wrong reg: reload it.  */
	      push_reload (op1, NULL_RTX, op1p, NULL, GENERAL_REGS,
			   GET_MODE (x), VOIDmode, 0, 0,
			   opnum, type);
	      something_reloaded = true;
	    }

	  gcc_assert (something_reloaded);

	  return true;
	}
    }

  return false;
}


/* Worker function for TARGET_PREFERRED_RELOAD_CLASS.

   It seems like gcc (2.7.2 and 2.9x of 2000-03-22) may send "NO_REGS" as
   the class for a constant (testcase: __Mul in arit.c).  To avoid forcing
   out a constant into the constant pool, we will trap this case and
   return something a bit more sane.  FIXME: Check if this is a bug.
   Beware that we must not "override" classes that can be specified as
   constraint letters, or else asm operands using them will fail when
   they need to be reloaded.  FIXME: Investigate whether that constitutes
   a bug.  */

static reg_class_t
cris_preferred_reload_class (rtx x ATTRIBUTE_UNUSED, reg_class_t rclass)
{
  if (rclass != ACR_REGS
      && rclass != MOF_REGS
      && rclass != MOF_SRP_REGS
      && rclass != SRP_REGS
      && rclass != CC0_REGS
      && rclass != SPECIAL_REGS)
    return GENNONACR_REGS;

  return rclass;
}

/* Worker function for TARGET_REGISTER_MOVE_COST.  */

static int
cris_register_move_cost (machine_mode mode ATTRIBUTE_UNUSED,
			 reg_class_t from, reg_class_t to)
{
  /* Can't move to and from a SPECIAL_REGS register, so we have to say
     their move cost within that class is higher.  How about 7?  That's 3
     for a move to a GENERAL_REGS register, 3 for the move from the
     GENERAL_REGS register, and 1 for the increased register pressure.
     Also, it's higher than the memory move cost, as it should.
     We also do this for ALL_REGS, since we don't want that class to be
     preferred (even to memory) at all where GENERAL_REGS doesn't fit.
     Whenever it's about to be used, it's for SPECIAL_REGS.  If we don't
     present a higher cost for ALL_REGS than memory, a SPECIAL_REGS may be
     used when a GENERAL_REGS should be used, even if there are call-saved
     GENERAL_REGS left to allocate.  This is because the fall-back when
     the most preferred register class isn't available, isn't the next
     (or next good) wider register class, but the *most widest* register
     class.  FIXME: pre-IRA comment, perhaps obsolete now.  */

  if ((reg_classes_intersect_p (from, SPECIAL_REGS)
       && reg_classes_intersect_p (to, SPECIAL_REGS))
      || from == ALL_REGS || to == ALL_REGS)
    return 7;

  /* Make moves to/from SPECIAL_REGS slightly more expensive, as we
     generally prefer GENERAL_REGS.  */
  if (reg_classes_intersect_p (from, SPECIAL_REGS)
      || reg_classes_intersect_p (to, SPECIAL_REGS))
    return 3;

  return 2;
}

/* Worker function for TARGET_MEMORY_MOVE_COST.

   This isn't strictly correct for v0..3 in buswidth-8bit mode, but should
   suffice.  */

static int
cris_memory_move_cost (machine_mode mode,
                       reg_class_t rclass ATTRIBUTE_UNUSED,
                       bool in ATTRIBUTE_UNUSED)
{
  if (mode == QImode
      || mode == HImode)
    return 4;
  else
    return 6;
}

/* Worker for cris_notice_update_cc; handles the "normal" cases.
   FIXME: this code is historical; its functionality should be
   refactored to look at insn attributes and moved to
   cris_notice_update_cc.  Except, we better lose cc0 entirely.  */

static void
cris_normal_notice_update_cc (rtx exp, rtx insn)
{
  /* "Normal" means, for:
     (set (cc0) (...)):
     CC is (...).

     (set (reg) (...)):
     CC is (reg) and (...) - unless (...) is 0 or reg is a special
        register or (v32 and (...) is -32..-1), then CC does not change.
     CC_NO_OVERFLOW unless (...) is reg or mem.

     (set (mem) (...)):
     CC does not change.

     (set (pc) (...)):
     CC does not change.

     (parallel
      (set (reg1) (mem (bdap/biap)))
      (set (reg2) (bdap/biap))):
     CC is (reg1) and (mem (reg2))

     (parallel
      (set (mem (bdap/biap)) (reg1)) [or 0]
      (set (reg2) (bdap/biap))):
     CC does not change.

     (where reg and mem includes strict_low_parts variants thereof)

     For all others, assume CC is clobbered.
     Note that we do not have to care about setting CC_NO_OVERFLOW,
     since the overflow flag is set to 0 (i.e. right) for
     instructions where it does not have any sane sense, but where
     other flags have meanings.  (This includes shifts; the carry is
     not set by them).

     Note that there are other parallel constructs we could match,
     but we don't do that yet.  */

  if (GET_CODE (exp) == SET)
    {
      /* FIXME: Check when this happens.  It looks like we should
	 actually do a CC_STATUS_INIT here to be safe.  */
      if (SET_DEST (exp) == pc_rtx)
	return;

      /* Record CC0 changes, so we do not have to output multiple
	 test insns.  */
      if (SET_DEST (exp) == cc0_rtx)
	{
	  CC_STATUS_INIT;

	  if (GET_CODE (SET_SRC (exp)) == COMPARE
	      && XEXP (SET_SRC (exp), 1) == const0_rtx)
	    cc_status.value1 = XEXP (SET_SRC (exp), 0);
	  else
	    cc_status.value1 = SET_SRC (exp);

          /* Handle flags for the special btstq on one bit.  */
	  if (GET_CODE (cc_status.value1) == ZERO_EXTRACT
	      && XEXP (cc_status.value1, 1) == const1_rtx)
	    {
	      if (CONST_INT_P (XEXP (cc_status.value1, 0)))
		/* Using cmpq.  */
		cc_status.flags = CC_INVERTED;
	      else
		/* A one-bit btstq.  */
		cc_status.flags = CC_Z_IN_NOT_N;
	    }

	  else if (GET_CODE (SET_SRC (exp)) == COMPARE)
	    {
	      if (!REG_P (XEXP (SET_SRC (exp), 0))
		  && XEXP (SET_SRC (exp), 1) != const0_rtx)
		/* For some reason gcc will not canonicalize compare
		   operations, reversing the sign by itself if
		   operands are in wrong order.  */
		/* (But NOT inverted; eq is still eq.) */
		cc_status.flags = CC_REVERSED;

	      /* This seems to be overlooked by gcc.  FIXME: Check again.
		 FIXME:  Is it really safe?  */
	      cc_status.value2
		= gen_rtx_MINUS (GET_MODE (SET_SRC (exp)),
				 XEXP (SET_SRC (exp), 0),
				 XEXP (SET_SRC (exp), 1));
	    }
	  return;
	}
      else if (REG_P (SET_DEST (exp))
	       || (GET_CODE (SET_DEST (exp)) == STRICT_LOW_PART
		   && REG_P (XEXP (SET_DEST (exp), 0))))
	{
	  /* A register is set; normally CC is set to show that no
	     test insn is needed.  Catch the exceptions.  */

	  /* If not to cc0, then no "set"s in non-natural mode give
	     ok cc0...  */
	  if (GET_MODE_SIZE (GET_MODE (SET_DEST (exp))) > UNITS_PER_WORD
	      || GET_MODE_CLASS (GET_MODE (SET_DEST (exp))) == MODE_FLOAT)
	    {
	      /* ... except add:s and sub:s in DImode.  */
	      if (GET_MODE (SET_DEST (exp)) == DImode
		  && (GET_CODE (SET_SRC (exp)) == PLUS
		      || GET_CODE (SET_SRC (exp)) == MINUS))
		{
		  CC_STATUS_INIT;
		  cc_status.value1 = SET_DEST (exp);
		  cc_status.value2 = SET_SRC (exp);

		  if (cris_reg_overlap_mentioned_p (cc_status.value1,
						    cc_status.value2))
		    cc_status.value2 = 0;

		  /* Add and sub may set V, which gets us
		     unoptimizable results in "gt" and "le" condition
		     codes.  */
		  cc_status.flags |= CC_NO_OVERFLOW;

		  return;
		}
	    }
	  else if (SET_SRC (exp) == const0_rtx
		   || (REG_P (SET_SRC (exp))
		       && (REGNO (SET_SRC (exp))
			   > CRIS_LAST_GENERAL_REGISTER))
		   || (TARGET_V32
		       && REG_P (SET_DEST (exp))
		       && satisfies_constraint_I (SET_SRC (exp))))
	    {
	      /* There's no CC0 change for this case.  Just check
		 for overlap.  */
	      if (cc_status.value1
		  && modified_in_p (cc_status.value1, insn))
		cc_status.value1 = 0;

	      if (cc_status.value2
		  && modified_in_p (cc_status.value2, insn))
		cc_status.value2 = 0;

	      return;
	    }
	  else
	    {
	      CC_STATUS_INIT;
	      cc_status.value1 = SET_DEST (exp);
	      cc_status.value2 = SET_SRC (exp);

	      if (cris_reg_overlap_mentioned_p (cc_status.value1,
						cc_status.value2))
		cc_status.value2 = 0;

	      /* Some operations may set V, which gets us
		 unoptimizable results in "gt" and "le" condition
		 codes.  */
	      if (GET_CODE (SET_SRC (exp)) == PLUS
		  || GET_CODE (SET_SRC (exp)) == MINUS
		  || GET_CODE (SET_SRC (exp)) == NEG)
		cc_status.flags |= CC_NO_OVERFLOW;

	      /* For V32, nothing with a register destination sets
		 C and V usefully.  */
	      if (TARGET_V32)
		cc_status.flags |= CC_NO_OVERFLOW;

	      return;
	    }
	}
      else if (MEM_P (SET_DEST (exp))
	       || (GET_CODE (SET_DEST (exp)) == STRICT_LOW_PART
		   && MEM_P (XEXP (SET_DEST (exp), 0))))
	{
	  /* When SET to MEM, then CC is not changed (except for
	     overlap).  */
	  if (cc_status.value1
	      && modified_in_p (cc_status.value1, insn))
	    cc_status.value1 = 0;

	  if (cc_status.value2
	      && modified_in_p (cc_status.value2, insn))
	    cc_status.value2 = 0;

	  return;
	}
    }
  else if (GET_CODE (exp) == PARALLEL)
    {
      if (GET_CODE (XVECEXP (exp, 0, 0)) == SET
	  && GET_CODE (XVECEXP (exp, 0, 1)) == SET
	  && REG_P (XEXP (XVECEXP (exp, 0, 1), 0)))
	{
	  if (REG_P (XEXP (XVECEXP (exp, 0, 0), 0))
	      && MEM_P (XEXP (XVECEXP (exp, 0, 0), 1)))
	    {
	      CC_STATUS_INIT;

	      /* For "move.S [rx=ry+o],rz", say CC reflects
		 value1=rz and value2=[rx] */
	      cc_status.value1 = XEXP (XVECEXP (exp, 0, 0), 0);
	      cc_status.value2
		= replace_equiv_address (XEXP (XVECEXP (exp, 0, 0), 1),
					 XEXP (XVECEXP (exp, 0, 1), 0));

	      /* Huh?  A side-effect cannot change the destination
		 register.  */
	      if (cris_reg_overlap_mentioned_p (cc_status.value1,
						cc_status.value2))
		internal_error ("internal error: sideeffect-insn affecting main effect");

	      /* For V32, moves to registers don't set C and V.  */
	      if (TARGET_V32)
		cc_status.flags |= CC_NO_OVERFLOW;
	      return;
	    }
	  else if ((REG_P (XEXP (XVECEXP (exp, 0, 0), 1))
		    || XEXP (XVECEXP (exp, 0, 0), 1) == const0_rtx)
		   && MEM_P (XEXP (XVECEXP (exp, 0, 0), 0)))
	    {
	      /* For "move.S rz,[rx=ry+o]" and "clear.S [rx=ry+o]",
		 say flags are not changed, except for overlap.  */
	      if (cc_status.value1
		  && modified_in_p (cc_status.value1, insn))
		cc_status.value1 = 0;

	      if (cc_status.value2
		  && modified_in_p (cc_status.value2, insn))
		cc_status.value2 = 0;

	      return;
	    }
	}
    }

  /* If we got here, the case wasn't covered by the code above.  */
  CC_STATUS_INIT;
}

/*  This function looks into the pattern to see how this insn affects
    condition codes.

    Used when to eliminate test insns before a condition-code user,
    such as a "scc" insn or a conditional branch.  This includes
    checking if the entities that cc was updated by, are changed by the
    operation.

    Currently a jumble of the old peek-inside-the-insn and the newer
    check-cc-attribute methods.  */

void
cris_notice_update_cc (rtx exp, rtx_insn *insn)
{
  enum attr_cc attrval = get_attr_cc (insn);

  /* Check if user specified "-mcc-init" as a bug-workaround.  Remember
     to still set CC_REVERSED as below, since that's required by some
     compare insn alternatives.  (FIXME: GCC should do this virtual
     operand swap by itself.)  A test-case that may otherwise fail is
     gcc.c-torture/execute/20000217-1.c -O0 and -O1.  */
  if (TARGET_CCINIT)
    {
      CC_STATUS_INIT;

      if (attrval == CC_REV)
	cc_status.flags = CC_REVERSED;
      return;
    }

  /* Slowly, we're converting to using attributes to control the setting
     of condition-code status.  */
  switch (attrval)
    {
    case CC_NONE:
      /* Even if it is "none", a setting may clobber a previous
	 cc-value, so check.  */
      if (GET_CODE (exp) == SET)
	{
	  if (cc_status.value1
	      && modified_in_p (cc_status.value1, insn))
	    cc_status.value1 = 0;

	  if (cc_status.value2
	      && modified_in_p (cc_status.value2, insn))
	    cc_status.value2 = 0;
	}
      return;

    case CC_CLOBBER:
      CC_STATUS_INIT;
      return;

    case CC_REV:
    case CC_NOOV32:
    case CC_NORMAL:
      cris_normal_notice_update_cc (exp, insn);

      /* The "test" insn doesn't clear (carry and) overflow on V32.  We
        can change bge => bpl and blt => bmi by passing on to the cc0
        user that V should not be considered; bgt and ble are taken
        care of by other methods (see {tst,cmp}{si,hi,qi}).  */
      if (attrval == CC_NOOV32 && TARGET_V32)
	cc_status.flags |= CC_NO_OVERFLOW;
      return;

    default:
      internal_error ("unknown cc_attr value");
    }

  CC_STATUS_INIT;
}

/* Return != 0 if the return sequence for the current function is short,
   like "ret" or "jump [sp+]".  Prior to reloading, we can't tell if
   registers must be saved, so return 0 then.  */

bool
cris_simple_epilogue (void)
{
  unsigned int regno;
  unsigned int reglimit = STACK_POINTER_REGNUM;
  bool got_really_used = false;

  if (! reload_completed
      || frame_pointer_needed
      || get_frame_size () != 0
      || crtl->args.pretend_args_size
      || crtl->args.size
      || crtl->outgoing_args_size
      || crtl->calls_eh_return

      /* If we're not supposed to emit prologue and epilogue, we must
	 not emit return-type instructions.  */
      || !TARGET_PROLOGUE_EPILOGUE)
    return false;

  /* Can't return from stacked return address with v32.  */
  if (TARGET_V32 && cris_return_address_on_stack ())
    return false;

  if (crtl->uses_pic_offset_table)
    {
      push_topmost_sequence ();
      got_really_used
	= reg_used_between_p (pic_offset_table_rtx, get_insns (), NULL);
      pop_topmost_sequence ();
    }

  /* No simple epilogue if there are saved registers.  */
  for (regno = 0; regno < reglimit; regno++)
    if (cris_reg_saved_in_regsave_area (regno, got_really_used))
      return false;

  return true;
}

/* Emit checking that MEM is aligned for an access in MODE, failing
   that, executing a "break 8" (or call to abort, if "break 8" is
   disabled).  */

void
cris_emit_trap_for_misalignment (rtx mem)
{
  rtx addr, reg, ok_label, andop;
  rtx_insn *jmp;
  int natural_alignment;
  gcc_assert (MEM_P (mem));

  natural_alignment = GET_MODE_SIZE (GET_MODE (mem));
  addr = XEXP (mem, 0);
  reg = force_reg (Pmode, addr);
  ok_label = gen_label_rtx ();

  /* This will yield a btstq without a separate register used, usually -
     with the exception for PRE hoisting the "and" but not the branch
     around the trap: see testsuite/gcc.target/cris/sync-3s.c.  */
  andop = gen_rtx_AND (Pmode, reg, GEN_INT (natural_alignment - 1));
  emit_cmp_and_jump_insns (force_reg (SImode, andop), const0_rtx, EQ,
			   NULL_RTX, Pmode, 1, ok_label);
  jmp = get_last_insn ();
  gcc_assert (JUMP_P (jmp));

  predict_insn_def (jmp, PRED_NORETURN, TAKEN);
  expand_builtin_trap ();
  emit_label (ok_label);
}

/* Expand a return insn (just one insn) marked as using SRP or stack
   slot depending on parameter ON_STACK.  */

void
cris_expand_return (bool on_stack)
{
  /* FIXME: emit a parallel with a USE for SRP or the stack-slot, to
     tell "ret" from "jump [sp+]".  Some, but not all, other parts of
     GCC expect just (return) to do the right thing when optimizing, so
     we do that until they're fixed.  Currently, all return insns in a
     function must be the same (not really a limiting factor) so we need
     to check that it doesn't change half-way through.  */
  emit_jump_insn (ret_rtx);

  CRIS_ASSERT (cfun->machine->return_type != CRIS_RETINSN_RET || !on_stack);
  CRIS_ASSERT (cfun->machine->return_type != CRIS_RETINSN_JUMP || on_stack);

  cfun->machine->return_type
    = on_stack ? CRIS_RETINSN_JUMP : CRIS_RETINSN_RET;
}

/* Compute a (partial) cost for rtx X.  Return true if the complete
   cost has been computed, and false if subexpressions should be
   scanned.  In either case, *TOTAL contains the cost result.  */

static bool
cris_rtx_costs (rtx x, machine_mode mode, int outer_code, int opno,
		int *total, bool speed)
{
  int code = GET_CODE (x);

  switch (code)
    {
    case CONST_INT:
      {
	HOST_WIDE_INT val = INTVAL (x);
	if (val == 0)
	  *total = 0;
	else if (val < 32 && val >= -32)
	  *total = 1;
	/* Eight or 16 bits are a word and cycle more expensive.  */
	else if (val <= 32767 && val >= -32768)
	  *total = 2;
	/* A 32-bit constant (or very seldom, unsigned 16 bits) costs
	   another word.  FIXME: This isn't linear to 16 bits.  */
	else
	  *total = 4;
	return true;
      }

    case LABEL_REF:
      *total = 6;
      return true;

    case CONST:
    case SYMBOL_REF:
      *total = 6;
      return true;

    case CONST_DOUBLE:
      if (x != CONST0_RTX (mode == VOIDmode ? DImode : mode))
	*total = 12;
      else
        /* Make 0.0 cheap, else test-insns will not be used.  */
	*total = 0;
      return true;

    case MULT:
      /* If we have one arm of an ADDI, make sure it gets the cost of
	 one insn, i.e. zero cost for this operand, and just the cost
	 of the PLUS, as the insn is created by combine from a PLUS
	 and an ASHIFT, and the MULT cost below would make the
	 combined value be larger than the separate insns.  The insn
	 validity is checked elsewhere by combine.

	 FIXME: this case is a stop-gap for 4.3 and 4.4, this whole
	 function should be rewritten.  */
      if (outer_code == PLUS && cris_biap_index_p (x, false))
	{
	  *total = 0;
	  return true;
	}

      /* Identify values that are no powers of two.  Powers of 2 are
         taken care of already and those values should not be changed.  */
      if (!CONST_INT_P (XEXP (x, 1))
          || exact_log2 (INTVAL (XEXP (x, 1)) < 0))
	{
	  /* If we have a multiply insn, then the cost is between
	     1 and 2 "fast" instructions.  */
	  if (TARGET_HAS_MUL_INSNS)
	    {
	      *total = COSTS_N_INSNS (1) + COSTS_N_INSNS (1) / 2;
	      return true;
	    }

	  /* Estimate as 4 + 4 * #ofbits.  */
	  *total = COSTS_N_INSNS (132);
	  return true;
	}
      return false;

    case UDIV:
    case MOD:
    case UMOD:
    case DIV:
      if (!CONST_INT_P (XEXP (x, 1))
          || exact_log2 (INTVAL (XEXP (x, 1)) < 0))
	{
	  /* Estimate this as 4 + 8 * #of bits.  */
	  *total = COSTS_N_INSNS (260);
	  return true;
	}
      return false;

    case AND:
      if (CONST_INT_P (XEXP (x, 1))
          /* Two constants may actually happen before optimization.  */
          && !CONST_INT_P (XEXP (x, 0))
	  && !satisfies_constraint_I (XEXP (x, 1)))
	{
	  *total
	    = (rtx_cost (XEXP (x, 0), mode, (enum rtx_code) outer_code,
			 opno, speed) + 2
	       + 2 * GET_MODE_NUNITS (mode));
	  return true;
	}
      return false;

    case ZERO_EXTRACT:
      if (outer_code != COMPARE)
        return false;
      /* fall through */

    case ZERO_EXTEND: case SIGN_EXTEND:
      *total = rtx_cost (XEXP (x, 0), VOIDmode, (enum rtx_code) outer_code,
			 opno, speed);
      return true;

    default:
      return false;
    }
}

/* The ADDRESS_COST worker.  */

static int
cris_address_cost (rtx x, machine_mode mode ATTRIBUTE_UNUSED,
		   addr_space_t as ATTRIBUTE_UNUSED,
		   bool speed ATTRIBUTE_UNUSED)
{
  /* The metric to use for the cost-macros is unclear.
     The metric used here is (the number of cycles needed) / 2,
     where we consider equal a cycle for a word of code and a cycle to
     read memory.  FIXME: Adding "+ 1" to all values would avoid
     returning 0, as tree-ssa-loop-ivopts.c as of r128272 "normalizes"
     0 to 1, thereby giving equal costs to [rN + rM] and [rN].
     Unfortunately(?) such a hack would expose other pessimizations,
     at least with g++.dg/tree-ssa/ivopts-1.C, adding insns to the
     loop there, without apparent reason.  */

  /* The cheapest addressing modes get 0, since nothing extra is needed.  */
  if (cris_base_or_autoincr_p (x, false))
    return 0;

  /* An indirect mem must be a DIP.  This means two bytes extra for code,
     and 4 bytes extra for memory read, i.e.  (2 + 4) / 2.  */
  if (MEM_P (x))
    return (2 + 4) / 2;

  /* Assume (2 + 4) / 2 for a single constant; a dword, since it needs
     an extra DIP prefix and 4 bytes of constant in most cases.  */
  if (CONSTANT_P (x))
    return (2 + 4) / 2;

  /* Handle BIAP and BDAP prefixes.  */
  if (GET_CODE (x) == PLUS)
    {
      rtx tem1 = XEXP (x, 0);
      rtx tem2 = XEXP (x, 1);

      /* Local extended canonicalization rule: the first operand must
	 be REG, unless it's an operation (MULT).  */
      if (!REG_P (tem1) && GET_CODE (tem1) != MULT)
	tem1 = tem2, tem2 = XEXP (x, 0);

      /* We'll "assume" we have canonical RTX now.  */
      gcc_assert (REG_P (tem1) || GET_CODE (tem1) == MULT);

      /* A BIAP is 2 extra bytes for the prefix insn, nothing more.  We
	 recognize the typical MULT which is always in tem1 because of
	 insn canonicalization.  */
      if ((GET_CODE (tem1) == MULT && cris_biap_index_p (tem1, false))
	  || REG_P (tem2))
	return 2 / 2;

      /* A BDAP (quick) is 2 extra bytes.  Any constant operand to the
	 PLUS is always found in tem2.  */
      if (CONST_INT_P (tem2) && INTVAL (tem2) < 128 && INTVAL (tem2) >= -128)
	return 2 / 2;

      /* A BDAP -32768 .. 32767 is like BDAP quick, but with 2 extra
	 bytes.  */
      if (satisfies_constraint_L (tem2))
	return (2 + 2) / 2;

      /* A BDAP with some other constant is 2 bytes extra.  */
      if (CRIS_CONSTANT_P (tem2))
	return (2 + 2 + 2) / 2;

      /* BDAP with something indirect should have a higher cost than
	 BIAP with register.   FIXME: Should it cost like a MEM or more?  */
      return (2 + 2 + 2) / 2;
    }

  /* What else?  Return a high cost.  It matters only for valid
     addressing modes.  */
  return 10;
}

/* Check various objections to the side-effect.  Used in the test-part
   of an anonymous insn describing an insn with a possible side-effect.
   Returns nonzero if the implied side-effect is ok.

   code     : PLUS or MULT
   ops	    : An array of rtx:es. lreg, rreg, rval,
	      The variables multop and other_op are indexes into this,
	      or -1 if they are not applicable.
   lreg     : The register that gets assigned in the side-effect.
   rreg     : One register in the side-effect expression
   rval     : The other register, or an int.
   multop   : An integer to multiply rval with.
   other_op : One of the entities of the main effect,
	      whose mode we must consider.  */

int
cris_side_effect_mode_ok (enum rtx_code code, rtx *ops,
			  int lreg, int rreg, int rval,
			  int multop, int other_op)
{
  /* Find what value to multiply with, for rx =ry + rz * n.  */
  int mult = multop < 0 ? 1 : INTVAL (ops[multop]);

  rtx reg_rtx = ops[rreg];
  rtx val_rtx = ops[rval];

  /* The operands may be swapped.  Canonicalize them in reg_rtx and
     val_rtx, where reg_rtx always is a reg (for this constraint to
     match).  */
  if (! cris_base_p (reg_rtx, reload_in_progress || reload_completed))
    reg_rtx = val_rtx, val_rtx = ops[rreg];

  /* Don't forget to check that reg_rtx really is a reg.  If it isn't,
     we have no business.  */
  if (! cris_base_p (reg_rtx, reload_in_progress || reload_completed))
    return 0;

  /* Don't do this when -mno-split.  */
  if (!TARGET_SIDE_EFFECT_PREFIXES)
    return 0;

  /* The mult expression may be hidden in lreg.  FIXME:  Add more
     commentary about that.  */
  if (GET_CODE (val_rtx) == MULT)
    {
      mult = INTVAL (XEXP (val_rtx, 1));
      val_rtx = XEXP (val_rtx, 0);
      code = MULT;
    }

  /* First check the "other operand".  */
  if (other_op >= 0)
    {
      if (GET_MODE_SIZE (GET_MODE (ops[other_op])) > UNITS_PER_WORD)
	return 0;

      /* Check if the lvalue register is the same as the "other
	 operand".  If so, the result is undefined and we shouldn't do
	 this.  FIXME:  Check again.  */
      if ((cris_base_p (ops[lreg], reload_in_progress || reload_completed)
	   && cris_base_p (ops[other_op],
			   reload_in_progress || reload_completed)
	   && REGNO (ops[lreg]) == REGNO (ops[other_op]))
	  || rtx_equal_p (ops[other_op], ops[lreg]))
      return 0;
    }

  /* Do not accept frame_pointer_rtx as any operand.  */
  if (ops[lreg] == frame_pointer_rtx || ops[rreg] == frame_pointer_rtx
      || ops[rval] == frame_pointer_rtx
      || (other_op >= 0 && ops[other_op] == frame_pointer_rtx))
    return 0;

  if (code == PLUS
      && ! cris_base_p (val_rtx, reload_in_progress || reload_completed))
    {

      /* Do not allow rx = rx + n if a normal add or sub with same size
	 would do.  */
      if (rtx_equal_p (ops[lreg], reg_rtx)
	  && CONST_INT_P (val_rtx)
	  && (INTVAL (val_rtx) <= 63 && INTVAL (val_rtx) >= -63))
	return 0;

      /* Check allowed cases, like [r(+)?].[bwd] and const.  */
      if (CRIS_CONSTANT_P (val_rtx))
	return 1;

      if (MEM_P (val_rtx)
	  && cris_base_or_autoincr_p (XEXP (val_rtx, 0),
				      reload_in_progress || reload_completed))
	return 1;

      if (GET_CODE (val_rtx) == SIGN_EXTEND
	  && MEM_P (XEXP (val_rtx, 0))
	  && cris_base_or_autoincr_p (XEXP (XEXP (val_rtx, 0), 0),
				      reload_in_progress || reload_completed))
	return 1;

      /* If we got here, it's not a valid addressing mode.  */
      return 0;
    }
  else if (code == MULT
	   || (code == PLUS
	       && cris_base_p (val_rtx,
			       reload_in_progress || reload_completed)))
    {
      /* Do not allow rx = rx + ry.S, since it doesn't give better code.  */
      if (rtx_equal_p (ops[lreg], reg_rtx)
	  || (mult == 1 && rtx_equal_p (ops[lreg], val_rtx)))
	return 0;

      /* Do not allow bad multiply-values.  */
      if (mult != 1 && mult != 2 && mult != 4)
	return 0;

      /* Only allow  r + ...  */
      if (! cris_base_p (reg_rtx, reload_in_progress || reload_completed))
	return 0;

      /* If we got here, all seems ok.
	 (All checks need to be done above).  */
      return 1;
    }

  /* If we get here, the caller got its initial tests wrong.  */
  internal_error ("internal error: cris_side_effect_mode_ok with bad operands");
}

/* Whether next_cc0_user of insn is LE or GT or requires a real compare
   insn for other reasons.  */

bool
cris_cc0_user_requires_cmp (rtx_insn *insn)
{
  rtx_insn *cc0_user = NULL;
  rtx body;
  rtx set;

  gcc_assert (insn != NULL);

  if (!TARGET_V32)
    return false;

  cc0_user = next_cc0_user (insn);
  if (cc0_user == NULL)
    return false;

  body = PATTERN (cc0_user);
  set = single_set (cc0_user);

  /* Users can be sCC and bCC.  */
  if (JUMP_P (cc0_user)
      && GET_CODE (body) == SET
      && SET_DEST (body) == pc_rtx
      && GET_CODE (SET_SRC (body)) == IF_THEN_ELSE
      && XEXP (XEXP (SET_SRC (body), 0), 0) == cc0_rtx)
    {
      return
	GET_CODE (XEXP (SET_SRC (body), 0)) == GT
	|| GET_CODE (XEXP (SET_SRC (body), 0)) == LE;
    }
  else if (set)
    {
      return
	GET_CODE (SET_SRC (body)) == GT
	|| GET_CODE (SET_SRC (body)) == LE;
    }

  gcc_unreachable ();
}

/* The function reg_overlap_mentioned_p in CVS (still as of 2001-05-16)
   does not handle the case where the IN operand is strict_low_part; it
   does handle it for X.  Test-case in Axis-20010516.  This function takes
   care of that for THIS port.  FIXME: strict_low_part is going away
   anyway.  */

static int
cris_reg_overlap_mentioned_p (rtx x, rtx in)
{
  /* The function reg_overlap_mentioned now handles when X is
     strict_low_part, but not when IN is a STRICT_LOW_PART.  */
  if (GET_CODE (in) == STRICT_LOW_PART)
    in = XEXP (in, 0);

  return reg_overlap_mentioned_p (x, in);
}

/* Return TRUE iff X is a CONST valid for e.g. indexing.
   ANY_OPERAND is 0 if X is in a CALL_P insn or movsi, 1
   elsewhere.  */

bool
cris_valid_pic_const (const_rtx x, bool any_operand)
{
  gcc_assert (flag_pic);

  switch (GET_CODE (x))
    {
    case CONST_INT:
    case CONST_DOUBLE:
      return true;
    default:
      ;
    }

  if (GET_CODE (x) != CONST)
    return false;

  x = XEXP (x, 0);

  /* Handle (const (plus (unspec .. UNSPEC_GOTREL) (const_int ...))).  */
  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 0)) == UNSPEC
      && (XINT (XEXP (x, 0), 1) == CRIS_UNSPEC_GOTREL
	  || XINT (XEXP (x, 0), 1) == CRIS_UNSPEC_PCREL)
      && CONST_INT_P (XEXP (x, 1)))
    x = XEXP (x, 0);

  if (GET_CODE (x) == UNSPEC)
    switch (XINT (x, 1))
      {
	/* A PCREL operand is only valid for call and movsi.  */
      case CRIS_UNSPEC_PLT_PCREL:
      case CRIS_UNSPEC_PCREL:
	return !any_operand;

      case CRIS_UNSPEC_PLT_GOTREL:
      case CRIS_UNSPEC_PLTGOTREAD:
      case CRIS_UNSPEC_GOTREAD:
      case CRIS_UNSPEC_GOTREL:
	return true;
      default:
	gcc_unreachable ();
      }

  return cris_symbol_type_of (x) == cris_no_symbol;
}

/* Helper function to find the right symbol-type to generate,
   given the original (non-PIC) representation.  */

enum cris_symbol_type
cris_symbol_type_of (const_rtx x)
{
  switch (GET_CODE (x))
    {
    case SYMBOL_REF:
      return flag_pic
	? (SYMBOL_REF_LOCAL_P (x)
	   ? cris_rel_symbol : cris_got_symbol)
	: cris_offsettable_symbol;

    case LABEL_REF:
      return flag_pic ? cris_rel_symbol : cris_offsettable_symbol;

    case CONST:
      return cris_symbol_type_of (XEXP (x, 0));

    case PLUS:
    case MINUS:
      {
	enum cris_symbol_type t1 = cris_symbol_type_of (XEXP (x, 0));
	enum cris_symbol_type t2 = cris_symbol_type_of (XEXP (x, 1));

	gcc_assert (t1 == cris_no_symbol || t2 == cris_no_symbol);

	if (t1 == cris_got_symbol || t2 == cris_got_symbol)
	  return cris_got_symbol_needing_fixup;

	return t1 != cris_no_symbol ? t1 : t2;
      }

    case CONST_INT:
    case CONST_DOUBLE:
      return cris_no_symbol;

    case UNSPEC:
      return cris_unspec;

    default:
      fatal_insn ("unrecognized supposed constant", x);
    }

  gcc_unreachable ();
}

/* The LEGITIMATE_PIC_OPERAND_P worker.  */

int
cris_legitimate_pic_operand (rtx x)
{
  /* Symbols are not valid PIC operands as-is; just constants.  */
  return cris_valid_pic_const (x, true);
}

/* Queue an .ident string in the queue of top-level asm statements.
   If the front-end is done, we must be being called from toplev.c.
   In that case, do nothing.  */
void 
cris_asm_output_ident (const char *string)
{
  if (symtab->state != PARSING)
    return;

  default_asm_output_ident_directive (string);
}

/* The ASM_OUTPUT_CASE_END worker.  */

void
cris_asm_output_case_end (FILE *stream, int num, rtx_insn *table)
{
  /* Step back, over the label for the table, to the actual casejump and
     assert that we find only what's expected.  */
  rtx_insn *whole_jump_insn = prev_nonnote_nondebug_insn (table);
  gcc_assert (whole_jump_insn != NULL_RTX && LABEL_P (whole_jump_insn));
  whole_jump_insn = prev_nonnote_nondebug_insn (whole_jump_insn);
  gcc_assert (whole_jump_insn != NULL_RTX
	      && (JUMP_P (whole_jump_insn)
		  || (TARGET_V32 && INSN_P (whole_jump_insn)
		      && GET_CODE (PATTERN (whole_jump_insn)) == SEQUENCE)));
  /* Get the pattern of the casejump, so we can extract the default label.  */
  rtx whole_jump_pat = PATTERN (whole_jump_insn);

  if (TARGET_V32)
    {
      /* This can be a SEQUENCE, meaning the delay-slot of the jump is
	 filled.  We also output the offset word a little differently.  */
      rtx parallel_jump
	= (GET_CODE (whole_jump_pat) == SEQUENCE
	   ? PATTERN (XVECEXP (whole_jump_pat, 0, 0)) : whole_jump_pat);

      asm_fprintf (stream,
		   "\t.word %LL%d-.%s\n",
		   CODE_LABEL_NUMBER (XEXP (XEXP (XEXP (XVECEXP
							(parallel_jump, 0, 0),
							1), 2), 0)),
		   (TARGET_PDEBUG ? "; default" : ""));
      return;
    }

  asm_fprintf (stream,
	       "\t.word %LL%d-%LL%d%s\n",
	       CODE_LABEL_NUMBER (XEXP
				  (XEXP
				   (XEXP (XVECEXP (whole_jump_pat, 0, 0), 1),
				    2), 0)),
	       num,
	       (TARGET_PDEBUG ? "; default" : ""));
}

/* The TARGET_OPTION_OVERRIDE worker.
   As is the norm, this also parses -mfoo=bar type parameters.  */

static void
cris_option_override (void)
{
  if (cris_max_stackframe_str)
    {
      cris_max_stackframe = atoi (cris_max_stackframe_str);

      /* Do some sanity checking.  */
      if (cris_max_stackframe < 0 || cris_max_stackframe > 0x20000000)
	internal_error ("-max-stackframe=%d is not usable, not between 0 and %d",
			cris_max_stackframe, 0x20000000);
    }

  /* Let "-metrax4" and "-metrax100" change the cpu version.  */
  if (TARGET_SVINTO && cris_cpu_version < CRIS_CPU_SVINTO)
    cris_cpu_version = CRIS_CPU_SVINTO;
  else if (TARGET_ETRAX4_ADD && cris_cpu_version < CRIS_CPU_ETRAX4)
    cris_cpu_version = CRIS_CPU_ETRAX4;

  /* Parse -march=... and its synonym, the deprecated -mcpu=...  */
  if (cris_cpu_str)
    {
      cris_cpu_version
	= (*cris_cpu_str == 'v' ? atoi (cris_cpu_str + 1) : -1);

      if (strcmp ("etrax4", cris_cpu_str) == 0)
	cris_cpu_version = 3;

      if (strcmp ("svinto", cris_cpu_str) == 0
	  || strcmp ("etrax100", cris_cpu_str) == 0)
	cris_cpu_version = 8;

      if (strcmp ("ng", cris_cpu_str) == 0
	  || strcmp ("etrax100lx", cris_cpu_str) == 0)
	cris_cpu_version = 10;

      if (cris_cpu_version < 0 || cris_cpu_version > 32)
	error ("unknown CRIS version specification in -march= or -mcpu= : %s",
	       cris_cpu_str);

      /* Set the target flags.  */
      if (cris_cpu_version >= CRIS_CPU_ETRAX4)
	target_flags |= MASK_ETRAX4_ADD;

      /* If this is Svinto or higher, align for 32 bit accesses.  */
      if (cris_cpu_version >= CRIS_CPU_SVINTO)
	target_flags
	  |= (MASK_SVINTO | MASK_ALIGN_BY_32
	      | MASK_STACK_ALIGN | MASK_CONST_ALIGN
	      | MASK_DATA_ALIGN);

      /* Note that we do not add new flags when it can be completely
	 described with a macro that uses -mcpu=X.  So
	 TARGET_HAS_MUL_INSNS is (cris_cpu_version >= CRIS_CPU_NG).  */
    }

  if (cris_tune_str)
    {
      int cris_tune
	= (*cris_tune_str == 'v' ? atoi (cris_tune_str + 1) : -1);

      if (strcmp ("etrax4", cris_tune_str) == 0)
	cris_tune = 3;

      if (strcmp ("svinto", cris_tune_str) == 0
	  || strcmp ("etrax100", cris_tune_str) == 0)
	cris_tune = 8;

      if (strcmp ("ng", cris_tune_str) == 0
	  || strcmp ("etrax100lx", cris_tune_str) == 0)
	cris_tune = 10;

      if (cris_tune < 0 || cris_tune > 32)
	error ("unknown CRIS cpu version specification in -mtune= : %s",
	       cris_tune_str);

      if (cris_tune >= CRIS_CPU_SVINTO)
	/* We have currently nothing more to tune than alignment for
	   memory accesses.  */
	target_flags
	  |= (MASK_STACK_ALIGN | MASK_CONST_ALIGN
	      | MASK_DATA_ALIGN | MASK_ALIGN_BY_32);
    }

  if (cris_cpu_version >= CRIS_CPU_V32)
    target_flags &= ~(MASK_SIDE_EFFECT_PREFIXES|MASK_MUL_BUG);

  if (flag_pic)
    {
      /* Use error rather than warning, so invalid use is easily
	 detectable.  Still change to the values we expect, to avoid
	 further errors.  */
      if (! TARGET_LINUX)
	{
	  error ("-fPIC and -fpic are not supported in this configuration");
	  flag_pic = 0;
	}

      /* Turn off function CSE.  We need to have the addresses reach the
	 call expanders to get PLT-marked, as they could otherwise be
	 compared against zero directly or indirectly.  After visiting the
	 call expanders they will then be cse:ed, as the call expanders
	 force_reg the addresses, effectively forcing flag_no_function_cse
	 to 0.  */
      flag_no_function_cse = 1;
    }

  /* Set the per-function-data initializer.  */
  init_machine_status = cris_init_machine_status;
}

/* The TARGET_ASM_OUTPUT_MI_THUNK worker.  */

static void
cris_asm_output_mi_thunk (FILE *stream,
			  tree thunkdecl ATTRIBUTE_UNUSED,
			  HOST_WIDE_INT delta,
			  HOST_WIDE_INT vcall_offset ATTRIBUTE_UNUSED,
			  tree funcdecl)
{
  /* Make sure unwind info is emitted for the thunk if needed.  */
  final_start_function (emit_barrier (), stream, 1);

  if (delta > 0)
    fprintf (stream, "\tadd%s " HOST_WIDE_INT_PRINT_DEC ",$%s\n",
	     ADDITIVE_SIZE_MODIFIER (delta), delta,
	     reg_names[CRIS_FIRST_ARG_REG]);
  else if (delta < 0)
    fprintf (stream, "\tsub%s " HOST_WIDE_INT_PRINT_DEC ",$%s\n",
	     ADDITIVE_SIZE_MODIFIER (-delta), -delta,
	     reg_names[CRIS_FIRST_ARG_REG]);

  if (flag_pic)
    {
      const char *name = XSTR (XEXP (DECL_RTL (funcdecl), 0), 0);

      name = (* targetm.strip_name_encoding) (name);

      if (TARGET_V32)
	{
	  fprintf (stream, "\tba ");
	  assemble_name (stream, name);
	  fprintf (stream, "%s\n\tnop\n", CRIS_PLT_PCOFFSET_SUFFIX);
	}
      else
	{
	  fprintf (stream, "\tadd.d ");
	  assemble_name (stream, name);
	  fprintf (stream, "%s,$pc\n", CRIS_PLT_PCOFFSET_SUFFIX);
	}
    }
  else
    {
      fprintf (stream, "\tjump ");
      assemble_name (stream, XSTR (XEXP (DECL_RTL (funcdecl), 0), 0));
      fprintf (stream, "\n");

      if (TARGET_V32)
	fprintf (stream, "\tnop\n");
    }

  final_end_function ();
}

/* Boilerplate emitted at start of file.

   NO_APP *only at file start* means faster assembly.  It also means
   comments are not allowed.  In some cases comments will be output
   for debugging purposes.  Make sure they are allowed then.  */
static void
cris_file_start (void)
{
  /* These expressions can vary at run time, so we cannot put
     them into TARGET_INITIALIZER.  */
  targetm.asm_file_start_app_off = !(TARGET_PDEBUG || flag_print_asm_name);

  default_file_start ();
}

/* Output that goes at the end of the file, similarly.  */

static void
cris_file_end (void)
{
  /* For CRIS, the default is to assume *no* executable stack, so output
     an executable-stack-note only when needed.  */
  if (TARGET_LINUX && trampolines_created)
    file_end_indicate_exec_stack ();
}

/* Rename the function calls for integer multiply and divide.  */
static void
cris_init_libfuncs (void)
{
  set_optab_libfunc (smul_optab, SImode, "__Mul");
  set_optab_libfunc (sdiv_optab, SImode, "__Div");
  set_optab_libfunc (udiv_optab, SImode, "__Udiv");
  set_optab_libfunc (smod_optab, SImode, "__Mod");
  set_optab_libfunc (umod_optab, SImode, "__Umod");

  /* Atomic data being unaligned is unfortunately a reality.
     Deal with it.  */
  if (TARGET_ATOMICS_MAY_CALL_LIBFUNCS)
    {
      set_optab_libfunc (sync_compare_and_swap_optab, SImode,
			 "__cris_atcmpxchgr32");
      set_optab_libfunc (sync_compare_and_swap_optab, HImode,
			 "__cris_atcmpxchgr16");
    }
}

/* The INIT_EXPANDERS worker sets the per-function-data initializer and
   mark functions.  */

void
cris_init_expanders (void)
{
  /* Nothing here at the moment.  */
}

/* Zero initialization is OK for all current fields.  */

static struct machine_function *
cris_init_machine_status (void)
{
  return ggc_cleared_alloc<machine_function> ();
}

/* Split a 2 word move (DI or presumably DF) into component parts.
   Originally a copy of gen_split_move_double in m32r.c.  */

rtx
cris_split_movdx (rtx *operands)
{
  machine_mode mode = GET_MODE (operands[0]);
  rtx dest = operands[0];
  rtx src  = operands[1];
  rtx val;

  /* We used to have to handle (SUBREG (MEM)) here, but that should no
     longer happen; after reload there are no SUBREGs any more, and we're
     only called after reload.  */
  CRIS_ASSERT (GET_CODE (dest) != SUBREG && GET_CODE (src) != SUBREG);

  start_sequence ();
  if (REG_P (dest))
    {
      int dregno = REGNO (dest);

      /* Reg-to-reg copy.  */
      if (REG_P (src))
	{
	  int sregno = REGNO (src);

	  int reverse = (dregno == sregno + 1);

	  /* We normally copy the low-numbered register first.  However, if
	     the first register operand 0 is the same as the second register of
	     operand 1, we must copy in the opposite order.  */
	  emit_insn (gen_rtx_SET (operand_subword (dest, reverse, TRUE, mode),
				  operand_subword (src, reverse, TRUE, mode)));

	  emit_insn (gen_rtx_SET (operand_subword (dest, !reverse, TRUE, mode),
				  operand_subword (src, !reverse, TRUE, mode)));
	}
      /* Constant-to-reg copy.  */
      else if (CONST_INT_P (src) || GET_CODE (src) == CONST_DOUBLE)
	{
	  rtx words[2];
	  split_double (src, &words[0], &words[1]);
	  emit_insn (gen_rtx_SET (operand_subword (dest, 0, TRUE, mode),
				  words[0]));

	  emit_insn (gen_rtx_SET (operand_subword (dest, 1, TRUE, mode),
				  words[1]));
	}
      /* Mem-to-reg copy.  */
      else if (MEM_P (src))
	{
	  /* If the high-address word is used in the address, we must load it
	     last.  Otherwise, load it first.  */
	  rtx addr = XEXP (src, 0);
	  int reverse = (refers_to_regno_p (dregno, addr) != 0);

	  /* The original code implies that we can't do
	     move.x [rN+],rM  move.x [rN],rM+1
	     when rN is dead, because of REG_NOTES damage.  That is
	     consistent with what I've seen, so don't try it.

             We have two different cases here; if the addr is POST_INC,
             just pass it through, otherwise add constants.  */

          if (GET_CODE (addr) == POST_INC)
	    {
	      rtx mem;
	      rtx insn;

	      /* Whenever we emit insns with post-incremented
		 addresses ourselves, we must add a post-inc note
		 manually.  */
	      mem = change_address (src, SImode, addr);
	      insn
		= gen_rtx_SET (operand_subword (dest, 0, TRUE, mode), mem);
	      insn = emit_insn (insn);
	      if (GET_CODE (XEXP (mem, 0)) == POST_INC)
		REG_NOTES (insn)
		  = alloc_EXPR_LIST (REG_INC, XEXP (XEXP (mem, 0), 0),
				     REG_NOTES (insn));

	      mem = copy_rtx (mem);
	      insn
		= gen_rtx_SET (operand_subword (dest, 1, TRUE, mode), mem);
	      insn = emit_insn (insn);
	      if (GET_CODE (XEXP (mem, 0)) == POST_INC)
		REG_NOTES (insn)
		  = alloc_EXPR_LIST (REG_INC, XEXP (XEXP (mem, 0), 0),
				     REG_NOTES (insn));
	    }
	  else
	    {
	      /* Make sure we don't get any other addresses with
		 embedded postincrements.  They should be stopped in
		 GO_IF_LEGITIMATE_ADDRESS, but we're here for your
		 safety.  */
	      if (side_effects_p (addr))
		fatal_insn ("unexpected side-effects in address", addr);

	      emit_insn (gen_rtx_SET
			 (operand_subword (dest, reverse, TRUE, mode),
			  change_address
			  (src, SImode,
			   plus_constant (Pmode, addr,
					  reverse * UNITS_PER_WORD))));
	      emit_insn (gen_rtx_SET
			 (operand_subword (dest, ! reverse, TRUE, mode),
			  change_address
			  (src, SImode,
			   plus_constant (Pmode, addr,
					  (! reverse) *
					  UNITS_PER_WORD))));
	    }
	}
      else
	internal_error ("unknown src");
    }
  /* Reg-to-mem copy or clear mem.  */
  else if (MEM_P (dest)
	   && (REG_P (src)
	       || src == const0_rtx
	       || src == CONST0_RTX (DFmode)))
    {
      rtx addr = XEXP (dest, 0);

      if (GET_CODE (addr) == POST_INC)
	{
	  rtx mem;
	  rtx insn;

	  /* Whenever we emit insns with post-incremented addresses
	     ourselves, we must add a post-inc note manually.  */
	  mem = change_address (dest, SImode, addr);
	  insn
	    = gen_rtx_SET (mem, operand_subword (src, 0, TRUE, mode));
	  insn = emit_insn (insn);
	  if (GET_CODE (XEXP (mem, 0)) == POST_INC)
	    REG_NOTES (insn)
	      = alloc_EXPR_LIST (REG_INC, XEXP (XEXP (mem, 0), 0),
				 REG_NOTES (insn));

	  mem = copy_rtx (mem);
	  insn = gen_rtx_SET (mem, operand_subword (src, 1, TRUE, mode));
	  insn = emit_insn (insn);
	  if (GET_CODE (XEXP (mem, 0)) == POST_INC)
	    REG_NOTES (insn)
	      = alloc_EXPR_LIST (REG_INC, XEXP (XEXP (mem, 0), 0),
				 REG_NOTES (insn));
	}
      else
	{
	  /* Make sure we don't get any other addresses with embedded
	     postincrements.  They should be stopped in
	     GO_IF_LEGITIMATE_ADDRESS, but we're here for your safety.  */
	  if (side_effects_p (addr))
	    fatal_insn ("unexpected side-effects in address", addr);

	  emit_insn (gen_rtx_SET
		     (change_address (dest, SImode, addr),
		      operand_subword (src, 0, TRUE, mode)));

	  emit_insn (gen_rtx_SET
		     (change_address (dest, SImode,
				      plus_constant (Pmode, addr,
						     UNITS_PER_WORD)),
		      operand_subword (src, 1, TRUE, mode)));
	}
    }

  else
    internal_error ("unknown dest");

  val = get_insns ();
  end_sequence ();
  return val;
}

/* The expander for the prologue pattern name.  */

void
cris_expand_prologue (void)
{
  int regno;
  int size = get_frame_size ();
  /* Shorten the used name for readability.  */
  int cfoa_size = crtl->outgoing_args_size;
  int last_movem_reg = -1;
  int framesize = 0;
  rtx mem, insn;
  int return_address_on_stack = cris_return_address_on_stack ();
  int got_really_used = false;
  int n_movem_regs = 0;
  int pretend = crtl->args.pretend_args_size;

  /* Don't do anything if no prologues or epilogues are wanted.  */
  if (!TARGET_PROLOGUE_EPILOGUE)
    return;

  CRIS_ASSERT (size >= 0);

  if (crtl->uses_pic_offset_table)
    {
      /* A reference may have been optimized out (like the abort () in
	 fde_split in unwind-dw2-fde.c, at least 3.2.1) so check that
	 it's still used.  */
      push_topmost_sequence ();
      got_really_used
	= reg_used_between_p (pic_offset_table_rtx, get_insns (), NULL);
      pop_topmost_sequence ();
    }

  /* Align the size to what's best for the CPU model.  */
  if (TARGET_STACK_ALIGN)
    size = TARGET_ALIGN_BY_32 ? (size + 3) & ~3 : (size + 1) & ~1;

  if (pretend)
    {
      /* See also cris_setup_incoming_varargs where
	 cfun->machine->stdarg_regs is set.  There are other setters of
	 crtl->args.pretend_args_size than stdarg handling, like
	 for an argument passed with parts in R13 and stack.  We must
	 not store R13 into the pretend-area for that case, as GCC does
	 that itself.  "Our" store would be marked as redundant and GCC
	 will attempt to remove it, which will then be flagged as an
	 internal error; trying to remove a frame-related insn.  */
      int stdarg_regs = cfun->machine->stdarg_regs;

      framesize += pretend;

      for (regno = CRIS_FIRST_ARG_REG + CRIS_MAX_ARGS_IN_REGS - 1;
	   stdarg_regs > 0;
	   regno--, pretend -= 4, stdarg_regs--)
	{
	  insn = emit_insn (gen_rtx_SET (stack_pointer_rtx,
					 plus_constant (Pmode,
							stack_pointer_rtx,
							-4)));
	  /* FIXME: When dwarf2 frame output and unless asynchronous
	     exceptions, make dwarf2 bundle together all stack
	     adjustments like it does for registers between stack
	     adjustments.  */
	  RTX_FRAME_RELATED_P (insn) = 1;

	  mem = gen_rtx_MEM (SImode, stack_pointer_rtx);
	  set_mem_alias_set (mem, get_varargs_alias_set ());
	  insn = emit_move_insn (mem, gen_raw_REG (SImode, regno));

	  /* Note the absence of RTX_FRAME_RELATED_P on the above insn:
	     the value isn't restored, so we don't want to tell dwarf2
	     that it's been stored to stack, else EH handling info would
	     get confused.  */
	}

      /* For other setters of crtl->args.pretend_args_size, we
	 just adjust the stack by leaving the remaining size in
	 "pretend", handled below.  */
    }

  /* Save SRP if not a leaf function.  */
  if (return_address_on_stack)
    {
      insn = emit_insn (gen_rtx_SET (stack_pointer_rtx,
				     plus_constant (Pmode, stack_pointer_rtx,
						    -4 - pretend)));
      pretend = 0;
      RTX_FRAME_RELATED_P (insn) = 1;

      mem = gen_rtx_MEM (SImode, stack_pointer_rtx);
      set_mem_alias_set (mem, get_frame_alias_set ());
      insn = emit_move_insn (mem, gen_raw_REG (SImode, CRIS_SRP_REGNUM));
      RTX_FRAME_RELATED_P (insn) = 1;
      framesize += 4;
    }

  /* Set up the frame pointer, if needed.  */
  if (frame_pointer_needed)
    {
      insn = emit_insn (gen_rtx_SET (stack_pointer_rtx,
				     plus_constant (Pmode, stack_pointer_rtx,
						    -4 - pretend)));
      pretend = 0;
      RTX_FRAME_RELATED_P (insn) = 1;

      mem = gen_rtx_MEM (SImode, stack_pointer_rtx);
      set_mem_alias_set (mem, get_frame_alias_set ());
      insn = emit_move_insn (mem, frame_pointer_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;

      insn = emit_move_insn (frame_pointer_rtx, stack_pointer_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;

      framesize += 4;
    }

  /* Between frame-pointer and saved registers lie the area for local
     variables.  If we get here with "pretended" size remaining, count
     it into the general stack size.  */
  size += pretend;

  /* Get a contiguous sequence of registers, starting with R0, that need
     to be saved.  */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    {
      if (cris_reg_saved_in_regsave_area (regno, got_really_used))
	{
	  n_movem_regs++;

	  /* Check if movem may be used for registers so far.  */
	  if (regno == last_movem_reg + 1)
	    /* Yes, update next expected register.  */
	    last_movem_reg = regno;
	  else
	    {
	      /* We cannot use movem for all registers.  We have to flush
		 any movem:ed registers we got so far.  */
	      if (last_movem_reg != -1)
		{
		  int n_saved
		    = (n_movem_regs == 1) ? 1 : last_movem_reg + 1;

		  /* It is a win to use a side-effect assignment for
		     64 <= size <= 128.  But side-effect on movem was
		     not usable for CRIS v0..3.  Also only do it if
		     side-effects insns are allowed.  */
		  if ((last_movem_reg + 1) * 4 + size >= 64
		      && (last_movem_reg + 1) * 4 + size <= 128
		      && (cris_cpu_version >= CRIS_CPU_SVINTO || n_saved == 1)
		      && TARGET_SIDE_EFFECT_PREFIXES)
		    {
		      mem
			= gen_rtx_MEM (SImode,
				       plus_constant (Pmode, stack_pointer_rtx,
						      -(n_saved * 4 + size)));
		      set_mem_alias_set (mem, get_frame_alias_set ());
		      insn
			= cris_emit_movem_store (mem, GEN_INT (n_saved),
						 -(n_saved * 4 + size),
						 true);
		    }
		  else
		    {
		      insn
			= gen_rtx_SET (stack_pointer_rtx,
				       plus_constant (Pmode, stack_pointer_rtx,
						      -(n_saved * 4 + size)));
		      insn = emit_insn (insn);
		      RTX_FRAME_RELATED_P (insn) = 1;

		      mem = gen_rtx_MEM (SImode, stack_pointer_rtx);
		      set_mem_alias_set (mem, get_frame_alias_set ());
		      insn = cris_emit_movem_store (mem, GEN_INT (n_saved),
						    0, true);
		    }

		  framesize += n_saved * 4 + size;
		  last_movem_reg = -1;
		  size = 0;
		}

	      insn = emit_insn (gen_rtx_SET (stack_pointer_rtx,
					     plus_constant (Pmode,
							    stack_pointer_rtx,
							    -4 - size)));
	      RTX_FRAME_RELATED_P (insn) = 1;

	      mem = gen_rtx_MEM (SImode, stack_pointer_rtx);
	      set_mem_alias_set (mem, get_frame_alias_set ());
	      insn = emit_move_insn (mem, gen_raw_REG (SImode, regno));
	      RTX_FRAME_RELATED_P (insn) = 1;

	      framesize += 4 + size;
	      size = 0;
	    }
	}
    }

  /* Check after, if we could movem all registers.  This is the normal case.  */
  if (last_movem_reg != -1)
    {
      int n_saved
	= (n_movem_regs == 1) ? 1 : last_movem_reg + 1;

      /* Side-effect on movem was not usable for CRIS v0..3.  Also only
	 do it if side-effects insns are allowed.  */
      if ((last_movem_reg + 1) * 4 + size >= 64
	  && (last_movem_reg + 1) * 4 + size <= 128
	  && (cris_cpu_version >= CRIS_CPU_SVINTO || n_saved == 1)
	  && TARGET_SIDE_EFFECT_PREFIXES)
	{
	  mem
	    = gen_rtx_MEM (SImode,
			   plus_constant (Pmode, stack_pointer_rtx,
					  -(n_saved * 4 + size)));
	  set_mem_alias_set (mem, get_frame_alias_set ());
	  insn = cris_emit_movem_store (mem, GEN_INT (n_saved),
					-(n_saved * 4 + size), true);
	}
      else
	{
	  insn
	    = gen_rtx_SET (stack_pointer_rtx,
			   plus_constant (Pmode, stack_pointer_rtx,
					  -(n_saved * 4 + size)));
	  insn = emit_insn (insn);
	  RTX_FRAME_RELATED_P (insn) = 1;

	  mem = gen_rtx_MEM (SImode, stack_pointer_rtx);
	  set_mem_alias_set (mem, get_frame_alias_set ());
	  insn = cris_emit_movem_store (mem, GEN_INT (n_saved), 0, true);
	}

      framesize += n_saved * 4 + size;
      /* We have to put outgoing argument space after regs.  */
      if (cfoa_size)
	{
	  insn = emit_insn (gen_rtx_SET (stack_pointer_rtx,
					 plus_constant (Pmode,
							stack_pointer_rtx,
							-cfoa_size)));
	  RTX_FRAME_RELATED_P (insn) = 1;
	  framesize += cfoa_size;
	}
    }
  else if ((size + cfoa_size) > 0)
    {
      insn = emit_insn (gen_rtx_SET (stack_pointer_rtx,
				     plus_constant (Pmode,
						    stack_pointer_rtx,
						    -(cfoa_size + size))));
      RTX_FRAME_RELATED_P (insn) = 1;
      framesize += size + cfoa_size;
    }

  /* Set up the PIC register, if it is used.  */
  if (got_really_used)
    {
      rtx got
	= gen_rtx_UNSPEC (SImode, gen_rtvec (1, const0_rtx), CRIS_UNSPEC_GOT);
      emit_move_insn (pic_offset_table_rtx, got);

      /* FIXME: This is a cover-up for flow2 messing up; it doesn't
	 follow exceptional paths and tries to delete the GOT load as
	 unused, if it isn't used on the non-exceptional paths.  Other
	 ports have similar or other cover-ups, or plain bugs marking
	 the GOT register load as maybe-dead.  To see this, remove the
	 line below and try libsupc++/vec.cc or a trivial
	 "static void y (); void x () {try {y ();} catch (...) {}}".  */
      emit_use (pic_offset_table_rtx);
    }

  if (cris_max_stackframe && framesize > cris_max_stackframe)
    warning (0, "stackframe too big: %d bytes", framesize);
}

/* The expander for the epilogue pattern.  */

void
cris_expand_epilogue (void)
{
  int regno;
  int size = get_frame_size ();
  int last_movem_reg = -1;
  int argspace_offset = crtl->outgoing_args_size;
  int pretend =	 crtl->args.pretend_args_size;
  rtx mem;
  bool return_address_on_stack = cris_return_address_on_stack ();
  /* A reference may have been optimized out
     (like the abort () in fde_split in unwind-dw2-fde.c, at least 3.2.1)
     so check that it's still used.  */
  int got_really_used = false;
  int n_movem_regs = 0;

  if (!TARGET_PROLOGUE_EPILOGUE)
    return;

  if (crtl->uses_pic_offset_table)
    {
      /* A reference may have been optimized out (like the abort () in
	 fde_split in unwind-dw2-fde.c, at least 3.2.1) so check that
	 it's still used.  */
      push_topmost_sequence ();
      got_really_used
	= reg_used_between_p (pic_offset_table_rtx, get_insns (), NULL);
      pop_topmost_sequence ();
    }

  /* Align byte count of stack frame.  */
  if (TARGET_STACK_ALIGN)
    size = TARGET_ALIGN_BY_32 ? (size + 3) & ~3 : (size + 1) & ~1;

  /* Check how many saved regs we can movem.  They start at r0 and must
     be contiguous.  */
  for (regno = 0;
       regno < FIRST_PSEUDO_REGISTER;
       regno++)
    if (cris_reg_saved_in_regsave_area (regno, got_really_used))
      {
	n_movem_regs++;

	if (regno == last_movem_reg + 1)
	  last_movem_reg = regno;
	else
	  break;
      }

  /* If there was only one register that really needed to be saved
     through movem, don't use movem.  */
  if (n_movem_regs == 1)
    last_movem_reg = -1;

  /* Now emit "normal" move insns for all regs higher than the movem
     regs.  */
  for (regno = FIRST_PSEUDO_REGISTER - 1;
       regno > last_movem_reg;
       regno--)
    if (cris_reg_saved_in_regsave_area (regno, got_really_used))
      {
	rtx insn;

	if (argspace_offset)
	  {
	    /* There is an area for outgoing parameters located before
	       the saved registers.  We have to adjust for that.  */
	    emit_insn (gen_rtx_SET (stack_pointer_rtx,
				    plus_constant (Pmode, stack_pointer_rtx,
						   argspace_offset)));
	    /* Make sure we only do this once.  */
	    argspace_offset = 0;
	  }

	mem = gen_rtx_MEM (SImode, gen_rtx_POST_INC (SImode,
						     stack_pointer_rtx));
	set_mem_alias_set (mem, get_frame_alias_set ());
	insn = emit_move_insn (gen_raw_REG (SImode, regno), mem);

	/* Whenever we emit insns with post-incremented addresses
	   ourselves, we must add a post-inc note manually.  */
	REG_NOTES (insn)
	  = alloc_EXPR_LIST (REG_INC, stack_pointer_rtx, REG_NOTES (insn));
      }

  /* If we have any movem-restore, do it now.  */
  if (last_movem_reg != -1)
    {
      rtx insn;

      if (argspace_offset)
	{
	  emit_insn (gen_rtx_SET (stack_pointer_rtx,
				  plus_constant (Pmode, stack_pointer_rtx,
						 argspace_offset)));
	  argspace_offset = 0;
	}

      mem = gen_rtx_MEM (SImode,
			 gen_rtx_POST_INC (SImode, stack_pointer_rtx));
      set_mem_alias_set (mem, get_frame_alias_set ());
      insn
	= emit_insn (cris_gen_movem_load (mem,
					  GEN_INT (last_movem_reg + 1), 0));
      /* Whenever we emit insns with post-incremented addresses
	 ourselves, we must add a post-inc note manually.  */
      if (side_effects_p (PATTERN (insn)))
	REG_NOTES (insn)
	  = alloc_EXPR_LIST (REG_INC, stack_pointer_rtx, REG_NOTES (insn));
    }

  /* If we don't clobber all of the allocated stack area (we've already
     deallocated saved registers), GCC might want to schedule loads from
     the stack to *after* the stack-pointer restore, which introduces an
     interrupt race condition.  This happened for the initial-value
     SRP-restore for g++.dg/eh/registers1.C (noticed by inspection of
     other failure for that test).  It also happened for the stack slot
     for the return value in (one version of)
     linux/fs/dcache.c:__d_lookup, at least with "-O2
     -fno-omit-frame-pointer".  */

  /* Restore frame pointer if necessary.  */
  if (frame_pointer_needed)
    {
      rtx insn;

      emit_insn (gen_cris_frame_deallocated_barrier ());

      emit_move_insn (stack_pointer_rtx, frame_pointer_rtx);
      mem = gen_rtx_MEM (SImode, gen_rtx_POST_INC (SImode,
						   stack_pointer_rtx));
      set_mem_alias_set (mem, get_frame_alias_set ());
      insn = emit_move_insn (frame_pointer_rtx, mem);

      /* Whenever we emit insns with post-incremented addresses
	 ourselves, we must add a post-inc note manually.  */
      REG_NOTES (insn)
	= alloc_EXPR_LIST (REG_INC, stack_pointer_rtx, REG_NOTES (insn));
    }
  else if ((size + argspace_offset) != 0)
    {
      emit_insn (gen_cris_frame_deallocated_barrier ());

      /* If there was no frame-pointer to restore sp from, we must
	 explicitly deallocate local variables.  */

      /* Handle space for outgoing parameters that hasn't been handled
	 yet.  */
      size += argspace_offset;

      emit_insn (gen_rtx_SET (stack_pointer_rtx,
			      plus_constant (Pmode, stack_pointer_rtx, size)));
    }

  /* If this function has no pushed register parameters
     (stdargs/varargs), and if it is not a leaf function, then we have
     the return address on the stack.  */
  if (return_address_on_stack && pretend == 0)
    {
      if (TARGET_V32 || crtl->calls_eh_return)
	{
	  rtx mem;
	  rtx insn;
	  rtx srpreg = gen_raw_REG (SImode, CRIS_SRP_REGNUM);
	  mem = gen_rtx_MEM (SImode,
			     gen_rtx_POST_INC (SImode,
					       stack_pointer_rtx));
	  set_mem_alias_set (mem, get_frame_alias_set ());
	  insn = emit_move_insn (srpreg, mem);

	  /* Whenever we emit insns with post-incremented addresses
	     ourselves, we must add a post-inc note manually.  */
	  REG_NOTES (insn)
	    = alloc_EXPR_LIST (REG_INC, stack_pointer_rtx, REG_NOTES (insn));

	  if (crtl->calls_eh_return)
	    emit_insn (gen_addsi3 (stack_pointer_rtx,
				   stack_pointer_rtx,
				   gen_raw_REG (SImode, CRIS_STACKADJ_REG)));
	  cris_expand_return (false);
	}
      else
	cris_expand_return (true);

      return;
    }

  /* If we pushed some register parameters, then adjust the stack for
     them.  */
  if (pretend != 0)
    {
      /* If SRP is stored on the way, we need to restore it first.  */
      if (return_address_on_stack)
	{
	  rtx mem;
	  rtx srpreg = gen_raw_REG (SImode, CRIS_SRP_REGNUM);
	  rtx insn;

	  mem = gen_rtx_MEM (SImode,
			     gen_rtx_POST_INC (SImode,
					       stack_pointer_rtx));
	  set_mem_alias_set (mem, get_frame_alias_set ());
	  insn = emit_move_insn (srpreg, mem);

	  /* Whenever we emit insns with post-incremented addresses
	     ourselves, we must add a post-inc note manually.  */
	  REG_NOTES (insn)
	    = alloc_EXPR_LIST (REG_INC, stack_pointer_rtx, REG_NOTES (insn));
	}

      emit_insn (gen_rtx_SET (stack_pointer_rtx,
			      plus_constant (Pmode, stack_pointer_rtx,
					     pretend)));
    }

  /* Perform the "physical" unwinding that the EH machinery calculated.  */
  if (crtl->calls_eh_return)
    emit_insn (gen_addsi3 (stack_pointer_rtx,
			   stack_pointer_rtx,
			   gen_raw_REG (SImode, CRIS_STACKADJ_REG)));
  cris_expand_return (false);
}

/* Worker function for generating movem from mem for load_multiple.  */

rtx
cris_gen_movem_load (rtx src, rtx nregs_rtx, int nprefix)
{
  int nregs = INTVAL (nregs_rtx);
  rtvec vec;
  int eltno = 1;
  int i;
  rtx srcreg = XEXP (src, 0);
  unsigned int regno = nregs - 1;
  int regno_inc = -1;

  if (TARGET_V32)
    {
      regno = 0;
      regno_inc = 1;
    }

  if (GET_CODE (srcreg) == POST_INC)
    srcreg = XEXP (srcreg, 0);

  CRIS_ASSERT (REG_P (srcreg));

  /* Don't use movem for just one insn.  The insns are equivalent except
     for the pipeline hazard (on v32); movem does not forward the loaded
     registers so there's a three cycles penalty for their use.  */
  if (nregs == 1)
    return gen_movsi (gen_rtx_REG (SImode, 0), src);

  vec = rtvec_alloc (nprefix + nregs
		     + (GET_CODE (XEXP (src, 0)) == POST_INC));

  if (GET_CODE (XEXP (src, 0)) == POST_INC)
    {
      RTVEC_ELT (vec, nprefix + 1)
	= gen_rtx_SET (srcreg, plus_constant (Pmode, srcreg, nregs * 4));
      eltno++;
    }

  src = replace_equiv_address (src, srcreg);
  RTVEC_ELT (vec, nprefix)
    = gen_rtx_SET (gen_rtx_REG (SImode, regno), src);
  regno += regno_inc;

  for (i = 1; i < nregs; i++, eltno++)
    {
      RTVEC_ELT (vec, nprefix + eltno)
	= gen_rtx_SET (gen_rtx_REG (SImode, regno),
		       adjust_address_nv (src, SImode, i * 4));
      regno += regno_inc;
    }

  return gen_rtx_PARALLEL (VOIDmode, vec);
}

/* Worker function for generating movem to mem.  If FRAME_RELATED, notes
   are added that the dwarf2 machinery understands.  */

rtx
cris_emit_movem_store (rtx dest, rtx nregs_rtx, int increment,
		       bool frame_related)
{
  int nregs = INTVAL (nregs_rtx);
  rtvec vec;
  int eltno = 1;
  int i;
  rtx insn;
  rtx destreg = XEXP (dest, 0);
  unsigned int regno = nregs - 1;
  int regno_inc = -1;

  if (TARGET_V32)
    {
      regno = 0;
      regno_inc = 1;
    }

  if (GET_CODE (destreg) == POST_INC)
    increment += nregs * 4;

  if (GET_CODE (destreg) == POST_INC || GET_CODE (destreg) == PLUS)
    destreg = XEXP (destreg, 0);

  CRIS_ASSERT (REG_P (destreg));

  /* Don't use movem for just one insn.  The insns are equivalent except
     for the pipeline hazard (on v32); movem does not forward the loaded
     registers so there's a three cycles penalty for use.  */
  if (nregs == 1)
    {
      rtx mov = gen_rtx_SET (dest, gen_rtx_REG (SImode, 0));

      if (increment == 0)
	{
	  insn = emit_insn (mov);
	  if (frame_related)
	    RTX_FRAME_RELATED_P (insn) = 1;
	  return insn;
	}

      /* If there was a request for a side-effect, create the ordinary
         parallel.  */
      vec = rtvec_alloc (2);

      RTVEC_ELT (vec, 0) = mov;
      RTVEC_ELT (vec, 1) = gen_rtx_SET (destreg, plus_constant (Pmode, destreg,
								increment));
      if (frame_related)
	{
	  RTX_FRAME_RELATED_P (mov) = 1;
	  RTX_FRAME_RELATED_P (RTVEC_ELT (vec, 1)) = 1;
	}
    }
  else
    {
      vec = rtvec_alloc (nregs + (increment != 0 ? 1 : 0));
      RTVEC_ELT (vec, 0)
	= gen_rtx_SET (replace_equiv_address (dest,
					      plus_constant (Pmode, destreg,
							     increment)),
		       gen_rtx_REG (SImode, regno));
      regno += regno_inc;

      /* The dwarf2 info wants this mark on each component in a parallel
	 that's part of the prologue (though it's optional on the first
	 component).  */
      if (frame_related)
	RTX_FRAME_RELATED_P (RTVEC_ELT (vec, 0)) = 1;

      if (increment != 0)
	{
	  RTVEC_ELT (vec, 1)
	    = gen_rtx_SET (destreg, plus_constant (Pmode, destreg,
						   increment != 0
						   ? increment : nregs * 4));
	  eltno++;

	  if (frame_related)
	    RTX_FRAME_RELATED_P (RTVEC_ELT (vec, 1)) = 1;

	  /* Don't call adjust_address_nv on a post-incremented address if
	     we can help it.  */
	  if (GET_CODE (XEXP (dest, 0)) == POST_INC)
	    dest = replace_equiv_address (dest, destreg);
	}

      for (i = 1; i < nregs; i++, eltno++)
	{
	  RTVEC_ELT (vec, eltno)
	    = gen_rtx_SET (adjust_address_nv (dest, SImode, i * 4),
			   gen_rtx_REG (SImode, regno));
	  if (frame_related)
	    RTX_FRAME_RELATED_P (RTVEC_ELT (vec, eltno)) = 1;
	  regno += regno_inc;
	}
    }

  insn = emit_insn (gen_rtx_PARALLEL (VOIDmode, vec));

  /* Because dwarf2out.c handles the insns in a parallel as a sequence,
     we need to keep the stack adjustment separate, after the
     MEM-setters.  Else the stack-adjustment in the second component of
     the parallel would be mishandled; the offsets for the SETs that
     follow it would be wrong.  We prepare for this by adding a
     REG_FRAME_RELATED_EXPR with the MEM-setting parts in a SEQUENCE
     followed by the increment.  Note that we have FRAME_RELATED_P on
     all the SETs, including the original stack adjustment SET in the
     parallel.  */
  if (frame_related)
    {
      if (increment != 0)
	{
	  rtx seq = gen_rtx_SEQUENCE (VOIDmode, rtvec_alloc (nregs + 1));
	  XVECEXP (seq, 0, 0) = copy_rtx (XVECEXP (PATTERN (insn), 0, 0));
	  for (i = 1; i < nregs; i++)
	    XVECEXP (seq, 0, i)
	      = copy_rtx (XVECEXP (PATTERN (insn), 0, i + 1));
	  XVECEXP (seq, 0, nregs) = copy_rtx (XVECEXP (PATTERN (insn), 0, 1));
	  add_reg_note (insn, REG_FRAME_RELATED_EXPR, seq);
	}

      RTX_FRAME_RELATED_P (insn) = 1;
    }

  return insn;
}

/* Worker function for expanding the address for PIC function calls.  */

void
cris_expand_pic_call_address (rtx *opp, rtx *markerp)
{
  rtx op = *opp;

  gcc_assert (flag_pic && MEM_P (op));
  op = XEXP (op, 0);

  /* It might be that code can be generated that jumps to 0 (or to a
     specific address).  Don't die on that.  (There is a
     testcase.)  */
  if (CONSTANT_P (op) && !CONST_INT_P (op))
    {
      enum cris_symbol_type t = cris_symbol_type_of (op);

      CRIS_ASSERT (can_create_pseudo_p ());

      /* For local symbols (non-PLT), just get the plain symbol
	 reference into a register.  For symbols that can be PLT, make
	 them PLT.  */
      if (t == cris_rel_symbol)
	{
	  /* For v32, we're fine as-is; just PICify the symbol.  Forcing
	     into a register caused performance regression for 3.2.1,
	     observable in __floatdidf and elsewhere in libgcc.  */
	  if (TARGET_V32)
	    {
	      rtx sym = GET_CODE (op) != CONST ? op : get_related_value (op);
	      HOST_WIDE_INT offs = get_integer_term (op);

	      /* We can't get calls to sym+N, N integer, can we?  */
	      gcc_assert (offs == 0);

	      op = gen_rtx_CONST (Pmode,
				  gen_rtx_UNSPEC (Pmode, gen_rtvec (1, sym),
						  CRIS_UNSPEC_PCREL));
	    }
	  else
	    op = force_reg (Pmode, op);

	  /* A local call.  */
	  *markerp = const0_rtx;
	}
      else if (t == cris_got_symbol)
	{
	  if (TARGET_AVOID_GOTPLT)
	    {
	      /* Change a "jsr sym" into (allocate register rM, rO)
		 "move.d (const (unspec [sym] CRIS_UNSPEC_PLT_GOTREL)),rM"
		 "add.d rPIC,rM,rO", "jsr rO" for pre-v32 and
		 "jsr (const (unspec [sym] CRIS_UNSPEC_PLT_PCREL))"
		 for v32.  */
	      rtx tem, rm, ro;

	      crtl->uses_pic_offset_table = 1;
	      tem = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, op),
				    TARGET_V32
				    ? CRIS_UNSPEC_PLT_PCREL
				    : CRIS_UNSPEC_PLT_GOTREL);
	      tem = gen_rtx_CONST (Pmode, tem);
	      if (TARGET_V32)
		op = tem;
	      else
		{
		  rm = gen_reg_rtx (Pmode);
		  emit_move_insn (rm, tem);
		  ro = gen_reg_rtx (Pmode);
		  if (expand_binop (Pmode, add_optab, rm,
				    pic_offset_table_rtx,
				    ro, 0, OPTAB_LIB_WIDEN) != ro)
		    internal_error ("expand_binop failed in movsi got");
		  op = ro;
		}
	    }
	  else
	    {
	      /* Change a "jsr sym" into (allocate register rM, rO)
		 "move.d (const (unspec [sym] CRIS_UNSPEC_PLTGOTREAD)),rM"
		 "add.d rPIC,rM,rO" "jsr [rO]" with the memory access
		 marked as not trapping and not aliasing.  No "move.d
		 [rO],rP" as that would invite to re-use of a value
		 that should not be reused.  FIXME: Need a peephole2
		 for cases when this is cse:d from the call, to change
		 back to just get the PLT entry address, so we don't
		 resolve the same symbol over and over (the memory
		 access of the PLTGOT isn't constant).  */
	      rtx tem, mem, rm, ro;

	      gcc_assert (can_create_pseudo_p ());
	      crtl->uses_pic_offset_table = 1;
	      tem = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, op),
				    CRIS_UNSPEC_PLTGOTREAD);
	      rm = gen_reg_rtx (Pmode);
	      emit_move_insn (rm, gen_rtx_CONST (Pmode, tem));
	      ro = gen_reg_rtx (Pmode);
	      if (expand_binop (Pmode, add_optab, rm,
				pic_offset_table_rtx,
				ro, 0, OPTAB_LIB_WIDEN) != ro)
		internal_error ("expand_binop failed in movsi got");
	      mem = gen_rtx_MEM (Pmode, ro);

	      /* This MEM doesn't alias anything.  Whether it aliases
		 other same symbols is unimportant.  */
	      set_mem_alias_set (mem, new_alias_set ());
	      MEM_NOTRAP_P (mem) = 1;
	      op = mem;
	    }

	  /* We need to prepare this call to go through the PLT; we
	     need to make GOT available.  */
	  *markerp = pic_offset_table_rtx;
	}
      else
	/* Can't possibly get anything else for a function-call, right?  */
	fatal_insn ("unidentifiable call op", op);

      /* If the validizing variant is called, it will try to validize
	 the address as a valid any-operand constant, but as it's only
	 valid for calls and moves, it will fail and always be forced
	 into a register.  */
      *opp = replace_equiv_address_nv (*opp, op);
    }
  else
    /* Can't tell what locality a call to a non-constant address has;
       better make the GOT register alive at it.
       FIXME: Can we see whether the register has known constant
       contents?  */
    *markerp = pic_offset_table_rtx;
}

/* Make sure operands are in the right order for an addsi3 insn as
   generated by a define_split.  Nothing but REG_P as the first
   operand is recognized by addsi3 after reload.  OPERANDS contains
   the operands, with the first at OPERANDS[N] and the second at
   OPERANDS[N+1].  */

void
cris_order_for_addsi3 (rtx *operands, int n)
{
  if (!REG_P (operands[n]))
    {
      rtx tem = operands[n];
      operands[n] = operands[n + 1];
      operands[n + 1] = tem;
    }
}

/* Use from within code, from e.g. PRINT_OPERAND and
   PRINT_OPERAND_ADDRESS.  Macros used in output_addr_const need to emit
   different things depending on whether code operand or constant is
   emitted.  */

static void
cris_output_addr_const (FILE *file, rtx x)
{
  in_code++;
  output_addr_const (file, x);
  in_code--;
}

/* Worker function for ASM_OUTPUT_SYMBOL_REF.  */

void
cris_asm_output_symbol_ref (FILE *file, rtx x)
{
  gcc_assert (GET_CODE (x) == SYMBOL_REF);

  if (flag_pic && in_code > 0)
    {
     const char *origstr = XSTR (x, 0);
     const char *str;
     str = (* targetm.strip_name_encoding) (origstr);
     assemble_name (file, str);

     /* Sanity check.  */
     if (!TARGET_V32 && !crtl->uses_pic_offset_table)
       output_operand_lossage ("PIC register isn't set up");
    }
  else
    assemble_name (file, XSTR (x, 0));
}

/* Worker function for ASM_OUTPUT_LABEL_REF.  */

void
cris_asm_output_label_ref (FILE *file, char *buf)
{
  if (flag_pic && in_code > 0)
    {
      assemble_name (file, buf);

      /* Sanity check.  */
      if (!TARGET_V32 && !crtl->uses_pic_offset_table)
	internal_error ("emitting PIC operand, but PIC register "
			"isn%'t set up");
    }
  else
    assemble_name (file, buf);
}

/* Worker function for TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA.  */

static bool
cris_output_addr_const_extra (FILE *file, rtx xconst)
{
  switch (GET_CODE (xconst))
    {
      rtx x;

    case UNSPEC:
      x = XVECEXP (xconst, 0, 0);
      CRIS_ASSERT (GET_CODE (x) == SYMBOL_REF
		   || GET_CODE (x) == LABEL_REF
		   || GET_CODE (x) == CONST);
      output_addr_const (file, x);
      switch (XINT (xconst, 1))
	{
	case CRIS_UNSPEC_PCREL:
	  /* We only get this with -fpic/PIC to tell it apart from an
	     invalid symbol.  We can't tell here, but it should only
	     be the operand of a call or movsi.  */
	  gcc_assert (TARGET_V32 && flag_pic);
	  break;

	case CRIS_UNSPEC_PLT_PCREL:
	  gcc_assert (TARGET_V32);
	  fprintf (file, ":PLT");
	  break;

	case CRIS_UNSPEC_PLT_GOTREL:
	  gcc_assert (!TARGET_V32);
	  fprintf (file, ":PLTG");
	  break;

	case CRIS_UNSPEC_GOTREL:
	  gcc_assert (!TARGET_V32);
	  fprintf (file, ":GOTOFF");
	  break;

	case CRIS_UNSPEC_GOTREAD:
	  if (flag_pic == 1)
	    fprintf (file, ":GOT16");
	  else
	    fprintf (file, ":GOT");
	  break;

	case CRIS_UNSPEC_PLTGOTREAD:
	  if (flag_pic == 1)
	    fprintf (file, CRIS_GOTPLT_SUFFIX "16");
	  else
	    fprintf (file, CRIS_GOTPLT_SUFFIX);
	  break;

	default:
	  gcc_unreachable ();
	}
      return true;

    default:
      return false;
    }
}

/* Worker function for TARGET_STRUCT_VALUE_RTX.  */

static rtx
cris_struct_value_rtx (tree fntype ATTRIBUTE_UNUSED,
		       int incoming ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (Pmode, CRIS_STRUCT_VALUE_REGNUM);
}

/* Worker function for TARGET_SETUP_INCOMING_VARARGS.  */

static void
cris_setup_incoming_varargs (cumulative_args_t ca_v,
			     machine_mode mode ATTRIBUTE_UNUSED,
			     tree type ATTRIBUTE_UNUSED,
			     int *pretend_arg_size,
			     int second_time)
{
  CUMULATIVE_ARGS *ca = get_cumulative_args (ca_v);

  if (ca->regs < CRIS_MAX_ARGS_IN_REGS)
    {
      int stdarg_regs = CRIS_MAX_ARGS_IN_REGS - ca->regs;
      cfun->machine->stdarg_regs = stdarg_regs;
      *pretend_arg_size = stdarg_regs * 4;
    }

  if (TARGET_PDEBUG)
    fprintf (asm_out_file,
	     "\n; VA:: ANSI: %d args before, anon @ #%d, %dtime\n",
	     ca->regs, *pretend_arg_size, second_time);
}

/* Return true if TYPE must be passed by invisible reference.
   For cris, we pass <= 8 bytes by value, others by reference.  */

static bool
cris_pass_by_reference (cumulative_args_t ca ATTRIBUTE_UNUSED,
			machine_mode mode, const_tree type,
			bool named ATTRIBUTE_UNUSED)
{
  return (targetm.calls.must_pass_in_stack (mode, type)
	  || CRIS_FUNCTION_ARG_SIZE (mode, type) > 8);
}

/* A combination of defining TARGET_PROMOTE_FUNCTION_MODE, promoting arguments
   and *not* defining TARGET_PROMOTE_PROTOTYPES or PROMOTE_MODE gives the
   best code size and speed for gcc, ipps and products in gcc-2.7.2.  */

machine_mode
cris_promote_function_mode (const_tree type ATTRIBUTE_UNUSED,
                            machine_mode mode,
                            int *punsignedp ATTRIBUTE_UNUSED,
			    const_tree fntype ATTRIBUTE_UNUSED,
                            int for_return)
{
  /* Defining PROMOTE_FUNCTION_RETURN in gcc-2.7.2 uncovered bug 981110 (even
     when modifying TARGET_FUNCTION_VALUE to return the promoted mode).
     Maybe pointless as of now, but let's keep the old behavior.  */
  if (for_return == 1)
    return mode;
  return CRIS_PROMOTED_MODE (mode, *punsignedp, type);
} 

/* Atomic types require alignment to be at least their "natural" size.  */

static unsigned int
cris_atomic_align_for_mode (machine_mode mode)
{
  return GET_MODE_BITSIZE (mode);
}

/* Let's assume all functions return in r[CRIS_FIRST_ARG_REG] for the
   time being.  */

static rtx
cris_function_value(const_tree type,
		    const_tree func ATTRIBUTE_UNUSED,
		    bool outgoing ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (TYPE_MODE (type), CRIS_FIRST_ARG_REG);
}

/* Let's assume all functions return in r[CRIS_FIRST_ARG_REG] for the
   time being.  */

static rtx
cris_libcall_value (machine_mode mode,
		    const_rtx fun ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (mode, CRIS_FIRST_ARG_REG);
}

/* Let's assume all functions return in r[CRIS_FIRST_ARG_REG] for the
   time being.  */

static bool
cris_function_value_regno_p (const unsigned int regno)
{
  return (regno == CRIS_FIRST_ARG_REG);
}

static int
cris_arg_partial_bytes (cumulative_args_t ca, machine_mode mode,
			tree type, bool named ATTRIBUTE_UNUSED)
{
  if (get_cumulative_args (ca)->regs == CRIS_MAX_ARGS_IN_REGS - 1
      && !targetm.calls.must_pass_in_stack (mode, type)
      && CRIS_FUNCTION_ARG_SIZE (mode, type) > 4
      && CRIS_FUNCTION_ARG_SIZE (mode, type) <= 8)
    return UNITS_PER_WORD;
  else
    return 0;
}

static rtx
cris_function_arg_1 (cumulative_args_t ca_v,
		     machine_mode mode ATTRIBUTE_UNUSED,
		     const_tree type ATTRIBUTE_UNUSED,
		     bool named, bool incoming)
{
  const CUMULATIVE_ARGS *ca = get_cumulative_args (ca_v);

  if ((!incoming || named) && ca->regs < CRIS_MAX_ARGS_IN_REGS)
    return gen_rtx_REG (mode, CRIS_FIRST_ARG_REG + ca->regs);
  else
    return NULL_RTX;
}

/* Worker function for TARGET_FUNCTION_ARG.
   The void_type_node is sent as a "closing" call.  */

static rtx
cris_function_arg (cumulative_args_t ca, machine_mode mode,
		   const_tree type, bool named)
{
  return cris_function_arg_1 (ca, mode, type, named, false);
}

/* Worker function for TARGET_FUNCTION_INCOMING_ARG.

   The differences between this and the previous, is that this one checks
   that an argument is named, since incoming stdarg/varargs arguments are
   pushed onto the stack, and we don't have to check against the "closing"
   void_type_node TYPE parameter.  */

static rtx
cris_function_incoming_arg (cumulative_args_t ca, machine_mode mode,
			    const_tree type, bool named)
{
  return cris_function_arg_1 (ca, mode, type, named, true);
}

/* Worker function for TARGET_FUNCTION_ARG_ADVANCE.  */

static void
cris_function_arg_advance (cumulative_args_t ca_v, machine_mode mode,
			   const_tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *ca = get_cumulative_args (ca_v);

  ca->regs += (3 + CRIS_FUNCTION_ARG_SIZE (mode, type)) / 4;
}

/* Worker function for TARGET_MD_ASM_ADJUST.  */

static rtx_insn *
cris_md_asm_adjust (vec<rtx> &outputs, vec<rtx> &inputs,
		    vec<const char *> &constraints,
		    vec<rtx> &clobbers, HARD_REG_SET &clobbered_regs)
{
  /* For the time being, all asms clobber condition codes.
     Revisit when there's a reasonable use for inputs/outputs
     that mention condition codes.  */
  clobbers.safe_push (gen_rtx_REG (CCmode, CRIS_CC0_REGNUM));
  SET_HARD_REG_BIT (clobbered_regs, CRIS_CC0_REGNUM);

  /* Determine if the source using MOF.  If it is, automatically
     clobbering MOF would cause it to have impossible constraints.  */

  /* Look for a use of the MOF constraint letter: h.  */
  for (unsigned i = 0, n = constraints.length(); i < n; ++i)
    if (strchr (constraints[i], 'h') != NULL)
      return NULL;

  /* Look for an output or an input that touches MOF.  */
  rtx mof_reg = gen_rtx_REG (SImode, CRIS_MOF_REGNUM);
  for (unsigned i = 0, n = outputs.length(); i < n; ++i)
    if (reg_overlap_mentioned_p (mof_reg, outputs[i]))
      return NULL;
  for (unsigned i = 0, n = inputs.length(); i < n; ++i)
    if (reg_overlap_mentioned_p (mof_reg, inputs[i]))
      return NULL;

  /* No direct reference to MOF or its constraint.
     Clobber it for backward compatibility.  */
  clobbers.safe_push (mof_reg);
  SET_HARD_REG_BIT (clobbered_regs, CRIS_MOF_REGNUM);
  return NULL;
}

/* Implement TARGET_FRAME_POINTER_REQUIRED.

   Really only needed if the stack frame has variable length (alloca
   or variable sized local arguments (GNU C extension).  See PR39499 and
   PR38609 for the reason this isn't just 0.  */

bool
cris_frame_pointer_required (void)
{
  return !crtl->sp_is_unchanging;
}

/* Implement TARGET_ASM_TRAMPOLINE_TEMPLATE.

   This looks too complicated, and it is.  I assigned r7 to be the
   static chain register, but it is call-saved, so we have to save it,
   and come back to restore it after the call, so we have to save srp...
   Anyway, trampolines are rare enough that we can cope with this
   somewhat lack of elegance.
    (Do not be tempted to "straighten up" whitespace in the asms; the
   assembler #NO_APP state mandates strict spacing).  */
/* ??? See the i386 regparm=3 implementation that pushes the static
   chain value to the stack in the trampoline, and uses a call-saved
   register when called directly.  */

static void
cris_asm_trampoline_template (FILE *f)
{
  if (TARGET_V32)
    {
      /* This normally-unused nop insn acts as an instruction to
	 the simulator to flush its instruction cache.  None of
	 the other instructions in the trampoline template suits
	 as a trigger for V32.  The pc-relative addressing mode
	 works nicely as a trigger for V10.
	 FIXME: Have specific V32 template (possibly avoiding the
	 use of a special instruction).  */
      fprintf (f, "\tclearf x\n");
      /* We have to use a register as an intermediate, choosing
	 semi-randomly R1 (which has to not be the STATIC_CHAIN_REGNUM),
	 so we can use it for address indirection and jsr target.  */
      fprintf (f, "\tmove $r1,$mof\n");
      /* +4 */
      fprintf (f, "\tmove.d 0,$r1\n");
      fprintf (f, "\tmove.d $%s,[$r1]\n", reg_names[STATIC_CHAIN_REGNUM]);
      fprintf (f, "\taddq 6,$r1\n");
      fprintf (f, "\tmove $mof,[$r1]\n");
      fprintf (f, "\taddq 6,$r1\n");
      fprintf (f, "\tmove $srp,[$r1]\n");
      /* +20 */
      fprintf (f, "\tmove.d 0,$%s\n", reg_names[STATIC_CHAIN_REGNUM]);
      /* +26 */
      fprintf (f, "\tmove.d 0,$r1\n");
      fprintf (f, "\tjsr $r1\n");
      fprintf (f, "\tsetf\n");
      /* +36 */
      fprintf (f, "\tmove.d 0,$%s\n", reg_names[STATIC_CHAIN_REGNUM]);
      /* +42 */
      fprintf (f, "\tmove.d 0,$r1\n");
      /* +48 */
      fprintf (f, "\tmove.d 0,$r9\n");
      fprintf (f, "\tjump $r9\n");
      fprintf (f, "\tsetf\n");
    }
  else
    {
      fprintf (f, "\tmove.d $%s,[$pc+20]\n", reg_names[STATIC_CHAIN_REGNUM]);
      fprintf (f, "\tmove $srp,[$pc+22]\n");
      fprintf (f, "\tmove.d 0,$%s\n", reg_names[STATIC_CHAIN_REGNUM]);
      fprintf (f, "\tjsr 0\n");
      fprintf (f, "\tmove.d 0,$%s\n", reg_names[STATIC_CHAIN_REGNUM]);
      fprintf (f, "\tjump 0\n");
    }
}

/* Implement TARGET_TRAMPOLINE_INIT.  */

static void
cris_trampoline_init (rtx m_tramp, tree fndecl, rtx chain_value)
{
  rtx fnaddr = XEXP (DECL_RTL (fndecl), 0);
  rtx tramp = XEXP (m_tramp, 0);
  rtx mem;

  emit_block_move (m_tramp, assemble_trampoline_template (),
		   GEN_INT (TRAMPOLINE_SIZE), BLOCK_OP_NORMAL);

  if (TARGET_V32)
    {
      mem = adjust_address (m_tramp, SImode, 6);
      emit_move_insn (mem, plus_constant (Pmode, tramp, 38));
      mem = adjust_address (m_tramp, SImode, 22);
      emit_move_insn (mem, chain_value);
      mem = adjust_address (m_tramp, SImode, 28);
      emit_move_insn (mem, fnaddr);
    }
  else
    {
      mem = adjust_address (m_tramp, SImode, 10);
      emit_move_insn (mem, chain_value);
      mem = adjust_address (m_tramp, SImode, 16);
      emit_move_insn (mem, fnaddr);
    }

  /* Note that there is no need to do anything with the cache for
     sake of a trampoline.  */
}

/* Implement TARGET_HARD_REGNO_MODE_OK.

   CRIS permits all registers to hold all modes.  Well, except for the
   condition-code register.  And we can't hold larger-than-register size
   modes in the last special register that can hold a full 32 bits.  */
static bool
cris_hard_regno_mode_ok (unsigned int regno, machine_mode mode)
{
  return ((mode == CCmode || regno != CRIS_CC0_REGNUM)
	  && (GET_MODE_SIZE (mode) <= UNITS_PER_WORD
	      || (regno != CRIS_MOF_REGNUM && regno != CRIS_ACR_REGNUM)));
}

#if 0
/* Various small functions to replace macros.  Only called from a
   debugger.  They might collide with gcc functions or system functions,
   so only emit them when '#if 1' above.  */

enum rtx_code Get_code (rtx);

enum rtx_code
Get_code (rtx x)
{
  return GET_CODE (x);
}

const char *Get_mode (rtx);

const char *
Get_mode (rtx x)
{
  return GET_MODE_NAME (GET_MODE (x));
}

rtx Xexp (rtx, int);

rtx
Xexp (rtx x, int n)
{
  return XEXP (x, n);
}

rtx Xvecexp (rtx, int, int);

rtx
Xvecexp (rtx x, int n, int m)
{
  return XVECEXP (x, n, m);
}

int Get_rtx_len (rtx);

int
Get_rtx_len (rtx x)
{
  return GET_RTX_LENGTH (GET_CODE (x));
}

/* Use upper-case to distinguish from local variables that are sometimes
   called next_insn and prev_insn.  */

rtx Next_insn (rtx);

rtx
Next_insn (rtx insn)
{
  return NEXT_INSN (insn);
}

rtx Prev_insn (rtx);

rtx
Prev_insn (rtx insn)
{
  return PREV_INSN (insn);
}
#endif

#include "gt-cris.h"

/*
 * Local variables:
 * eval: (c-set-style "gnu")
 * indent-tabs-mode: t
 * End:
 */
