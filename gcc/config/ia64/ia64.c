/* Definitions of target machine for GNU compiler.
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.
   Contributed by James E. Wilson <wilson@cygnus.com> and
   		  David Mosberger <davidm@hpl.hp.com>.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "tree.h"
#include "tm_p.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "recog.h"
#include "expr.h"
#include "obstack.h"
#include "except.h"
#include "function.h"
#include "ggc.h"
#include "basic-block.h"
#include "toplev.h"

/* This is used for communication between ASM_OUTPUT_LABEL and
   ASM_OUTPUT_LABELREF.  */
int ia64_asm_output_label = 0;

/* Define the information needed to generate branch and scc insns.  This is
   stored from the compare operation.  */
struct rtx_def * ia64_compare_op0;
struct rtx_def * ia64_compare_op1;

/* Register names for ia64_expand_prologue.  */
static const char * const ia64_reg_numbers[96] =
{ "r32", "r33", "r34", "r35", "r36", "r37", "r38", "r39",
  "r40", "r41", "r42", "r43", "r44", "r45", "r46", "r47",
  "r48", "r49", "r50", "r51", "r52", "r53", "r54", "r55",
  "r56", "r57", "r58", "r59", "r60", "r61", "r62", "r63",
  "r64", "r65", "r66", "r67", "r68", "r69", "r70", "r71",
  "r72", "r73", "r74", "r75", "r76", "r77", "r78", "r79",
  "r80", "r81", "r82", "r83", "r84", "r85", "r86", "r87",
  "r88", "r89", "r90", "r91", "r92", "r93", "r94", "r95",
  "r96", "r97", "r98", "r99", "r100","r101","r102","r103",
  "r104","r105","r106","r107","r108","r109","r110","r111",
  "r112","r113","r114","r115","r116","r117","r118","r119",
  "r120","r121","r122","r123","r124","r125","r126","r127"};

/* ??? These strings could be shared with REGISTER_NAMES.  */
static const char * const ia64_input_reg_names[8] =
{ "in0",  "in1",  "in2",  "in3",  "in4",  "in5",  "in6",  "in7" };

/* ??? These strings could be shared with REGISTER_NAMES.  */
static const char * const ia64_local_reg_names[80] =
{ "loc0", "loc1", "loc2", "loc3", "loc4", "loc5", "loc6", "loc7",
  "loc8", "loc9", "loc10","loc11","loc12","loc13","loc14","loc15",
  "loc16","loc17","loc18","loc19","loc20","loc21","loc22","loc23",
  "loc24","loc25","loc26","loc27","loc28","loc29","loc30","loc31",
  "loc32","loc33","loc34","loc35","loc36","loc37","loc38","loc39",
  "loc40","loc41","loc42","loc43","loc44","loc45","loc46","loc47",
  "loc48","loc49","loc50","loc51","loc52","loc53","loc54","loc55",
  "loc56","loc57","loc58","loc59","loc60","loc61","loc62","loc63",
  "loc64","loc65","loc66","loc67","loc68","loc69","loc70","loc71",
  "loc72","loc73","loc74","loc75","loc76","loc77","loc78","loc79" };

/* ??? These strings could be shared with REGISTER_NAMES.  */
static const char * const ia64_output_reg_names[8] =
{ "out0", "out1", "out2", "out3", "out4", "out5", "out6", "out7" };

/* String used with the -mfixed-range= option.  */
const char *ia64_fixed_range_string;

/* Variables which are this size or smaller are put in the sdata/sbss
   sections.  */

unsigned int ia64_section_threshold;

static int find_gr_spill PARAMS ((int));
static int next_scratch_gr_reg PARAMS ((void));
static void mark_reg_gr_used_mask PARAMS ((rtx, void *));
static void ia64_compute_frame_size PARAMS ((HOST_WIDE_INT));
static void setup_spill_pointers PARAMS ((int, rtx, HOST_WIDE_INT));
static void finish_spill_pointers PARAMS ((void));
static rtx spill_restore_mem PARAMS ((rtx, HOST_WIDE_INT));
static void do_spill PARAMS ((rtx (*)(rtx, rtx, rtx), rtx, HOST_WIDE_INT, rtx));
static void do_restore PARAMS ((rtx (*)(rtx, rtx, rtx), rtx, HOST_WIDE_INT));
static rtx gen_movdi_x PARAMS ((rtx, rtx, rtx));
static rtx gen_fr_spill_x PARAMS ((rtx, rtx, rtx));
static rtx gen_fr_restore_x PARAMS ((rtx, rtx, rtx));

static enum machine_mode hfa_element_mode PARAMS ((tree, int));
static void fix_range PARAMS ((const char *));
static void ia64_add_gc_roots PARAMS ((void));
static void ia64_init_machine_status PARAMS ((struct function *));
static void ia64_mark_machine_status PARAMS ((struct function *));
static void emit_insn_group_barriers PARAMS ((rtx));
static void emit_predicate_relation_info PARAMS ((rtx));
static int process_set PARAMS ((FILE *, rtx));

static rtx ia64_expand_fetch_and_op PARAMS ((optab, enum machine_mode,
					     tree, rtx));
static rtx ia64_expand_op_and_fetch PARAMS ((optab, enum machine_mode,
					     tree, rtx));
static rtx ia64_expand_compare_and_swap PARAMS ((enum machine_mode, int,
						 tree, rtx));
static rtx ia64_expand_lock_test_and_set PARAMS ((enum machine_mode,
						  tree, rtx));
static rtx ia64_expand_lock_release PARAMS ((enum machine_mode, tree, rtx));


/* Return 1 if OP is a valid operand for the MEM of a CALL insn.  */

int
call_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (mode != GET_MODE (op))
    return 0;

  return (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == REG
	  || (GET_CODE (op) == SUBREG && GET_CODE (XEXP (op, 0)) == REG));
}

/* Return 1 if OP refers to a symbol in the sdata section.  */

int
sdata_symbolic_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  switch (GET_CODE (op))
    {
    case CONST:
      if (GET_CODE (XEXP (op, 0)) != PLUS
	  || GET_CODE (XEXP (XEXP (op, 0), 0)) != SYMBOL_REF)
	break;
      op = XEXP (XEXP (op, 0), 0);
      /* FALLTHRU */

    case SYMBOL_REF:
      if (CONSTANT_POOL_ADDRESS_P (op))
	return GET_MODE_SIZE (get_pool_mode (op)) <= ia64_section_threshold;
      else
        return XSTR (op, 0)[0] == SDATA_NAME_FLAG_CHAR;

    default:
      break;
    }

  return 0;
}

/* Return 1 if OP refers to a symbol, and is appropriate for a GOT load.  */

int
got_symbolic_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  switch (GET_CODE (op))
    {
    case CONST:
      op = XEXP (op, 0);
      if (GET_CODE (op) != PLUS)
	return 0;
      if (GET_CODE (XEXP (op, 0)) != SYMBOL_REF)
	return 0;
      op = XEXP (op, 1);
      if (GET_CODE (op) != CONST_INT)
	return 0;

	return 1;

      /* Ok if we're not using GOT entries at all.  */
      if (TARGET_NO_PIC || TARGET_AUTO_PIC)
	return 1;

      /* "Ok" while emitting rtl, since otherwise we won't be provided
	 with the entire offset during emission, which makes it very
	 hard to split the offset into high and low parts.  */
      if (rtx_equal_function_value_matters)
	return 1;

      /* Force the low 14 bits of the constant to zero so that we do not
	 use up so many GOT entries.  */
      return (INTVAL (op) & 0x3fff) == 0;

    case SYMBOL_REF:
    case LABEL_REF:
      return 1;

    default:
      break;
    }
  return 0;
}

/* Return 1 if OP refers to a symbol.  */

int
symbolic_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  switch (GET_CODE (op))
    {
    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      return 1;

    default:
      break;
    }
  return 0;
}

/* Return 1 if OP refers to a function.  */

int
function_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) == SYMBOL_REF && SYMBOL_REF_FLAG (op))
    return 1;
  else
    return 0;
}

/* Return 1 if OP is setjmp or a similar function.  */

/* ??? This is an unsatisfying solution.  Should rethink.  */

int
setjmp_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  const char *name;
  int retval = 0;

  if (GET_CODE (op) != SYMBOL_REF)
    return 0;

  name = XSTR (op, 0);

  /* The following code is borrowed from special_function_p in calls.c.  */

  /* Disregard prefix _, __ or __x.  */
  if (name[0] == '_')
    {
      if (name[1] == '_' && name[2] == 'x')
	name += 3;
      else if (name[1] == '_')
	name += 2;
      else
	name += 1;
    }

  if (name[0] == 's')
    {
      retval
	= ((name[1] == 'e'
	    && (! strcmp (name, "setjmp")
		|| ! strcmp (name, "setjmp_syscall")))
	   || (name[1] == 'i'
	       && ! strcmp (name, "sigsetjmp"))
	   || (name[1] == 'a'
	       && ! strcmp (name, "savectx")));
    }
  else if ((name[0] == 'q' && name[1] == 's'
	    && ! strcmp (name, "qsetjmp"))
	   || (name[0] == 'v' && name[1] == 'f'
	       && ! strcmp (name, "vfork")))
    retval = 1;

  return retval;
}

/* Return 1 if OP is a general operand, but when pic exclude symbolic
   operands.  */

/* ??? If we drop no-pic support, can delete SYMBOL_REF, CONST, and LABEL_REF
   from PREDICATE_CODES.  */

int
move_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (! TARGET_NO_PIC && symbolic_operand (op, mode))
    return 0;

  return general_operand (op, mode);
}

/* Return 1 if OP is a register operand that is (or could be) a GR reg.  */

int
gr_register_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (! register_operand (op, mode))
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (GET_CODE (op) == REG)
    {
      unsigned int regno = REGNO (op);
      if (regno < FIRST_PSEUDO_REGISTER)
	return GENERAL_REGNO_P (regno);
    }
  return 1;
}

/* Return 1 if OP is a register operand that is (or could be) an FR reg.  */

int
fr_register_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (! register_operand (op, mode))
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (GET_CODE (op) == REG)
    {
      unsigned int regno = REGNO (op);
      if (regno < FIRST_PSEUDO_REGISTER)
	return FR_REGNO_P (regno);
    }
  return 1;
}

/* Return 1 if OP is a register operand that is (or could be) a GR/FR reg.  */

int
grfr_register_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (! register_operand (op, mode))
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (GET_CODE (op) == REG)
    {
      unsigned int regno = REGNO (op);
      if (regno < FIRST_PSEUDO_REGISTER)
	return GENERAL_REGNO_P (regno) || FR_REGNO_P (regno);
    }
  return 1;
}

/* Return 1 if OP is a nonimmediate operand that is (or could be) a GR reg.  */

int
gr_nonimmediate_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (! nonimmediate_operand (op, mode))
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (GET_CODE (op) == REG)
    {
      unsigned int regno = REGNO (op);
      if (regno < FIRST_PSEUDO_REGISTER)
	return GENERAL_REGNO_P (regno);
    }
  return 1;
}

/* Return 1 if OP is a nonimmediate operand that is a GR/FR reg.  */

int
grfr_nonimmediate_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (! nonimmediate_operand (op, mode))
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (GET_CODE (op) == REG)
    {
      unsigned int regno = REGNO (op);
      if (regno < FIRST_PSEUDO_REGISTER)
	return GENERAL_REGNO_P (regno) || FR_REGNO_P (regno);
    }
  return 1;
}

/* Return 1 if OP is a GR register operand, or zero.  */

int
gr_reg_or_0_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (op == const0_rtx || gr_register_operand (op, mode));
}

/* Return 1 if OP is a GR register operand, or a 5 bit immediate operand.  */

int
gr_reg_or_5bit_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == CONST_INT && INTVAL (op) >= 0 && INTVAL (op) < 32)
	  || GET_CODE (op) == CONSTANT_P_RTX
	  || gr_register_operand (op, mode));
}

/* Return 1 if OP is a GR register operand, or a 6 bit immediate operand.  */

int
gr_reg_or_6bit_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == CONST_INT && CONST_OK_FOR_M (INTVAL (op)))
	  || GET_CODE (op) == CONSTANT_P_RTX
	  || gr_register_operand (op, mode));
}

/* Return 1 if OP is a GR register operand, or an 8 bit immediate operand.  */

int
gr_reg_or_8bit_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == CONST_INT && CONST_OK_FOR_K (INTVAL (op)))
	  || GET_CODE (op) == CONSTANT_P_RTX
	  || gr_register_operand (op, mode));
}

/* Return 1 if OP is a GR/FR register operand, or an 8 bit immediate.  */

int
grfr_reg_or_8bit_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == CONST_INT && CONST_OK_FOR_K (INTVAL (op)))
	  || GET_CODE (op) == CONSTANT_P_RTX
	  || grfr_register_operand (op, mode));
}

/* Return 1 if OP is a register operand, or an 8 bit adjusted immediate
   operand.  */

int
gr_reg_or_8bit_adjusted_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == CONST_INT && CONST_OK_FOR_L (INTVAL (op)))
	  || GET_CODE (op) == CONSTANT_P_RTX
	  || gr_register_operand (op, mode));
}

/* Return 1 if OP is a register operand, or is valid for both an 8 bit
   immediate and an 8 bit adjusted immediate operand.  This is necessary
   because when we emit a compare, we don't know what the condition will be,
   so we need the union of the immediates accepted by GT and LT.  */

int
gr_reg_or_8bit_and_adjusted_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == CONST_INT && CONST_OK_FOR_K (INTVAL (op))
	   && CONST_OK_FOR_L (INTVAL (op)))
	  || GET_CODE (op) == CONSTANT_P_RTX
	  || gr_register_operand (op, mode));
}

/* Return 1 if OP is a register operand, or a 14 bit immediate operand.  */

int
gr_reg_or_14bit_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == CONST_INT && CONST_OK_FOR_I (INTVAL (op)))
	  || GET_CODE (op) == CONSTANT_P_RTX
	  || gr_register_operand (op, mode));
}

/* Return 1 if OP is a register operand, or a 22 bit immediate operand.  */

int
gr_reg_or_22bit_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == CONST_INT && CONST_OK_FOR_J (INTVAL (op)))
	  || GET_CODE (op) == CONSTANT_P_RTX
	  || gr_register_operand (op, mode));
}

/* Return 1 if OP is a 6 bit immediate operand.  */

int
shift_count_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return ((GET_CODE (op) == CONST_INT && CONST_OK_FOR_M (INTVAL (op)))
	  || GET_CODE (op) == CONSTANT_P_RTX);
}

/* Return 1 if OP is a 5 bit immediate operand.  */

int
shift_32bit_count_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return ((GET_CODE (op) == CONST_INT
	   && (INTVAL (op) >= 0 && INTVAL (op) < 32))
	  || GET_CODE (op) == CONSTANT_P_RTX);
}

/* Return 1 if OP is a 2, 4, 8, or 16 immediate operand.  */

int
shladd_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == CONST_INT
	  && (INTVAL (op) == 2 || INTVAL (op) == 4
	      || INTVAL (op) == 8 || INTVAL (op) == 16));
}

/* Return 1 if OP is a -16, -8, -4, -1, 1, 4, 8, or 16 immediate operand. */

int
fetchadd_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == CONST_INT
          && (INTVAL (op) == -16 || INTVAL (op) == -8 ||
              INTVAL (op) == -4  || INTVAL (op) == -1 ||
              INTVAL (op) == 1   || INTVAL (op) == 4  ||
              INTVAL (op) == 8   || INTVAL (op) == 16));
}

/* Return 1 if OP is a floating-point constant zero, one, or a register.  */

int
fr_reg_or_fp01_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == CONST_DOUBLE && CONST_DOUBLE_OK_FOR_G (op))
	  || fr_register_operand (op, mode));
}

/* Like nonimmediate_operand, but don't allow MEMs that try to use a
   POST_MODIFY with a REG as displacement.  */

int
destination_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (! nonimmediate_operand (op, mode))
    return 0;
  if (GET_CODE (op) == MEM
      && GET_CODE (XEXP (op, 0)) == POST_MODIFY
      && GET_CODE (XEXP (XEXP (XEXP (op, 0), 1), 1)) == REG)
    return 0;
  return 1;
}

/* Like memory_operand, but don't allow post-increments.  */

int
not_postinc_memory_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (memory_operand (op, mode)
	  && GET_RTX_CLASS (GET_CODE (XEXP (op, 0))) != 'a');
}

/* Return 1 if this is a comparison operator, which accepts an normal 8-bit
   signed immediate operand.  */

int
normal_comparison_operator (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (op);
  return ((mode == VOIDmode || GET_MODE (op) == mode)
	  && (code == EQ || code == NE
	      || code == GT || code == LE || code == GTU || code == LEU));
}

/* Return 1 if this is a comparison operator, which accepts an adjusted 8-bit
   signed immediate operand.  */

int
adjusted_comparison_operator (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (op);
  return ((mode == VOIDmode || GET_MODE (op) == mode)
	  && (code == LT || code == GE || code == LTU || code == GEU));
}

/* Return 1 if OP is a call returning an HFA.  It is known to be a PARALLEL
   and the first section has already been tested.  */

int
call_multiple_values_operation (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  int count = XVECLEN (op, 0) - 2;
  int i;
  unsigned int dest_regno;

  /* Perform a quick check so we don't block up below.  */
  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != REG
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != CALL)
    return 0;

  dest_regno = REGNO (SET_DEST (XVECEXP (op, 0, 0)));

  for (i = 1; i < count; i++)
    {
      rtx elt = XVECEXP (op, 0, i + 2);

      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_SRC (elt)) != CALL
	  || GET_CODE (SET_DEST (elt)) != REG
	  || REGNO (SET_DEST (elt)) != dest_regno + i)
	return 0;
    }

  return 1;
}

/* Return 1 if this operator is valid for predication.  */

int
predicate_operator (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (op);
  return ((GET_MODE (op) == mode || mode == VOIDmode)
	  && (code == EQ || code == NE));
}

/* Return 1 if this is the ar.lc register.  */

int
ar_lc_reg_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (GET_MODE (op) == DImode
	  && (mode == DImode || mode == VOIDmode)
	  && GET_CODE (op) == REG
	  && REGNO (op) == AR_LC_REGNUM);
}

/* Return 1 if this is the ar.ccv register.  */

int
ar_ccv_reg_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return ((GET_MODE (op) == mode || mode == VOIDmode)
	  && GET_CODE (op) == REG
	  && REGNO (op) == AR_CCV_REGNUM);
}

/* Like general_operand, but don't allow (mem (addressof)).  */

int
general_tfmode_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (! general_operand (op, mode))
    return 0;
  if (GET_CODE (op) == MEM && GET_CODE (XEXP (op, 0)) == ADDRESSOF)
    return 0;
  return 1;
}

/* Similarly.  */

int
destination_tfmode_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (! destination_operand (op, mode))
    return 0;
  if (GET_CODE (op) == MEM && GET_CODE (XEXP (op, 0)) == ADDRESSOF)
    return 0;
  return 1;
}

/* Similarly.  */

int
tfreg_or_fp01_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == SUBREG)
    return 0;
  return fr_reg_or_fp01_operand (op, mode);
}

/* Return 1 if the operands of a move are ok.  */

int
ia64_move_ok (dst, src)
     rtx dst, src;
{
  /* If we're under init_recog_no_volatile, we'll not be able to use
     memory_operand.  So check the code directly and don't worry about
     the validity of the underlying address, which should have been
     checked elsewhere anyway.  */
  if (GET_CODE (dst) != MEM)
    return 1;
  if (GET_CODE (src) == MEM)
    return 0;
  if (register_operand (src, VOIDmode))
    return 1;

  /* Otherwise, this must be a constant, and that either 0 or 0.0 or 1.0.  */
  if (INTEGRAL_MODE_P (GET_MODE (dst)))
    return src == const0_rtx;
  else
    return GET_CODE (src) == CONST_DOUBLE && CONST_DOUBLE_OK_FOR_G (src);
}

/* Check if OP is a mask suitible for use with SHIFT in a dep.z instruction.
   Return the length of the field, or <= 0 on failure.  */

int
ia64_depz_field_mask (rop, rshift)
     rtx rop, rshift;
{
  unsigned HOST_WIDE_INT op = INTVAL (rop);
  unsigned HOST_WIDE_INT shift = INTVAL (rshift);

  /* Get rid of the zero bits we're shifting in.  */
  op >>= shift;

  /* We must now have a solid block of 1's at bit 0.  */
  return exact_log2 (op + 1);
}

/* Expand a symbolic constant load.  */
/* ??? Should generalize this, so that we can also support 32 bit pointers.  */

void
ia64_expand_load_address (dest, src)
      rtx dest, src;
{
  rtx temp;

  /* The destination could be a MEM during initial rtl generation,
     which isn't a valid destination for the PIC load address patterns.  */
  if (! register_operand (dest, DImode))
    temp = gen_reg_rtx (DImode);
  else
    temp = dest;

  if (TARGET_AUTO_PIC)
    emit_insn (gen_load_gprel64 (temp, src));
  else if (GET_CODE (src) == SYMBOL_REF && SYMBOL_REF_FLAG (src))
    emit_insn (gen_load_fptr (temp, src));
  else if (sdata_symbolic_operand (src, DImode))
    emit_insn (gen_load_gprel (temp, src));
  else if (GET_CODE (src) == CONST
	   && GET_CODE (XEXP (src, 0)) == PLUS
	   && GET_CODE (XEXP (XEXP (src, 0), 1)) == CONST_INT
	   && (INTVAL (XEXP (XEXP (src, 0), 1)) & 0x1fff) != 0)
    {
      rtx subtarget = no_new_pseudos ? temp : gen_reg_rtx (DImode);
      rtx sym = XEXP (XEXP (src, 0), 0);
      HOST_WIDE_INT ofs, hi, lo;

      /* Split the offset into a sign extended 14-bit low part
	 and a complementary high part.  */
      ofs = INTVAL (XEXP (XEXP (src, 0), 1));
      lo = ((ofs & 0x3fff) ^ 0x2000) - 0x2000;
      hi = ofs - lo;

      emit_insn (gen_load_symptr (subtarget, plus_constant (sym, hi)));
      emit_insn (gen_adddi3 (temp, subtarget, GEN_INT (lo)));
    }
  else
    emit_insn (gen_load_symptr (temp, src));

  if (temp != dest)
    emit_move_insn (dest, temp);
}

rtx
ia64_gp_save_reg (setjmp_p)
     int setjmp_p;
{
  rtx save = cfun->machine->ia64_gp_save;

  if (save != NULL)
    {
      /* We can't save GP in a pseudo if we are calling setjmp, because
	 pseudos won't be restored by longjmp.  For now, we save it in r4.  */
      /* ??? It would be more efficient to save this directly into a stack
	 slot.  Unfortunately, the stack slot address gets cse'd across
	 the setjmp call because the NOTE_INSN_SETJMP note is in the wrong
	 place.  */

      /* ??? Get the barf bag, Virginia.  We've got to replace this thing
         in place, since this rtx is used in exception handling receivers.
         Moreover, we must get this rtx out of regno_reg_rtx or reload
         will do the wrong thing.  */
      unsigned int old_regno = REGNO (save);
      if (setjmp_p && old_regno != GR_REG (4))
        {
          REGNO (save) = GR_REG (4);
          regno_reg_rtx[old_regno] = gen_rtx_raw_REG (DImode, old_regno);
        }
    }
  else
    {
      if (setjmp_p)
	save = gen_rtx_REG (DImode, GR_REG (4));
      else if (! optimize)
	save = gen_rtx_REG (DImode, LOC_REG (0));
      else
	save = gen_reg_rtx (DImode);
      cfun->machine->ia64_gp_save = save;
    }

  return save;
}

/* Split a post-reload TImode reference into two DImode components.  */

rtx
ia64_split_timode (out, in, scratch)
     rtx out[2];
     rtx in, scratch;
{
  switch (GET_CODE (in))
    {
    case REG:
      out[0] = gen_rtx_REG (DImode, REGNO (in));
      out[1] = gen_rtx_REG (DImode, REGNO (in) + 1);
      return NULL_RTX;

    case MEM:
      {
	rtx base = XEXP (in, 0);

	switch (GET_CODE (base))
	  {
	  case REG:
	    out[0] = change_address (in, DImode, NULL_RTX);
	    break;
	  case POST_MODIFY:
	    base = XEXP (base, 0);
	    out[0] = change_address (in, DImode, NULL_RTX);
	    break;

	  /* Since we're changing the mode, we need to change to POST_MODIFY
	     as well to preserve the size of the increment.  Either that or
	     do the update in two steps, but we've already got this scratch
	     register handy so let's use it.  */
	  case POST_INC:
	    base = XEXP (base, 0);
	    out[0] = change_address (in, DImode,
	      gen_rtx_POST_MODIFY (Pmode, base,plus_constant (base, 16)));
	    break;
	  case POST_DEC:
	    base = XEXP (base, 0);
	    out[0] = change_address (in, DImode,
	      gen_rtx_POST_MODIFY (Pmode, base,plus_constant (base, -16)));
	    break;
	  default:
	    abort ();
	  }

	if (scratch == NULL_RTX)
	  abort ();
	out[1] = change_address (in, DImode, scratch);
	return gen_adddi3 (scratch, base, GEN_INT (8));
      }

    case CONST_INT:
    case CONST_DOUBLE:
      split_double (in, &out[0], &out[1]);
      return NULL_RTX;

    default:
      abort ();
    }
}

/* ??? Fixing GR->FR TFmode moves during reload is hard.  You need to go
   through memory plus an extra GR scratch register.  Except that you can
   either get the first from SECONDARY_MEMORY_NEEDED or the second from
   SECONDARY_RELOAD_CLASS, but not both.

   We got into problems in the first place by allowing a construct like
   (subreg:TF (reg:TI)), which we got from a union containing a long double.  
   This solution attempts to prevent this situation from ocurring.  When
   we see something like the above, we spill the inner register to memory.  */

rtx
spill_tfmode_operand (in, force)
     rtx in;
     int force;
{
  if (GET_CODE (in) == SUBREG
      && GET_MODE (SUBREG_REG (in)) == TImode
      && GET_CODE (SUBREG_REG (in)) == REG)
    {
      rtx mem = gen_mem_addressof (SUBREG_REG (in), NULL_TREE);
      return gen_rtx_MEM (TFmode, copy_to_reg (XEXP (mem, 0)));
    }
  else if (force && GET_CODE (in) == REG)
    {
      rtx mem = gen_mem_addressof (in, NULL_TREE);
      return gen_rtx_MEM (TFmode, copy_to_reg (XEXP (mem, 0)));
    }
  else if (GET_CODE (in) == MEM
	   && GET_CODE (XEXP (in, 0)) == ADDRESSOF)
    {
      return change_address (in, TFmode, copy_to_reg (XEXP (in, 0)));
    }
  else
    return in;
}

/* Begin the assembly file.  */

void
ia64_file_start (f)
     FILE *f;
{
  unsigned int rs, re;
  int out_state;

  rs = 1;
  out_state = 0;
  while (1)
    {
      while (rs < 64 && call_used_regs[PR_REG (rs)])
	rs++;
      if (rs >= 64)
	break;
      for (re = rs + 1; re < 64 && ! call_used_regs[PR_REG (re)]; re++)
	continue;
      if (out_state == 0)
	{
	  fputs ("\t.pred.safe_across_calls ", f);
	  out_state = 1;
	}
      else
	fputc (',', f);
      if (re == rs + 1)
	fprintf (f, "p%u", rs);
      else
	fprintf (f, "p%u-p%u", rs, re - 1);
      rs = re + 1;
    }
  if (out_state)
    fputc ('\n', f);
}


/* Structure to be filled in by ia64_compute_frame_size with register
   save masks and offsets for the current function.  */

struct ia64_frame_info
{
  HOST_WIDE_INT total_size;	/* size of the stack frame, not including
				   the caller's scratch area.  */
  HOST_WIDE_INT spill_cfa_off;	/* top of the reg spill area from the cfa.  */
  HOST_WIDE_INT spill_size;	/* size of the gr/br/fr spill area.  */
  HOST_WIDE_INT extra_spill_size;  /* size of spill area for others.  */
  HARD_REG_SET mask;		/* mask of saved registers.  */
  unsigned int gr_used_mask;	/* mask of registers in use as gr spill 
				   registers or long-term scratches.  */
  int n_spilled;		/* number of spilled registers.  */
  int reg_fp;			/* register for fp.  */
  int reg_save_b0;		/* save register for b0.  */
  int reg_save_pr;		/* save register for prs.  */
  int reg_save_ar_pfs;		/* save register for ar.pfs.  */
  int reg_save_ar_unat;		/* save register for ar.unat.  */
  int reg_save_ar_lc;		/* save register for ar.lc.  */
  int n_input_regs;		/* number of input registers used.  */
  int n_local_regs;		/* number of local registers used.  */
  int n_output_regs;		/* number of output registers used.  */
  int n_rotate_regs;		/* number of rotating registers used.  */

  char need_regstk;		/* true if a .regstk directive needed.  */
  char initialized;		/* true if the data is finalized.  */
};

/* Current frame information calculated by ia64_compute_frame_size.  */
static struct ia64_frame_info current_frame_info;

/* Helper function for ia64_compute_frame_size: find an appropriate general
   register to spill some special register to.  SPECIAL_SPILL_MASK contains
   bits in GR0 to GR31 that have already been allocated by this routine.
   TRY_LOCALS is true if we should attempt to locate a local regnum.  */

static int
find_gr_spill (try_locals)
     int try_locals;
{
  int regno;

  /* If this is a leaf function, first try an otherwise unused
     call-clobbered register.  */
  if (current_function_is_leaf)
    {
      for (regno = GR_REG (1); regno <= GR_REG (31); regno++)
	if (! regs_ever_live[regno]
	    && call_used_regs[regno]
	    && ! fixed_regs[regno]
	    && ! global_regs[regno]
	    && ((current_frame_info.gr_used_mask >> regno) & 1) == 0)
	  {
	    current_frame_info.gr_used_mask |= 1 << regno;
	    return regno;
	  }
    }

  if (try_locals)
    {
      regno = current_frame_info.n_local_regs;
      if (regno < 80)
	{
	  current_frame_info.n_local_regs = regno + 1;
	  return LOC_REG (0) + regno;
	}
    }

  /* Failed to find a general register to spill to.  Must use stack.  */
  return 0;
}

/* In order to make for nice schedules, we try to allocate every temporary
   to a different register.  We must of course stay away from call-saved,
   fixed, and global registers.  We must also stay away from registers
   allocated in current_frame_info.gr_used_mask, since those include regs
   used all through the prologue.

   Any register allocated here must be used immediately.  The idea is to
   aid scheduling, not to solve data flow problems.  */

static int last_scratch_gr_reg;

static int
next_scratch_gr_reg ()
{
  int i, regno;

  for (i = 0; i < 32; ++i)
    {
      regno = (last_scratch_gr_reg + i + 1) & 31;
      if (call_used_regs[regno]
	  && ! fixed_regs[regno]
	  && ! global_regs[regno]
	  && ((current_frame_info.gr_used_mask >> regno) & 1) == 0)
	{
	  last_scratch_gr_reg = regno;
	  return regno;
	}
    }

  /* There must be _something_ available.  */
  abort ();
}

/* Helper function for ia64_compute_frame_size, called through
   diddle_return_value.  Mark REG in current_frame_info.gr_used_mask.  */

static void
mark_reg_gr_used_mask (reg, data)
     rtx reg;
     void *data ATTRIBUTE_UNUSED;
{
  unsigned int regno = REGNO (reg);
  if (regno < 32)
    current_frame_info.gr_used_mask |= 1 << regno;
}

/* Returns the number of bytes offset between the frame pointer and the stack
   pointer for the current function.  SIZE is the number of bytes of space
   needed for local variables.  */

static void
ia64_compute_frame_size (size)
     HOST_WIDE_INT size;
{
  HOST_WIDE_INT total_size;
  HOST_WIDE_INT spill_size = 0;
  HOST_WIDE_INT extra_spill_size = 0;
  HOST_WIDE_INT pretend_args_size;
  HARD_REG_SET mask;
  int n_spilled = 0;
  int spilled_gr_p = 0;
  int spilled_fr_p = 0;
  unsigned int regno;
  int i;

  if (current_frame_info.initialized)
    return;

  memset (&current_frame_info, 0, sizeof current_frame_info);
  CLEAR_HARD_REG_SET (mask);

  /* Don't allocate scratches to the return register.  */
  diddle_return_value (mark_reg_gr_used_mask, NULL);

  /* Don't allocate scratches to the EH scratch registers.  */
  if (cfun->machine->ia64_eh_epilogue_sp)
    mark_reg_gr_used_mask (cfun->machine->ia64_eh_epilogue_sp, NULL);
  if (cfun->machine->ia64_eh_epilogue_bsp)
    mark_reg_gr_used_mask (cfun->machine->ia64_eh_epilogue_bsp, NULL);

  /* Find the size of the register stack frame.  We have only 80 local
     registers, because we reserve 8 for the inputs and 8 for the
     outputs.  */

  /* Skip HARD_FRAME_POINTER_REGNUM (loc79) when frame_pointer_needed,
     since we'll be adjusting that down later.  */
  regno = LOC_REG (78) + ! frame_pointer_needed;
  for (; regno >= LOC_REG (0); regno--)
    if (regs_ever_live[regno])
      break;
  current_frame_info.n_local_regs = regno - LOC_REG (0) + 1;

  if (cfun->machine->n_varargs > 0)
    current_frame_info.n_input_regs = 8;
  else
    {
      for (regno = IN_REG (7); regno >= IN_REG (0); regno--)
	if (regs_ever_live[regno])
	  break;
      current_frame_info.n_input_regs = regno - IN_REG (0) + 1;
    }

  for (regno = OUT_REG (7); regno >= OUT_REG (0); regno--)
    if (regs_ever_live[regno])
      break;
  i = regno - OUT_REG (0) + 1;

  /* When -p profiling, we need one output register for the mcount argument.
     Likwise for -a profiling for the bb_init_func argument.  For -ax
     profiling, we need two output registers for the two bb_init_trace_func
     arguments.  */
  if (profile_flag || profile_block_flag == 1)
    i = MAX (i, 1);
  else if (profile_block_flag == 2)
    i = MAX (i, 2);
  current_frame_info.n_output_regs = i;

  /* ??? No rotating register support yet.  */
  current_frame_info.n_rotate_regs = 0;

  /* Discover which registers need spilling, and how much room that
     will take.  Begin with floating point and general registers, 
     which will always wind up on the stack.  */

  for (regno = FR_REG (2); regno <= FR_REG (127); regno++)
    if (regs_ever_live[regno] && ! call_used_regs[regno])
      {
	SET_HARD_REG_BIT (mask, regno);
	spill_size += 16;
	n_spilled += 1;
	spilled_fr_p = 1;
      }

  for (regno = GR_REG (1); regno <= GR_REG (31); regno++)
    if (regs_ever_live[regno] && ! call_used_regs[regno])
      {
	SET_HARD_REG_BIT (mask, regno);
	spill_size += 8;
	n_spilled += 1;
	spilled_gr_p = 1;
      }

  for (regno = BR_REG (1); regno <= BR_REG (7); regno++)
    if (regs_ever_live[regno] && ! call_used_regs[regno])
      {
	SET_HARD_REG_BIT (mask, regno);
	spill_size += 8;
	n_spilled += 1;
      }

  /* Now come all special registers that might get saved in other
     general registers.  */
  
  if (frame_pointer_needed)
    {
      current_frame_info.reg_fp = find_gr_spill (1);
      /* We should have gotten at least LOC79, since that's what
	 HARD_FRAME_POINTER_REGNUM is.  */
      if (current_frame_info.reg_fp == 0)
	abort ();
    }

  if (! current_function_is_leaf)
    {
      /* Emit a save of BR0 if we call other functions.  Do this even
	 if this function doesn't return, as EH depends on this to be
	 able to unwind the stack.  */
      SET_HARD_REG_BIT (mask, BR_REG (0));

      current_frame_info.reg_save_b0 = find_gr_spill (1);
      if (current_frame_info.reg_save_b0 == 0)
	{
	  spill_size += 8;
	  n_spilled += 1;
	}

      /* Similarly for ar.pfs.  */
      SET_HARD_REG_BIT (mask, AR_PFS_REGNUM);
      current_frame_info.reg_save_ar_pfs = find_gr_spill (1);
      if (current_frame_info.reg_save_ar_pfs == 0)
	{
	  extra_spill_size += 8;
	  n_spilled += 1;
	}
    }
  else
    {
      if (regs_ever_live[BR_REG (0)] && ! call_used_regs[BR_REG (0)])
	{
	  SET_HARD_REG_BIT (mask, BR_REG (0));
	  spill_size += 8;
	  n_spilled += 1;
	}
    }

  /* Unwind descriptor hackery: things are most efficient if we allocate
     consecutive GR save registers for RP, PFS, FP in that order. However,
     it is absolutely critical that FP get the only hard register that's
     guaranteed to be free, so we allocated it first.  If all three did
     happen to be allocated hard regs, and are consecutive, rearrange them
     into the preferred order now.  */
  if (current_frame_info.reg_fp != 0
      && current_frame_info.reg_save_b0 == current_frame_info.reg_fp + 1
      && current_frame_info.reg_save_ar_pfs == current_frame_info.reg_fp + 2)
    {
      current_frame_info.reg_save_b0 = current_frame_info.reg_fp;
      current_frame_info.reg_save_ar_pfs = current_frame_info.reg_fp + 1;
      current_frame_info.reg_fp = current_frame_info.reg_fp + 2;
    }

  /* See if we need to store the predicate register block.  */
  for (regno = PR_REG (0); regno <= PR_REG (63); regno++)
    if (regs_ever_live[regno] && ! call_used_regs[regno])
      break;
  if (regno <= PR_REG (63))
    {
      SET_HARD_REG_BIT (mask, PR_REG (0));
      current_frame_info.reg_save_pr = find_gr_spill (1);
      if (current_frame_info.reg_save_pr == 0)
	{
	  extra_spill_size += 8;
	  n_spilled += 1;
	}

      /* ??? Mark them all as used so that register renaming and such
	 are free to use them.  */
      for (regno = PR_REG (0); regno <= PR_REG (63); regno++)
	regs_ever_live[regno] = 1;
    }

  /* If we're forced to use st8.spill, we're forced to save and restore
     ar.unat as well.  */
  if (spilled_gr_p || cfun->machine->n_varargs)
    {
      SET_HARD_REG_BIT (mask, AR_UNAT_REGNUM);
      current_frame_info.reg_save_ar_unat = find_gr_spill (spill_size == 0);
      if (current_frame_info.reg_save_ar_unat == 0)
	{
	  extra_spill_size += 8;
	  n_spilled += 1;
	}
    }

  if (regs_ever_live[AR_LC_REGNUM])
    {
      SET_HARD_REG_BIT (mask, AR_LC_REGNUM);
      current_frame_info.reg_save_ar_lc = find_gr_spill (spill_size == 0);
      if (current_frame_info.reg_save_ar_lc == 0)
	{
	  extra_spill_size += 8;
	  n_spilled += 1;
	}
    }

  /* If we have an odd number of words of pretend arguments written to
     the stack, then the FR save area will be unaligned.  We round the
     size of this area up to keep things 16 byte aligned.  */
  if (spilled_fr_p)
    pretend_args_size = IA64_STACK_ALIGN (current_function_pretend_args_size);
  else
    pretend_args_size = current_function_pretend_args_size;

  total_size = (spill_size + extra_spill_size + size + pretend_args_size
		+ current_function_outgoing_args_size);
  total_size = IA64_STACK_ALIGN (total_size);

  /* We always use the 16-byte scratch area provided by the caller, but
     if we are a leaf function, there's no one to which we need to provide
     a scratch area.  */
  if (current_function_is_leaf)
    total_size = MAX (0, total_size - 16);

  current_frame_info.total_size = total_size;
  current_frame_info.spill_cfa_off = pretend_args_size - 16;
  current_frame_info.spill_size = spill_size;
  current_frame_info.extra_spill_size = extra_spill_size;
  COPY_HARD_REG_SET (current_frame_info.mask, mask);
  current_frame_info.n_spilled = n_spilled;
  current_frame_info.initialized = reload_completed;
}

/* Compute the initial difference between the specified pair of registers.  */

HOST_WIDE_INT
ia64_initial_elimination_offset (from, to)
     int from, to;
{
  HOST_WIDE_INT offset;

  ia64_compute_frame_size (get_frame_size ());
  switch (from)
    {
    case FRAME_POINTER_REGNUM:
      if (to == HARD_FRAME_POINTER_REGNUM)
	{
	  if (current_function_is_leaf)
	    offset = -current_frame_info.total_size;
	  else
	    offset = -(current_frame_info.total_size
		       - current_function_outgoing_args_size - 16);
	}
      else if (to == STACK_POINTER_REGNUM)
	{
	  if (current_function_is_leaf)
	    offset = 0;
	  else
	    offset = 16 + current_function_outgoing_args_size;
	}
      else
	abort ();
      break;

    case ARG_POINTER_REGNUM:
      /* Arguments start above the 16 byte save area, unless stdarg
	 in which case we store through the 16 byte save area.  */
      if (to == HARD_FRAME_POINTER_REGNUM)
	offset = 16 - current_function_pretend_args_size;
      else if (to == STACK_POINTER_REGNUM)
	offset = (current_frame_info.total_size
		  + 16 - current_function_pretend_args_size);
      else
	abort ();
      break;

    case RETURN_ADDRESS_POINTER_REGNUM:
      offset = 0;
      break;

    default:
      abort ();
    }

  return offset;
}

/* If there are more than a trivial number of register spills, we use
   two interleaved iterators so that we can get two memory references
   per insn group.

   In order to simplify things in the prologue and epilogue expanders,
   we use helper functions to fix up the memory references after the
   fact with the appropriate offsets to a POST_MODIFY memory mode.
   The following data structure tracks the state of the two iterators
   while insns are being emitted.  */

struct spill_fill_data
{
  rtx init_after;		/* point at which to emit intializations */
  rtx init_reg[2];		/* initial base register */
  rtx iter_reg[2];		/* the iterator registers */
  rtx *prev_addr[2];		/* address of last memory use */
  HOST_WIDE_INT prev_off[2];	/* last offset */
  int n_iter;			/* number of iterators in use */
  int next_iter;		/* next iterator to use */
  unsigned int save_gr_used_mask;
};

static struct spill_fill_data spill_fill_data;

static void
setup_spill_pointers (n_spills, init_reg, cfa_off)
     int n_spills;
     rtx init_reg;
     HOST_WIDE_INT cfa_off;
{
  int i;

  spill_fill_data.init_after = get_last_insn ();
  spill_fill_data.init_reg[0] = init_reg;
  spill_fill_data.init_reg[1] = init_reg;
  spill_fill_data.prev_addr[0] = NULL;
  spill_fill_data.prev_addr[1] = NULL;
  spill_fill_data.prev_off[0] = cfa_off;
  spill_fill_data.prev_off[1] = cfa_off;
  spill_fill_data.next_iter = 0;
  spill_fill_data.save_gr_used_mask = current_frame_info.gr_used_mask;

  spill_fill_data.n_iter = 1 + (n_spills > 2);
  for (i = 0; i < spill_fill_data.n_iter; ++i)
    {
      int regno = next_scratch_gr_reg ();
      spill_fill_data.iter_reg[i] = gen_rtx_REG (DImode, regno);
      current_frame_info.gr_used_mask |= 1 << regno;
    }
}

static void
finish_spill_pointers ()
{
  current_frame_info.gr_used_mask = spill_fill_data.save_gr_used_mask;
}

static rtx
spill_restore_mem (reg, cfa_off)
     rtx reg;
     HOST_WIDE_INT cfa_off;
{
  int iter = spill_fill_data.next_iter;
  HOST_WIDE_INT disp = spill_fill_data.prev_off[iter] - cfa_off;
  rtx disp_rtx = GEN_INT (disp);
  rtx mem;

  if (spill_fill_data.prev_addr[iter])
    {
      if (CONST_OK_FOR_N (disp))
	*spill_fill_data.prev_addr[iter]
	  = gen_rtx_POST_MODIFY (DImode, spill_fill_data.iter_reg[iter],
				 gen_rtx_PLUS (DImode,
					       spill_fill_data.iter_reg[iter],
					       disp_rtx));
      else
	{
	  /* ??? Could use register post_modify for loads.  */
	  if (! CONST_OK_FOR_I (disp))
	    {
	      rtx tmp = gen_rtx_REG (DImode, next_scratch_gr_reg ());
	      emit_move_insn (tmp, disp_rtx);
	      disp_rtx = tmp;
	    }
	  emit_insn (gen_adddi3 (spill_fill_data.iter_reg[iter],
				 spill_fill_data.iter_reg[iter], disp_rtx));
	}
    }
  /* Micro-optimization: if we've created a frame pointer, it's at
     CFA 0, which may allow the real iterator to be initialized lower,
     slightly increasing parallelism.  Also, if there are few saves
     it may eliminate the iterator entirely.  */
  else if (disp == 0
	   && spill_fill_data.init_reg[iter] == stack_pointer_rtx
	   && frame_pointer_needed)
    {
      mem = gen_rtx_MEM (GET_MODE (reg), hard_frame_pointer_rtx);
      MEM_ALIAS_SET (mem) = get_varargs_alias_set ();
      return mem;
    }
  else
    {
      rtx seq;

      if (disp == 0)
	seq = gen_movdi (spill_fill_data.iter_reg[iter],
			 spill_fill_data.init_reg[iter]);
      else
	{
	  start_sequence ();

	  if (! CONST_OK_FOR_I (disp))
	    {
	      rtx tmp = gen_rtx_REG (DImode, next_scratch_gr_reg ());
	      emit_move_insn (tmp, disp_rtx);
	      disp_rtx = tmp;
	    }

	  emit_insn (gen_adddi3 (spill_fill_data.iter_reg[iter],
				 spill_fill_data.init_reg[iter],
				 disp_rtx));

	  seq = gen_sequence ();
	  end_sequence ();
	}

      /* Careful for being the first insn in a sequence.  */
      if (spill_fill_data.init_after)
	spill_fill_data.init_after
	  = emit_insn_after (seq, spill_fill_data.init_after);
      else
	{
	  rtx first = get_insns ();
	  if (first)
	    spill_fill_data.init_after
	      = emit_insn_before (seq, first);
	  else
	    spill_fill_data.init_after = emit_insn (seq);
	}
    }

  mem = gen_rtx_MEM (GET_MODE (reg), spill_fill_data.iter_reg[iter]);

  /* ??? Not all of the spills are for varargs, but some of them are.
     The rest of the spills belong in an alias set of their own.  But
     it doesn't actually hurt to include them here.  */
  MEM_ALIAS_SET (mem) = get_varargs_alias_set ();

  spill_fill_data.prev_addr[iter] = &XEXP (mem, 0);
  spill_fill_data.prev_off[iter] = cfa_off;

  if (++iter >= spill_fill_data.n_iter)
    iter = 0;
  spill_fill_data.next_iter = iter;

  return mem;
}

static void
do_spill (move_fn, reg, cfa_off, frame_reg)
     rtx (*move_fn) PARAMS ((rtx, rtx, rtx));
     rtx reg, frame_reg;
     HOST_WIDE_INT cfa_off;
{
  rtx mem, insn;

  mem = spill_restore_mem (reg, cfa_off);
  insn = emit_insn ((*move_fn) (mem, reg, GEN_INT (cfa_off)));

  if (frame_reg)
    {
      rtx base;
      HOST_WIDE_INT off;

      RTX_FRAME_RELATED_P (insn) = 1;

      /* Don't even pretend that the unwind code can intuit its way 
	 through a pair of interleaved post_modify iterators.  Just
	 provide the correct answer.  */

      if (frame_pointer_needed)
	{
	  base = hard_frame_pointer_rtx;
	  off = - cfa_off;
	}
      else
	{
	  base = stack_pointer_rtx;
	  off = current_frame_info.total_size - cfa_off;
	}

      REG_NOTES (insn)
	= gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR,
		gen_rtx_SET (VOIDmode,
			     gen_rtx_MEM (GET_MODE (reg),
					  plus_constant (base, off)),
			     frame_reg),
		REG_NOTES (insn));
    }
}

static void
do_restore (move_fn, reg, cfa_off)
     rtx (*move_fn) PARAMS ((rtx, rtx, rtx));
     rtx reg;
     HOST_WIDE_INT cfa_off;
{
  emit_insn ((*move_fn) (reg, spill_restore_mem (reg, cfa_off),
			 GEN_INT (cfa_off)));
}

/* Wrapper functions that discards the CONST_INT spill offset.  These
   exist so that we can give gr_spill/gr_fill the offset they need and
   use a consistant function interface.  */

static rtx
gen_movdi_x (dest, src, offset)
     rtx dest, src;
     rtx offset ATTRIBUTE_UNUSED;
{
  return gen_movdi (dest, src);
}

static rtx
gen_fr_spill_x (dest, src, offset)
     rtx dest, src;
     rtx offset ATTRIBUTE_UNUSED;
{
  return gen_fr_spill (dest, src);
}

static rtx
gen_fr_restore_x (dest, src, offset)
     rtx dest, src;
     rtx offset ATTRIBUTE_UNUSED;
{
  return gen_fr_restore (dest, src);
}

/* Called after register allocation to add any instructions needed for the
   prologue.  Using a prologue insn is favored compared to putting all of the
   instructions in the FUNCTION_PROLOGUE macro, since it allows the scheduler
   to intermix instructions with the saves of the caller saved registers.  In
   some cases, it might be necessary to emit a barrier instruction as the last
   insn to prevent such scheduling.

   Also any insns generated here should have RTX_FRAME_RELATED_P(insn) = 1
   so that the debug info generation code can handle them properly.

   The register save area is layed out like so:
   cfa+16
	[ varargs spill area ]
	[ fr register spill area ]
	[ br register spill area ]
	[ ar register spill area ]
	[ pr register spill area ]
	[ gr register spill area ] */

/* ??? Get inefficient code when the frame size is larger than can fit in an
   adds instruction.  */

void
ia64_expand_prologue ()
{
  rtx insn, ar_pfs_save_reg, ar_unat_save_reg;
  int i, epilogue_p, regno, alt_regno, cfa_off, n_varargs;
  rtx reg, alt_reg;

  ia64_compute_frame_size (get_frame_size ());
  last_scratch_gr_reg = 15;

  /* If there is no epilogue, then we don't need some prologue insns.
     We need to avoid emitting the dead prologue insns, because flow
     will complain about them.  */
  if (optimize)
    {
      edge e;

      for (e = EXIT_BLOCK_PTR->pred; e ; e = e->pred_next)
	if ((e->flags & EDGE_FAKE) == 0
	    && (e->flags & EDGE_FALLTHRU) != 0)
	  break;
      epilogue_p = (e != NULL);
    }
  else
    epilogue_p = 1;

  /* Set the local, input, and output register names.  We need to do this
     for GNU libc, which creates crti.S/crtn.S by splitting initfini.c in
     half.  If we use in/loc/out register names, then we get assembler errors
     in crtn.S because there is no alloc insn or regstk directive in there.  */
  if (! TARGET_REG_NAMES)
    {
      int inputs = current_frame_info.n_input_regs;
      int locals = current_frame_info.n_local_regs;
      int outputs = current_frame_info.n_output_regs;

      for (i = 0; i < inputs; i++)
	reg_names[IN_REG (i)] = ia64_reg_numbers[i];
      for (i = 0; i < locals; i++)
	reg_names[LOC_REG (i)] = ia64_reg_numbers[inputs + i];
      for (i = 0; i < outputs; i++)
	reg_names[OUT_REG (i)] = ia64_reg_numbers[inputs + locals + i];
    }

  /* Set the frame pointer register name.  The regnum is logically loc79,
     but of course we'll not have allocated that many locals.  Rather than
     worrying about renumbering the existing rtxs, we adjust the name.  */
  if (current_frame_info.reg_fp)
    {
      const char *tmp = reg_names[HARD_FRAME_POINTER_REGNUM];
      reg_names[HARD_FRAME_POINTER_REGNUM]
	= reg_names[current_frame_info.reg_fp];
      reg_names[current_frame_info.reg_fp] = tmp;
    }

  /* Fix up the return address placeholder.  */
  /* ??? We can fail if __builtin_return_address is used, and we didn't
     allocate a register in which to save b0.  I can't think of a way to
     eliminate RETURN_ADDRESS_POINTER_REGNUM to a local register and
     then be sure that I got the right one.  Further, reload doesn't seem
     to care if an eliminable register isn't used, and "eliminates" it
     anyway.  */
  if (regs_ever_live[RETURN_ADDRESS_POINTER_REGNUM]
      && current_frame_info.reg_save_b0 != 0)
    XINT (return_address_pointer_rtx, 0) = current_frame_info.reg_save_b0;

  /* We don't need an alloc instruction if we've used no outputs or locals.  */
  if (current_frame_info.n_local_regs == 0
      && current_frame_info.n_output_regs == 0)
    {
      /* If there is no alloc, but there are input registers used, then we
	 need a .regstk directive.  */
      current_frame_info.need_regstk = (TARGET_REG_NAMES != 0);
      ar_pfs_save_reg = NULL_RTX;
    }
  else
    {
      current_frame_info.need_regstk = 0;

      if (current_frame_info.reg_save_ar_pfs)
	regno = current_frame_info.reg_save_ar_pfs;
      else
	regno = next_scratch_gr_reg ();
      ar_pfs_save_reg = gen_rtx_REG (DImode, regno);

      insn = emit_insn (gen_alloc (ar_pfs_save_reg, 
				   GEN_INT (current_frame_info.n_input_regs),
				   GEN_INT (current_frame_info.n_local_regs),
				   GEN_INT (current_frame_info.n_output_regs),
				   GEN_INT (current_frame_info.n_rotate_regs)));
      RTX_FRAME_RELATED_P (insn) = (current_frame_info.reg_save_ar_pfs != 0);
    }

  /* Set up frame pointer, stack pointer, and spill iterators.  */

  n_varargs = cfun->machine->n_varargs;
  setup_spill_pointers (current_frame_info.n_spilled + n_varargs,
			stack_pointer_rtx, 0);

  if (frame_pointer_needed)
    {
      insn = emit_move_insn (hard_frame_pointer_rtx, stack_pointer_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  if (current_frame_info.total_size != 0)
    {
      rtx frame_size_rtx = GEN_INT (- current_frame_info.total_size);
      rtx offset;

      if (CONST_OK_FOR_I (- current_frame_info.total_size))
	offset = frame_size_rtx;
      else
	{
	  regno = next_scratch_gr_reg ();
 	  offset = gen_rtx_REG (DImode, regno);
	  emit_move_insn (offset, frame_size_rtx);
	}

      insn = emit_insn (gen_adddi3 (stack_pointer_rtx,
				    stack_pointer_rtx, offset));

      if (! frame_pointer_needed)
	{
	  RTX_FRAME_RELATED_P (insn) = 1;
	  if (GET_CODE (offset) != CONST_INT)
	    {
	      REG_NOTES (insn)
		= gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR,
			gen_rtx_SET (VOIDmode,
				     stack_pointer_rtx,
				     gen_rtx_PLUS (DImode,
						   stack_pointer_rtx,
						   frame_size_rtx)),
			REG_NOTES (insn));
	    }
	}

      /* ??? At this point we must generate a magic insn that appears to
	 modify the stack pointer, the frame pointer, and all spill
	 iterators.  This would allow the most scheduling freedom.  For
	 now, just hard stop.  */
      emit_insn (gen_blockage ());
    }

  /* Must copy out ar.unat before doing any integer spills.  */
  if (TEST_HARD_REG_BIT (current_frame_info.mask, AR_UNAT_REGNUM))
    {
      if (current_frame_info.reg_save_ar_unat)
	ar_unat_save_reg
	  = gen_rtx_REG (DImode, current_frame_info.reg_save_ar_unat);
      else
	{
	  alt_regno = next_scratch_gr_reg ();
	  ar_unat_save_reg = gen_rtx_REG (DImode, alt_regno);
	  current_frame_info.gr_used_mask |= 1 << alt_regno;
	}

      reg = gen_rtx_REG (DImode, AR_UNAT_REGNUM);
      insn = emit_move_insn (ar_unat_save_reg, reg);
      RTX_FRAME_RELATED_P (insn) = (current_frame_info.reg_save_ar_unat != 0);

      /* Even if we're not going to generate an epilogue, we still
	 need to save the register so that EH works.  */
      if (! epilogue_p && current_frame_info.reg_save_ar_unat)
	emit_insn (gen_rtx_USE (VOIDmode, ar_unat_save_reg));
    }
  else
    ar_unat_save_reg = NULL_RTX;

  /* Spill all varargs registers.  Do this before spilling any GR registers,
     since we want the UNAT bits for the GR registers to override the UNAT
     bits from varargs, which we don't care about.  */

  cfa_off = -16;
  for (regno = GR_ARG_FIRST + 7; n_varargs > 0; --n_varargs, --regno)
    {
      reg = gen_rtx_REG (DImode, regno);
      do_spill (gen_gr_spill, reg, cfa_off += 8, NULL_RTX);
    }

  /* Locate the bottom of the register save area.  */
  cfa_off = (current_frame_info.spill_cfa_off
	     + current_frame_info.spill_size
	     + current_frame_info.extra_spill_size);

  /* Save the predicate register block either in a register or in memory.  */
  if (TEST_HARD_REG_BIT (current_frame_info.mask, PR_REG (0)))
    {
      reg = gen_rtx_REG (DImode, PR_REG (0));
      if (current_frame_info.reg_save_pr != 0)
	{
	  alt_reg = gen_rtx_REG (DImode, current_frame_info.reg_save_pr);
	  insn = emit_move_insn (alt_reg, reg);

	  /* ??? Denote pr spill/fill by a DImode move that modifies all
	     64 hard registers.  */
	  RTX_FRAME_RELATED_P (insn) = 1;
	  REG_NOTES (insn)
	    = gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR,
			gen_rtx_SET (VOIDmode, alt_reg, reg),
			REG_NOTES (insn));

	  /* Even if we're not going to generate an epilogue, we still
	     need to save the register so that EH works.  */
	  if (! epilogue_p)
	    emit_insn (gen_rtx_USE (VOIDmode, alt_reg));
	}
      else
	{
	  alt_regno = next_scratch_gr_reg ();
	  alt_reg = gen_rtx_REG (DImode, alt_regno);
	  insn = emit_move_insn (alt_reg, reg);
	  do_spill (gen_movdi_x, alt_reg, cfa_off, reg);
	  cfa_off -= 8;
	}
    }

  /* Handle AR regs in numerical order.  All of them get special handling.  */
  if (TEST_HARD_REG_BIT (current_frame_info.mask, AR_UNAT_REGNUM)
      && current_frame_info.reg_save_ar_unat == 0)
    {
      reg = gen_rtx_REG (DImode, AR_UNAT_REGNUM);
      do_spill (gen_movdi_x, ar_unat_save_reg, cfa_off, reg);
      cfa_off -= 8;
    }

  /* The alloc insn already copied ar.pfs into a general register.  The
     only thing we have to do now is copy that register to a stack slot
     if we'd not allocated a local register for the job.  */
  if (current_frame_info.reg_save_ar_pfs == 0
      && ! current_function_is_leaf)
    {
      reg = gen_rtx_REG (DImode, AR_PFS_REGNUM);
      do_spill (gen_movdi_x, ar_pfs_save_reg, cfa_off, reg);
      cfa_off -= 8;
    }

  if (TEST_HARD_REG_BIT (current_frame_info.mask, AR_LC_REGNUM))
    {
      reg = gen_rtx_REG (DImode, AR_LC_REGNUM);
      if (current_frame_info.reg_save_ar_lc != 0)
	{
	  alt_reg = gen_rtx_REG (DImode, current_frame_info.reg_save_ar_lc);
	  insn = emit_move_insn (alt_reg, reg);
	  RTX_FRAME_RELATED_P (insn) = 1;

	  /* Even if we're not going to generate an epilogue, we still
	     need to save the register so that EH works.  */
	  if (! epilogue_p)
	    emit_insn (gen_rtx_USE (VOIDmode, alt_reg));
	}
      else
	{
	  alt_regno = next_scratch_gr_reg ();
	  alt_reg = gen_rtx_REG (DImode, alt_regno);
	  emit_move_insn (alt_reg, reg);
	  do_spill (gen_movdi_x, alt_reg, cfa_off, reg);
	  cfa_off -= 8;
	}
    }

  /* We should now be at the base of the gr/br/fr spill area.  */
  if (cfa_off != (current_frame_info.spill_cfa_off
		  + current_frame_info.spill_size))
    abort ();

  /* Spill all general registers.  */
  for (regno = GR_REG (1); regno <= GR_REG (31); ++regno)
    if (TEST_HARD_REG_BIT (current_frame_info.mask, regno))
      {
	reg = gen_rtx_REG (DImode, regno);
	do_spill (gen_gr_spill, reg, cfa_off, reg);
	cfa_off -= 8;
      }

  /* Handle BR0 specially -- it may be getting stored permanently in
     some GR register.  */
  if (TEST_HARD_REG_BIT (current_frame_info.mask, BR_REG (0)))
    {
      reg = gen_rtx_REG (DImode, BR_REG (0));
      if (current_frame_info.reg_save_b0 != 0)
	{
	  alt_reg = gen_rtx_REG (DImode, current_frame_info.reg_save_b0);
	  insn = emit_move_insn (alt_reg, reg);
	  RTX_FRAME_RELATED_P (insn) = 1;

	  /* Even if we're not going to generate an epilogue, we still
	     need to save the register so that EH works.  */
	  if (! epilogue_p)
	    emit_insn (gen_rtx_USE (VOIDmode, alt_reg));
	}
      else
	{
	  alt_regno = next_scratch_gr_reg ();
	  alt_reg = gen_rtx_REG (DImode, alt_regno);
	  emit_move_insn (alt_reg, reg);
	  do_spill (gen_movdi_x, alt_reg, cfa_off, reg);
	  cfa_off -= 8;
	}
    }

  /* Spill the rest of the BR registers.  */
  for (regno = BR_REG (1); regno <= BR_REG (7); ++regno)
    if (TEST_HARD_REG_BIT (current_frame_info.mask, regno))
      {
	alt_regno = next_scratch_gr_reg ();
	alt_reg = gen_rtx_REG (DImode, alt_regno);
	reg = gen_rtx_REG (DImode, regno);
	emit_move_insn (alt_reg, reg);
	do_spill (gen_movdi_x, alt_reg, cfa_off, reg);
	cfa_off -= 8;
      }

  /* Align the frame and spill all FR registers.  */
  for (regno = FR_REG (2); regno <= FR_REG (127); ++regno)
    if (TEST_HARD_REG_BIT (current_frame_info.mask, regno))
      {
        if (cfa_off & 15)
	  abort ();
	reg = gen_rtx_REG (TFmode, regno);
	do_spill (gen_fr_spill_x, reg, cfa_off, reg);
	cfa_off -= 16;
      }

  if (cfa_off != current_frame_info.spill_cfa_off)
    abort ();

  finish_spill_pointers ();
}

/* Called after register allocation to add any instructions needed for the
   epilogue.  Using a epilogue insn is favored compared to putting all of the
   instructions in the FUNCTION_PROLOGUE macro, since it allows the scheduler
   to intermix instructions with the saves of the caller saved registers.  In
   some cases, it might be necessary to emit a barrier instruction as the last
   insn to prevent such scheduling.  */

void
ia64_expand_epilogue ()
{
  rtx insn, reg, alt_reg, ar_unat_save_reg;
  int regno, alt_regno, cfa_off;

  ia64_compute_frame_size (get_frame_size ());

  /* If there is a frame pointer, then we use it instead of the stack
     pointer, so that the stack pointer does not need to be valid when
     the epilogue starts.  See EXIT_IGNORE_STACK.  */
  if (frame_pointer_needed)
    setup_spill_pointers (current_frame_info.n_spilled,
			  hard_frame_pointer_rtx, 0);
  else
    setup_spill_pointers (current_frame_info.n_spilled, stack_pointer_rtx, 
			  current_frame_info.total_size);

  if (current_frame_info.total_size != 0)
    {
      /* ??? At this point we must generate a magic insn that appears to
         modify the spill iterators and the frame pointer.  This would
	 allow the most scheduling freedom.  For now, just hard stop.  */
      emit_insn (gen_blockage ());
    }

  /* Locate the bottom of the register save area.  */
  cfa_off = (current_frame_info.spill_cfa_off
	     + current_frame_info.spill_size
	     + current_frame_info.extra_spill_size);

  /* Restore the predicate registers.  */
  if (TEST_HARD_REG_BIT (current_frame_info.mask, PR_REG (0)))
    {
      if (current_frame_info.reg_save_pr != 0)
	alt_reg = gen_rtx_REG (DImode, current_frame_info.reg_save_pr);
      else
	{
	  alt_regno = next_scratch_gr_reg ();
	  alt_reg = gen_rtx_REG (DImode, alt_regno);
	  do_restore (gen_movdi_x, alt_reg, cfa_off);
	  cfa_off -= 8;
	}
      reg = gen_rtx_REG (DImode, PR_REG (0));
      emit_move_insn (reg, alt_reg);
    }

  /* Restore the application registers.  */

  /* Load the saved unat from the stack, but do not restore it until
     after the GRs have been restored.  */
  if (TEST_HARD_REG_BIT (current_frame_info.mask, AR_UNAT_REGNUM))
    {
      if (current_frame_info.reg_save_ar_unat != 0)
        ar_unat_save_reg
	  = gen_rtx_REG (DImode, current_frame_info.reg_save_ar_unat);
      else
	{
	  alt_regno = next_scratch_gr_reg ();
	  ar_unat_save_reg = gen_rtx_REG (DImode, alt_regno);
	  current_frame_info.gr_used_mask |= 1 << alt_regno;
	  do_restore (gen_movdi_x, ar_unat_save_reg, cfa_off);
	  cfa_off -= 8;
	}
    }
  else
    ar_unat_save_reg = NULL_RTX;
      
  if (current_frame_info.reg_save_ar_pfs != 0)
    {
      alt_reg = gen_rtx_REG (DImode, current_frame_info.reg_save_ar_pfs);
      reg = gen_rtx_REG (DImode, AR_PFS_REGNUM);
      emit_move_insn (reg, alt_reg);
    }
  else if (! current_function_is_leaf)
    {
      alt_regno = next_scratch_gr_reg ();
      alt_reg = gen_rtx_REG (DImode, alt_regno);
      do_restore (gen_movdi_x, alt_reg, cfa_off);
      cfa_off -= 8;
      reg = gen_rtx_REG (DImode, AR_PFS_REGNUM);
      emit_move_insn (reg, alt_reg);
    }

  if (TEST_HARD_REG_BIT (current_frame_info.mask, AR_LC_REGNUM))
    {
      if (current_frame_info.reg_save_ar_lc != 0)
	alt_reg = gen_rtx_REG (DImode, current_frame_info.reg_save_ar_lc);
      else
	{
	  alt_regno = next_scratch_gr_reg ();
	  alt_reg = gen_rtx_REG (DImode, alt_regno);
	  do_restore (gen_movdi_x, alt_reg, cfa_off);
	  cfa_off -= 8;
	}
      reg = gen_rtx_REG (DImode, AR_LC_REGNUM);
      emit_move_insn (reg, alt_reg);
    }

  /* We should now be at the base of the gr/br/fr spill area.  */
  if (cfa_off != (current_frame_info.spill_cfa_off
		  + current_frame_info.spill_size))
    abort ();

  /* Restore all general registers.  */
  for (regno = GR_REG (1); regno <= GR_REG (31); ++regno)
    if (TEST_HARD_REG_BIT (current_frame_info.mask, regno))
      {
	reg = gen_rtx_REG (DImode, regno);
	do_restore (gen_gr_restore, reg, cfa_off);
	cfa_off -= 8;
      }
  
  /* Restore the branch registers.  Handle B0 specially, as it may
     have gotten stored in some GR register.  */
  if (TEST_HARD_REG_BIT (current_frame_info.mask, BR_REG (0)))
    {
      if (current_frame_info.reg_save_b0 != 0)
	alt_reg = gen_rtx_REG (DImode, current_frame_info.reg_save_b0);
      else
	{
	  alt_regno = next_scratch_gr_reg ();
	  alt_reg = gen_rtx_REG (DImode, alt_regno);
	  do_restore (gen_movdi_x, alt_reg, cfa_off);
	  cfa_off -= 8;
	}
      reg = gen_rtx_REG (DImode, BR_REG (0));
      emit_move_insn (reg, alt_reg);
    }
    
  for (regno = BR_REG (1); regno <= BR_REG (7); ++regno)
    if (TEST_HARD_REG_BIT (current_frame_info.mask, regno))
      {
	alt_regno = next_scratch_gr_reg ();
	alt_reg = gen_rtx_REG (DImode, alt_regno);
	do_restore (gen_movdi_x, alt_reg, cfa_off);
	cfa_off -= 8;
	reg = gen_rtx_REG (DImode, regno);
	emit_move_insn (reg, alt_reg);
      }

  /* Restore floating point registers.  */
  for (regno = FR_REG (2); regno <= FR_REG (127); ++regno)
    if (TEST_HARD_REG_BIT (current_frame_info.mask, regno))
      {
        if (cfa_off & 15)
	  abort ();
	reg = gen_rtx_REG (TFmode, regno);
	do_restore (gen_fr_restore_x, reg, cfa_off);
	cfa_off -= 16;
      }

  /* Restore ar.unat for real.  */
  if (TEST_HARD_REG_BIT (current_frame_info.mask, AR_UNAT_REGNUM))
    {
      reg = gen_rtx_REG (DImode, AR_UNAT_REGNUM);
      emit_move_insn (reg, ar_unat_save_reg);
    }

  if (cfa_off != current_frame_info.spill_cfa_off)
    abort ();

  finish_spill_pointers ();

  if (current_frame_info.total_size || cfun->machine->ia64_eh_epilogue_sp)
    {
      /* ??? At this point we must generate a magic insn that appears to
         modify the spill iterators, the stack pointer, and the frame
	 pointer.  This would allow the most scheduling freedom.  For now,
	 just hard stop.  */
      emit_insn (gen_blockage ());
    }

  if (cfun->machine->ia64_eh_epilogue_sp)
    emit_move_insn (stack_pointer_rtx, cfun->machine->ia64_eh_epilogue_sp);
  else if (frame_pointer_needed)
    {
      insn = emit_move_insn (stack_pointer_rtx, hard_frame_pointer_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;
    }
  else if (current_frame_info.total_size)
    {
      rtx offset, frame_size_rtx;

      frame_size_rtx = GEN_INT (current_frame_info.total_size);
      if (CONST_OK_FOR_I (current_frame_info.total_size))
	offset = frame_size_rtx;
      else
	{
	  regno = next_scratch_gr_reg ();
	  offset = gen_rtx_REG (DImode, regno);
	  emit_move_insn (offset, frame_size_rtx);
	}

      insn = emit_insn (gen_adddi3 (stack_pointer_rtx, stack_pointer_rtx,
				    offset));

      RTX_FRAME_RELATED_P (insn) = 1;
      if (GET_CODE (offset) != CONST_INT)
	{
	  REG_NOTES (insn)
	    = gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR,
			gen_rtx_SET (VOIDmode,
				     stack_pointer_rtx,
				     gen_rtx_PLUS (DImode,
						   stack_pointer_rtx,
						   frame_size_rtx)),
			REG_NOTES (insn));
	}
    }

  if (cfun->machine->ia64_eh_epilogue_bsp)
    emit_insn (gen_set_bsp (cfun->machine->ia64_eh_epilogue_bsp));
 
  emit_jump_insn (gen_return_internal (gen_rtx_REG (DImode, BR_REG (0))));
}

/* Return 1 if br.ret can do all the work required to return from a
   function.  */

int
ia64_direct_return ()
{
  if (reload_completed && ! frame_pointer_needed)
    {
      ia64_compute_frame_size (get_frame_size ());

      return (current_frame_info.total_size == 0
	      && current_frame_info.n_spilled == 0
	      && current_frame_info.reg_save_b0 == 0
	      && current_frame_info.reg_save_pr == 0
	      && current_frame_info.reg_save_ar_pfs == 0
	      && current_frame_info.reg_save_ar_unat == 0
	      && current_frame_info.reg_save_ar_lc == 0);
    }
  return 0;
}

/* Emit the function prologue.  */

void
ia64_function_prologue (file, size)
     FILE *file;
     int size ATTRIBUTE_UNUSED;
{
  int mask, grsave, grsave_prev;

  if (current_frame_info.need_regstk)
    fprintf (file, "\t.regstk %d, %d, %d, %d\n",
	     current_frame_info.n_input_regs,
	     current_frame_info.n_local_regs,
	     current_frame_info.n_output_regs,
	     current_frame_info.n_rotate_regs);

  if (!flag_unwind_tables && (!flag_exceptions || exceptions_via_longjmp))
    return;

  /* Emit the .prologue directive.  */

  mask = 0;
  grsave = grsave_prev = 0;
  if (current_frame_info.reg_save_b0 != 0)
    {
      mask |= 8;
      grsave = grsave_prev = current_frame_info.reg_save_b0;
    }
  if (current_frame_info.reg_save_ar_pfs != 0
      && (grsave_prev == 0
	  || current_frame_info.reg_save_ar_pfs == grsave_prev + 1))
    {
      mask |= 4;
      if (grsave_prev == 0)
	grsave = current_frame_info.reg_save_ar_pfs;
      grsave_prev = current_frame_info.reg_save_ar_pfs;
    }
  if (current_frame_info.reg_fp != 0
      && (grsave_prev == 0
	  || current_frame_info.reg_fp == grsave_prev + 1))
    {
      mask |= 2;
      if (grsave_prev == 0)
	grsave = HARD_FRAME_POINTER_REGNUM;
      grsave_prev = current_frame_info.reg_fp;
    }
  if (current_frame_info.reg_save_pr != 0
      && (grsave_prev == 0
	  || current_frame_info.reg_save_pr == grsave_prev + 1))
    {
      mask |= 1;
      if (grsave_prev == 0)
	grsave = current_frame_info.reg_save_pr;
    }

  if (mask)
    fprintf (file, "\t.prologue %d, %d\n", mask,
	     ia64_dbx_register_number (grsave));
  else
    fputs ("\t.prologue\n", file);

  /* Emit a .spill directive, if necessary, to relocate the base of
     the register spill area.  */
  if (current_frame_info.spill_cfa_off != -16)
    fprintf (file, "\t.spill %ld\n",
	     (long) (current_frame_info.spill_cfa_off
		     + current_frame_info.spill_size));
}

/* Emit the .body directive at the scheduled end of the prologue.  */

void
ia64_output_end_prologue (file)
     FILE *file;
{
  if (!flag_unwind_tables && (!flag_exceptions || exceptions_via_longjmp))
    return;

  fputs ("\t.body\n", file);
}

/* Emit the function epilogue.  */

void
ia64_function_epilogue (file, size)
     FILE *file ATTRIBUTE_UNUSED;
     int size ATTRIBUTE_UNUSED;
{
  /* Reset from the function's potential modifications.  */
  XINT (return_address_pointer_rtx, 0) = RETURN_ADDRESS_POINTER_REGNUM;

  if (current_frame_info.reg_fp)
    {
      const char *tmp = reg_names[HARD_FRAME_POINTER_REGNUM];
      reg_names[HARD_FRAME_POINTER_REGNUM]
	= reg_names[current_frame_info.reg_fp];
      reg_names[current_frame_info.reg_fp] = tmp;
    }
  if (! TARGET_REG_NAMES)
    {
      int i;

      for (i = 0; i < current_frame_info.n_input_regs; i++)
	reg_names[IN_REG (i)] = ia64_input_reg_names[i];
      for (i = 0; i < current_frame_info.n_local_regs; i++)
	reg_names[LOC_REG (i)] = ia64_local_reg_names[i];
      for (i = 0; i < current_frame_info.n_output_regs; i++)
	reg_names[OUT_REG (i)] = ia64_output_reg_names[i];
    }
  current_frame_info.initialized = 0;
}

int
ia64_dbx_register_number (regno)
     int regno;
{
  /* In ia64_expand_prologue we quite literally renamed the frame pointer
     from its home at loc79 to something inside the register frame.  We
     must perform the same renumbering here for the debug info.  */
  if (current_frame_info.reg_fp)
    {
      if (regno == HARD_FRAME_POINTER_REGNUM)
	regno = current_frame_info.reg_fp;
      else if (regno == current_frame_info.reg_fp)
	regno = HARD_FRAME_POINTER_REGNUM;
    }

  if (IN_REGNO_P (regno))
    return 32 + regno - IN_REG (0);
  else if (LOC_REGNO_P (regno))
    return 32 + current_frame_info.n_input_regs + regno - LOC_REG (0);
  else if (OUT_REGNO_P (regno))
    return (32 + current_frame_info.n_input_regs
	    + current_frame_info.n_local_regs + regno - OUT_REG (0));
  else
    return regno;
}

void
ia64_initialize_trampoline (addr, fnaddr, static_chain)
     rtx addr, fnaddr, static_chain;
{
  rtx addr_reg, eight = GEN_INT (8);

  /* Load up our iterator.  */
  addr_reg = gen_reg_rtx (Pmode);
  emit_move_insn (addr_reg, addr);

  /* The first two words are the fake descriptor:
     __ia64_trampoline, ADDR+16.  */
  emit_move_insn (gen_rtx_MEM (Pmode, addr_reg),
		  gen_rtx_SYMBOL_REF (Pmode, "__ia64_trampoline"));
  emit_insn (gen_adddi3 (addr_reg, addr_reg, eight));

  emit_move_insn (gen_rtx_MEM (Pmode, addr_reg),
		  copy_to_reg (plus_constant (addr, 16)));
  emit_insn (gen_adddi3 (addr_reg, addr_reg, eight));

  /* The third word is the target descriptor.  */
  emit_move_insn (gen_rtx_MEM (Pmode, addr_reg), fnaddr);
  emit_insn (gen_adddi3 (addr_reg, addr_reg, eight));

  /* The fourth word is the static chain.  */
  emit_move_insn (gen_rtx_MEM (Pmode, addr_reg), static_chain);
}

/* Do any needed setup for a variadic function.  CUM has not been updated
   for the last named argument which has type TYPE and mode MODE.

   We generate the actual spill instructions during prologue generation.  */

void
ia64_setup_incoming_varargs (cum, int_mode, type, pretend_size, second_time)
     CUMULATIVE_ARGS cum;
     int             int_mode;
     tree            type;
     int *           pretend_size;
     int	     second_time ATTRIBUTE_UNUSED;
{
  /* If this is a stdarg function, then skip the current argument.  */
  if (! current_function_varargs)
    ia64_function_arg_advance (&cum, int_mode, type, 1);

  if (cum.words < MAX_ARGUMENT_SLOTS)
    {
      int n = MAX_ARGUMENT_SLOTS - cum.words;
      *pretend_size = n * UNITS_PER_WORD;
      cfun->machine->n_varargs = n;
    }
}

/* Check whether TYPE is a homogeneous floating point aggregate.  If
   it is, return the mode of the floating point type that appears
   in all leafs.  If it is not, return VOIDmode.

   An aggregate is a homogeneous floating point aggregate is if all
   fields/elements in it have the same floating point type (e.g,
   SFmode).  128-bit quad-precision floats are excluded.  */

static enum machine_mode
hfa_element_mode (type, nested)
     tree type;
     int nested;
{
  enum machine_mode element_mode = VOIDmode;
  enum machine_mode mode;
  enum tree_code code = TREE_CODE (type);
  int know_element_mode = 0;
  tree t;

  switch (code)
    {
    case VOID_TYPE:	case INTEGER_TYPE:	case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:	case CHAR_TYPE:		case POINTER_TYPE:
    case OFFSET_TYPE:	case REFERENCE_TYPE:	case METHOD_TYPE:
    case FILE_TYPE:	case SET_TYPE:		case LANG_TYPE:
    case FUNCTION_TYPE:
      return VOIDmode;

      /* Fortran complex types are supposed to be HFAs, so we need to handle
	 gcc's COMPLEX_TYPEs as HFAs.  We need to exclude the integral complex
	 types though.  */
    case COMPLEX_TYPE:
      if (GET_MODE_CLASS (TYPE_MODE (type)) == MODE_COMPLEX_FLOAT)
	return mode_for_size (GET_MODE_UNIT_SIZE (TYPE_MODE (type))
			      * BITS_PER_UNIT, MODE_FLOAT, 0);
      else
	return VOIDmode;

    case REAL_TYPE:
      /* We want to return VOIDmode for raw REAL_TYPEs, but the actual
	 mode if this is contained within an aggregate.  */
      if (nested)
	return TYPE_MODE (type);
      else
	return VOIDmode;

    case ARRAY_TYPE:
      return TYPE_MODE (TREE_TYPE (type));

    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      for (t = TYPE_FIELDS (type); t; t = TREE_CHAIN (t))
	{
	  if (TREE_CODE (t) != FIELD_DECL)
	    continue;

	  mode = hfa_element_mode (TREE_TYPE (t), 1);
	  if (know_element_mode)
	    {
	      if (mode != element_mode)
		return VOIDmode;
	    }
	  else if (GET_MODE_CLASS (mode) != MODE_FLOAT)
	    return VOIDmode;
	  else
	    {
	      know_element_mode = 1;
	      element_mode = mode;
	    }
	}
      return element_mode;

    default:
      /* If we reach here, we probably have some front-end specific type
	 that the backend doesn't know about.  This can happen via the
	 aggregate_value_p call in init_function_start.  All we can do is
	 ignore unknown tree types.  */
      return VOIDmode;
    }

  return VOIDmode;
}

/* Return rtx for register where argument is passed, or zero if it is passed
   on the stack.  */

/* ??? 128-bit quad-precision floats are always passed in general
   registers.  */

rtx
ia64_function_arg (cum, mode, type, named, incoming)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named;
     int incoming;
{
  int basereg = (incoming ? GR_ARG_FIRST : AR_ARG_FIRST);
  int words = (((mode == BLKmode ? int_size_in_bytes (type)
		 : GET_MODE_SIZE (mode)) + UNITS_PER_WORD - 1)
	       / UNITS_PER_WORD);
  int offset = 0;
  enum machine_mode hfa_mode = VOIDmode;

  /* Integer and float arguments larger than 8 bytes start at the next even
     boundary.  Aggregates larger than 8 bytes start at the next even boundary
     if the aggregate has 16 byte alignment.  Net effect is that types with
     alignment greater than 8 start at the next even boundary.  */
  /* ??? The ABI does not specify how to handle aggregates with alignment from
     9 to 15 bytes, or greater than 16.   We handle them all as if they had
     16 byte alignment.  Such aggregates can occur only if gcc extensions are
     used.  */
  if ((type ? (TYPE_ALIGN (type) > 8 * BITS_PER_UNIT)
       : (words > 1))
      && (cum->words & 1))
    offset = 1;

  /* If all argument slots are used, then it must go on the stack.  */
  if (cum->words + offset >= MAX_ARGUMENT_SLOTS)
    return 0;

  /* Check for and handle homogeneous FP aggregates.  */
  if (type)
    hfa_mode = hfa_element_mode (type, 0);

  /* Unnamed prototyped hfas are passed as usual.  Named prototyped hfas
     and unprototyped hfas are passed specially.  */
  if (hfa_mode != VOIDmode && (! cum->prototype || named))
    {
      rtx loc[16];
      int i = 0;
      int fp_regs = cum->fp_regs;
      int int_regs = cum->words + offset;
      int hfa_size = GET_MODE_SIZE (hfa_mode);
      int byte_size;
      int args_byte_size;

      /* If prototyped, pass it in FR regs then GR regs.
	 If not prototyped, pass it in both FR and GR regs.

	 If this is an SFmode aggregate, then it is possible to run out of
	 FR regs while GR regs are still left.  In that case, we pass the
	 remaining part in the GR regs.  */

      /* Fill the FP regs.  We do this always.  We stop if we reach the end
	 of the argument, the last FP register, or the last argument slot.  */

      byte_size = ((mode == BLKmode)
		   ? int_size_in_bytes (type) : GET_MODE_SIZE (mode));
      args_byte_size = int_regs * UNITS_PER_WORD;
      offset = 0;
      for (; (offset < byte_size && fp_regs < MAX_ARGUMENT_SLOTS
	      && args_byte_size < (MAX_ARGUMENT_SLOTS * UNITS_PER_WORD)); i++)
	{
	  loc[i] = gen_rtx_EXPR_LIST (VOIDmode,
				      gen_rtx_REG (hfa_mode, (FR_ARG_FIRST
							      + fp_regs)),
				      GEN_INT (offset));
	  offset += hfa_size;
	  args_byte_size += hfa_size;
	  fp_regs++;
	}

      /* If no prototype, then the whole thing must go in GR regs.  */
      if (! cum->prototype)
	offset = 0;
      /* If this is an SFmode aggregate, then we might have some left over
	 that needs to go in GR regs.  */
      else if (byte_size != offset)
	int_regs += offset / UNITS_PER_WORD;

      /* Fill in the GR regs.  We must use DImode here, not the hfa mode.  */

      for (; offset < byte_size && int_regs < MAX_ARGUMENT_SLOTS; i++)
	{
	  enum machine_mode gr_mode = DImode;

	  /* If we have an odd 4 byte hunk because we ran out of FR regs,
	     then this goes in a GR reg left adjusted/little endian, right
	     adjusted/big endian.  */
	  /* ??? Currently this is handled wrong, because 4-byte hunks are
	     always right adjusted/little endian.  */
	  if (offset & 0x4)
	    gr_mode = SImode;
	  /* If we have an even 4 byte hunk because the aggregate is a
	     multiple of 4 bytes in size, then this goes in a GR reg right
	     adjusted/little endian.  */
	  else if (byte_size - offset == 4)
	    gr_mode = SImode;

	  loc[i] = gen_rtx_EXPR_LIST (VOIDmode,
				      gen_rtx_REG (gr_mode, (basereg
							     + int_regs)),
				      GEN_INT (offset));
	  offset += GET_MODE_SIZE (gr_mode);
	  int_regs++;
	}

      /* If we ended up using just one location, just return that one loc.  */
      if (i == 1)
	return XEXP (loc[0], 0);
      else
	return gen_rtx_PARALLEL (mode, gen_rtvec_v (i, loc));
    }

  /* Integral and aggregates go in general registers.  If we have run out of
     FR registers, then FP values must also go in general registers.  This can
     happen when we have a SFmode HFA.  */
  else if (! FLOAT_MODE_P (mode) || cum->fp_regs == MAX_ARGUMENT_SLOTS)
    return gen_rtx_REG (mode, basereg + cum->words + offset);

  /* If there is a prototype, then FP values go in a FR register when
     named, and in a GR registeer when unnamed.  */
  else if (cum->prototype)
    {
      if (! named)
	return gen_rtx_REG (mode, basereg + cum->words + offset);
      else
	return gen_rtx_REG (mode, FR_ARG_FIRST + cum->fp_regs);
    }
  /* If there is no prototype, then FP values go in both FR and GR
     registers.  */
  else
    {
      rtx fp_reg = gen_rtx_EXPR_LIST (VOIDmode,
				      gen_rtx_REG (mode, (FR_ARG_FIRST
							  + cum->fp_regs)),
				      const0_rtx);
      rtx gr_reg = gen_rtx_EXPR_LIST (VOIDmode,
				      gen_rtx_REG (mode,
						   (basereg + cum->words
						    + offset)),
				      const0_rtx);

      return gen_rtx_PARALLEL (mode, gen_rtvec (2, fp_reg, gr_reg));
    }
}

/* Return number of words, at the beginning of the argument, that must be
   put in registers.  0 is the argument is entirely in registers or entirely
   in memory.  */

int
ia64_function_arg_partial_nregs (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named ATTRIBUTE_UNUSED;
{
  int words = (((mode == BLKmode ? int_size_in_bytes (type)
		 : GET_MODE_SIZE (mode)) + UNITS_PER_WORD - 1)
	       / UNITS_PER_WORD);
  int offset = 0;

  /* Arguments with alignment larger than 8 bytes start at the next even
     boundary.  */
  if ((type ? (TYPE_ALIGN (type) > 8 * BITS_PER_UNIT)
       : (words > 1))
      && (cum->words & 1))
    offset = 1;

  /* If all argument slots are used, then it must go on the stack.  */
  if (cum->words + offset >= MAX_ARGUMENT_SLOTS)
    return 0;

  /* It doesn't matter whether the argument goes in FR or GR regs.  If
     it fits within the 8 argument slots, then it goes entirely in
     registers.  If it extends past the last argument slot, then the rest
     goes on the stack.  */

  if (words + cum->words + offset <= MAX_ARGUMENT_SLOTS)
    return 0;

  return MAX_ARGUMENT_SLOTS - cum->words - offset;
}

/* Update CUM to point after this argument.  This is patterned after
   ia64_function_arg.  */

void
ia64_function_arg_advance (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named;
{
  int words = (((mode == BLKmode ? int_size_in_bytes (type)
		 : GET_MODE_SIZE (mode)) + UNITS_PER_WORD - 1)
	       / UNITS_PER_WORD);
  int offset = 0;
  enum machine_mode hfa_mode = VOIDmode;

  /* If all arg slots are already full, then there is nothing to do.  */
  if (cum->words >= MAX_ARGUMENT_SLOTS)
    return;

  /* Arguments with alignment larger than 8 bytes start at the next even
     boundary.  */
  if ((type ? (TYPE_ALIGN (type) > 8 * BITS_PER_UNIT)
       : (words > 1))
      && (cum->words & 1))
    offset = 1;

  cum->words += words + offset;

  /* Check for and handle homogeneous FP aggregates.  */
  if (type)
    hfa_mode = hfa_element_mode (type, 0);

  /* Unnamed prototyped hfas are passed as usual.  Named prototyped hfas
     and unprototyped hfas are passed specially.  */
  if (hfa_mode != VOIDmode && (! cum->prototype || named))
    {
      int fp_regs = cum->fp_regs;
      /* This is the original value of cum->words + offset.  */
      int int_regs = cum->words - words;
      int hfa_size = GET_MODE_SIZE (hfa_mode);
      int byte_size;
      int args_byte_size;

      /* If prototyped, pass it in FR regs then GR regs.
	 If not prototyped, pass it in both FR and GR regs.

	 If this is an SFmode aggregate, then it is possible to run out of
	 FR regs while GR regs are still left.  In that case, we pass the
	 remaining part in the GR regs.  */

      /* Fill the FP regs.  We do this always.  We stop if we reach the end
	 of the argument, the last FP register, or the last argument slot.  */

      byte_size = ((mode == BLKmode)
		   ? int_size_in_bytes (type) : GET_MODE_SIZE (mode));
      args_byte_size = int_regs * UNITS_PER_WORD;
      offset = 0;
      for (; (offset < byte_size && fp_regs < MAX_ARGUMENT_SLOTS
	      && args_byte_size < (MAX_ARGUMENT_SLOTS * UNITS_PER_WORD));)
	{
	  offset += hfa_size;
	  args_byte_size += hfa_size;
	  fp_regs++;
	}

      cum->fp_regs = fp_regs;
    }

  /* Integral and aggregates go in general registers.  If we have run out of
     FR registers, then FP values must also go in general registers.  This can
     happen when we have a SFmode HFA.  */
  else if (! FLOAT_MODE_P (mode) || cum->fp_regs == MAX_ARGUMENT_SLOTS)
    return;

  /* If there is a prototype, then FP values go in a FR register when
     named, and in a GR registeer when unnamed.  */
  else if (cum->prototype)
    {
      if (! named)
	return;
      else
	/* ??? Complex types should not reach here.  */
	cum->fp_regs += (GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT ? 2 : 1);
    }
  /* If there is no prototype, then FP values go in both FR and GR
     registers.  */
  else
    /* ??? Complex types should not reach here.  */
    cum->fp_regs += (GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT ? 2 : 1);

  return;
}

/* Implement va_start.  */

void
ia64_va_start (stdarg_p, valist, nextarg)
     int stdarg_p;
     tree valist;
     rtx nextarg;
{
  int arg_words;
  int ofs;

  arg_words = current_function_args_info.words;

  if (stdarg_p)
    ofs = 0;
  else
    ofs = (arg_words >= MAX_ARGUMENT_SLOTS ? -UNITS_PER_WORD : 0);

  nextarg = plus_constant (nextarg, ofs);
  std_expand_builtin_va_start (1, valist, nextarg);
}

/* Implement va_arg.  */

rtx
ia64_va_arg (valist, type)
     tree valist, type;
{
  tree t;

  /* Arguments with alignment larger than 8 bytes start at the next even
     boundary.  */
  if (TYPE_ALIGN (type) > 8 * BITS_PER_UNIT)
    {
      t = build (PLUS_EXPR, TREE_TYPE (valist), valist,
		 build_int_2 (2 * UNITS_PER_WORD - 1, 0));
      t = build (BIT_AND_EXPR, TREE_TYPE (t), t,
		 build_int_2 (-2 * UNITS_PER_WORD, -1));
      t = build (MODIFY_EXPR, TREE_TYPE (valist), valist, t);
      TREE_SIDE_EFFECTS (t) = 1;
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
    }

  return std_expand_builtin_va_arg (valist, type);
}

/* Return 1 if function return value returned in memory.  Return 0 if it is
   in a register.  */

int
ia64_return_in_memory (valtype)
     tree valtype;
{
  enum machine_mode mode;
  enum machine_mode hfa_mode;
  int byte_size;

  mode = TYPE_MODE (valtype);
  byte_size = ((mode == BLKmode)
	       ? int_size_in_bytes (valtype) : GET_MODE_SIZE (mode));

  /* Hfa's with up to 8 elements are returned in the FP argument registers.  */

  hfa_mode = hfa_element_mode (valtype, 0);
  if (hfa_mode != VOIDmode)
    {
      int hfa_size = GET_MODE_SIZE (hfa_mode);

      if (byte_size / hfa_size > MAX_ARGUMENT_SLOTS)
	return 1;
      else
	return 0;
    }

  else if (byte_size > UNITS_PER_WORD * MAX_INT_RETURN_SLOTS)
    return 1;
  else
    return 0;
}

/* Return rtx for register that holds the function return value.  */

rtx
ia64_function_value (valtype, func)
     tree valtype;
     tree func ATTRIBUTE_UNUSED;
{
  enum machine_mode mode;
  enum machine_mode hfa_mode;

  mode = TYPE_MODE (valtype);
  hfa_mode = hfa_element_mode (valtype, 0);

  if (hfa_mode != VOIDmode)
    {
      rtx loc[8];
      int i;
      int hfa_size;
      int byte_size;
      int offset;

      hfa_size = GET_MODE_SIZE (hfa_mode);
      byte_size = ((mode == BLKmode)
		   ? int_size_in_bytes (valtype) : GET_MODE_SIZE (mode));
      offset = 0;
      for (i = 0; offset < byte_size; i++)
	{
	  loc[i] = gen_rtx_EXPR_LIST (VOIDmode,
				      gen_rtx_REG (hfa_mode, FR_ARG_FIRST + i),
				      GEN_INT (offset));
	  offset += hfa_size;
	}

      if (i == 1)
	return XEXP (loc[0], 0);
      else
	return gen_rtx_PARALLEL (mode, gen_rtvec_v (i, loc));
    }
  else if (FLOAT_TYPE_P (valtype))
    return gen_rtx_REG (mode, FR_ARG_FIRST);
  else
    return gen_rtx_REG (mode, GR_RET_FIRST);
}

/* Print a memory address as an operand to reference that memory location.  */

/* ??? Do we need this?  It gets used only for 'a' operands.  We could perhaps
   also call this from ia64_print_operand for memory addresses.  */

void
ia64_print_operand_address (stream, address)
     FILE * stream ATTRIBUTE_UNUSED;
     rtx    address ATTRIBUTE_UNUSED;
{
}

/* Print an operand to a assembler instruction.
   B    Work arounds for hardware bugs.
   C	Swap and print a comparison operator.
   D	Print an FP comparison operator.
   E    Print 32 - constant, for SImode shifts as extract.
   F	A floating point constant 0.0 emitted as f0, or 1.0 emitted as f1, or
        a floating point register emitted normally.
   I	Invert a predicate register by adding 1.
   J    Select the proper predicate register for a condition.
   j    Select the inverse predicate register for a condition.
   O	Append .acq for volatile load.
   P	Postincrement of a MEM.
   Q	Append .rel for volatile store.
   S	Shift amount for shladd instruction.
   T	Print an 8-bit sign extended number (K) as a 32-bit unsigned number
	for Intel assembler.
   U	Print an 8-bit sign extended number (K) as a 64-bit unsigned number
	for Intel assembler.
   r	Print register name, or constant 0 as r0.  HP compatibility for
	Linux kernel.  */
void
ia64_print_operand (file, x, code)
     FILE * file;
     rtx    x;
     int    code;
{
  const char *str;

  switch (code)
    {
    case 0:
      /* Handled below.  */
      break;

    case 'B':
      if (TARGET_A_STEP)
	fputs (" ;; nop 0 ;; nop 0 ;;", file);
      return;

    case 'C':
      {
	enum rtx_code c = swap_condition (GET_CODE (x));
	fputs (GET_RTX_NAME (c), file);
	return;
      }

    case 'D':
      switch (GET_CODE (x))
	{
	case NE:
	  str = "neq";
	  break;
	case UNORDERED:
	  str = "unord";
	  break;
	case ORDERED:
	  str = "ord";
	  break;
	default:
	  str = GET_RTX_NAME (GET_CODE (x));
	  break;
	}
      fputs (str, file);
      return;

    case 'E':
      fprintf (file, HOST_WIDE_INT_PRINT_DEC, 32 - INTVAL (x));
      return;

    case 'F':
      if (x == CONST0_RTX (GET_MODE (x)))
	str = reg_names [FR_REG (0)];
      else if (x == CONST1_RTX (GET_MODE (x)))
	str = reg_names [FR_REG (1)];
      else if (GET_CODE (x) == REG)
	str = reg_names [REGNO (x)];
      else
	abort ();
      fputs (str, file);
      return;

    case 'I':
      fputs (reg_names [REGNO (x) + 1], file);
      return;

    case 'J':
    case 'j':
      {
	unsigned int regno = REGNO (XEXP (x, 0));
	if (GET_CODE (x) == EQ)
	  regno += 1;
	if (code == 'j')
	  regno ^= 1;
        fputs (reg_names [regno], file);
      }
      return;

    case 'O':
      if (MEM_VOLATILE_P (x))
	fputs(".acq", file);
      return;

    case 'P':
      {
	HOST_WIDE_INT value;

	switch (GET_CODE (XEXP (x, 0)))
	  {
	  default:
	    return;

	  case POST_MODIFY:
	    x = XEXP (XEXP (XEXP (x, 0), 1), 1);
	    if (GET_CODE (x) == CONST_INT)
	      value = INTVAL (x);
	    else if (GET_CODE (x) == REG)
	      {
		fprintf (file, ", %s", reg_names[REGNO (x)]);
		return;
	      }
	    else
	      abort ();
	    break;

	  case POST_INC:
	    value = GET_MODE_SIZE (GET_MODE (x));
	    break;

	  case POST_DEC:
	    value = - (HOST_WIDE_INT) GET_MODE_SIZE (GET_MODE (x));
	    break;
	  }

	putc (',', file);
	putc (' ', file);
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, value);
	return;
      }

    case 'Q':
      if (MEM_VOLATILE_P (x))
	fputs(".rel", file);
      return;

    case 'S':
      fprintf (file, "%d", exact_log2 (INTVAL (x)));
      return;

    case 'T':
      if (! TARGET_GNU_AS && GET_CODE (x) == CONST_INT)
	{
	  fprintf (file, "0x%x", (int) INTVAL (x) & 0xffffffff);
	  return;
	}
      break;

    case 'U':
      if (! TARGET_GNU_AS && GET_CODE (x) == CONST_INT)
	{
	  const char *prefix = "0x";
	  if (INTVAL (x) & 0x80000000)
	    {
	      fprintf (file, "0xffffffff");
	      prefix = "";
	    }
	  fprintf (file, "%s%x", prefix, (int) INTVAL (x) & 0xffffffff);
	  return;
	}
      break;

    case 'r':
      /* If this operand is the constant zero, write it as zero.  */
      if (GET_CODE (x) == REG)
	fputs (reg_names[REGNO (x)], file);
      else if (x == CONST0_RTX (GET_MODE (x)))
	fputs ("r0", file);
      else
	output_operand_lossage ("invalid %%r value");
      return;

    case '+':
      {
	const char *which;
	
	/* For conditional branches, returns or calls, substitute
	   sptk, dptk, dpnt, or spnt for %s.  */
	x = find_reg_note (current_output_insn, REG_BR_PROB, 0);
	if (x)
	  {
	    int pred_val = INTVAL (XEXP (x, 0));

	    /* Guess top and bottom 10% statically predicted.  */
	    if (pred_val < REG_BR_PROB_BASE / 10)
	      which = ".spnt";
	    else if (pred_val < REG_BR_PROB_BASE / 2)
	      which = ".dpnt";
	    else if (pred_val < REG_BR_PROB_BASE * 9 / 10)
	      which = ".dptk";
	    else
	      which = ".sptk";
	  }
	else if (GET_CODE (current_output_insn) == CALL_INSN)
	  which = ".sptk";
	else
	  which = ".dptk";

	fputs (which, file);
	return;
      }

    case ',':
      x = current_insn_predicate;
      if (x)
	{
	  unsigned int regno = REGNO (XEXP (x, 0));
	  if (GET_CODE (x) == EQ)
	    regno += 1;
          fprintf (file, "(%s) ", reg_names [regno]);
	}
      return;

    default:
      output_operand_lossage ("ia64_print_operand: unknown code");
      return;
    }

  switch (GET_CODE (x))
    {
      /* This happens for the spill/restore instructions.  */
    case POST_INC:
    case POST_DEC:
    case POST_MODIFY:
      x = XEXP (x, 0);
      /* ... fall through ... */

    case REG:
      fputs (reg_names [REGNO (x)], file);
      break;

    case MEM:
      {
	rtx addr = XEXP (x, 0);
	if (GET_RTX_CLASS (GET_CODE (addr)) == 'a')
	  addr = XEXP (addr, 0);
	fprintf (file, "[%s]", reg_names [REGNO (addr)]);
	break;
      }

    default:
      output_addr_const (file, x);
      break;
    }

  return;
}

/* Calulate the cost of moving data from a register in class FROM to
   one in class TO.  */

int
ia64_register_move_cost (from, to)
     enum reg_class from, to;
{
  int from_hard, to_hard;
  int from_gr, to_gr;
  int from_fr, to_fr;

  from_hard = (from == BR_REGS || from == AR_M_REGS || from == AR_I_REGS);
  to_hard = (to == BR_REGS || to == AR_M_REGS || to == AR_I_REGS);
  from_gr = (from == GENERAL_REGS);
  to_gr = (to == GENERAL_REGS);
  from_fr = (from == FR_REGS);
  to_fr = (to == FR_REGS);

  if (from_hard && to_hard)
    return 8;
  else if ((from_hard && !to_gr) || (!from_gr && to_hard))
    return 6;

  /* ??? Moving from FR<->GR must be more expensive than 2, so that we get
     secondary memory reloads for TFmode moves.  Unfortunately, we don't
     have the mode here, so we can't check that.  */
  /* Moreover, we have to make this at least as high as MEMORY_MOVE_COST
     to avoid spectacularly poor register class preferencing for TFmode.  */
  else if (from_fr != to_fr)
    return 5;

  return 2;
}

/* This function returns the register class required for a secondary
   register when copying between one of the registers in CLASS, and X,
   using MODE.  A return value of NO_REGS means that no secondary register
   is required.  */

enum reg_class
ia64_secondary_reload_class (class, mode, x)
     enum reg_class class;
     enum machine_mode mode ATTRIBUTE_UNUSED;
     rtx x;
{
  int regno = -1;

  if (GET_CODE (x) == REG || GET_CODE (x) == SUBREG)
    regno = true_regnum (x);

  switch (class)
    {
    case BR_REGS:
      /* ??? This is required because of a bad gcse/cse/global interaction.
	 We end up with two pseudos with overlapping lifetimes both of which
	 are equiv to the same constant, and both which need to be in BR_REGS.
	 This results in a BR_REGS to BR_REGS copy which doesn't exist.  To
	 reproduce, return NO_REGS here, and compile divdi3 in libgcc2.c.
	 This seems to be a cse bug.  cse_basic_block_end changes depending
	 on the path length, which means the qty_first_reg check in
	 make_regs_eqv can give different answers at different times.  */
      /* ??? At some point I'll probably need a reload_indi pattern to handle
	 this.  */
      if (BR_REGNO_P (regno))
	return GR_REGS;

      /* This is needed if a pseudo used as a call_operand gets spilled to a
	 stack slot.  */
      if (GET_CODE (x) == MEM)
	return GR_REGS;
      break;

    case FR_REGS:
      /* This can happen when a paradoxical subreg is an operand to the
	 muldi3 pattern.  */
      /* ??? This shouldn't be necessary after instruction scheduling is
	 enabled, because paradoxical subregs are not accepted by
	 register_operand when INSN_SCHEDULING is defined.  Or alternatively,
	 stop the paradoxical subreg stupidity in the *_operand functions
	 in recog.c.  */
      if (GET_CODE (x) == MEM
	  && (GET_MODE (x) == SImode || GET_MODE (x) == HImode
	      || GET_MODE (x) == QImode))
	return GR_REGS;

      /* This can happen because of the ior/and/etc patterns that accept FP
	 registers as operands.  If the third operand is a constant, then it
	 needs to be reloaded into a FP register.  */
      if (GET_CODE (x) == CONST_INT)
	return GR_REGS;

      /* This can happen because of register elimination in a muldi3 insn.
	 E.g. `26107 * (unsigned long)&u'.  */
      if (GET_CODE (x) == PLUS)
	return GR_REGS;
      break;

    case PR_REGS:
      /* ??? This happens if we cse/gcse a CCmode value across a call,
	 and the function has a nonlocal goto.  This is because global
	 does not allocate call crossing pseudos to hard registers when
	 current_function_has_nonlocal_goto is true.  This is relatively
	 common for C++ programs that use exceptions.  To reproduce,
	 return NO_REGS and compile libstdc++.  */
      if (GET_CODE (x) == MEM)
	return GR_REGS;
      break;

    case GR_REGS:
      /* Since we have no offsettable memory addresses, we need a temporary
	 to hold the address of the second word.  */
      if (mode == TImode)
	return GR_REGS;
      break;

    default:
      break;
    }

  return NO_REGS;
}


/* Emit text to declare externally defined variables and functions, because
   the Intel assembler does not support undefined externals.  */

void
ia64_asm_output_external (file, decl, name)
     FILE *file;
     tree decl;
     const char *name;
{
  int save_referenced;

  /* GNU as does not need anything here.  */
  if (TARGET_GNU_AS)
    return;

  /* ??? The Intel assembler creates a reference that needs to be satisfied by
     the linker when we do this, so we need to be careful not to do this for
     builtin functions which have no library equivalent.  Unfortunately, we
     can't tell here whether or not a function will actually be called by
     expand_expr, so we pull in library functions even if we may not need
     them later.  */
  if (! strcmp (name, "__builtin_next_arg")
      || ! strcmp (name, "alloca")
      || ! strcmp (name, "__builtin_constant_p")
      || ! strcmp (name, "__builtin_args_info"))
    return;

  /* assemble_name will set TREE_SYMBOL_REFERENCED, so we must save and
     restore it.  */
  save_referenced = TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl));
  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      fprintf (file, "\t%s\t ", TYPE_ASM_OP);
      assemble_name (file, name);
      putc (',', file);
      fprintf (file, TYPE_OPERAND_FMT, "function");
      putc ('\n', file);
    }
  ASM_GLOBALIZE_LABEL (file, name);
  TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl)) = save_referenced;
}

/* Parse the -mfixed-range= option string.  */

static void
fix_range (const_str)
     const char *const_str;
{
  int i, first, last;
  char *str, *dash, *comma;

  /* str must be of the form REG1'-'REG2{,REG1'-'REG} where REG1 and
     REG2 are either register names or register numbers.  The effect
     of this option is to mark the registers in the range from REG1 to
     REG2 as ``fixed'' so they won't be used by the compiler.  This is
     used, e.g., to ensure that kernel mode code doesn't use f32-f127.  */

  i = strlen (const_str);
  str = (char *) alloca (i + 1);
  memcpy (str, const_str, i + 1);

  while (1)
    {
      dash = strchr (str, '-');
      if (!dash)
	{
	  warning ("value of -mfixed-range must have form REG1-REG2");
	  return;
	}
      *dash = '\0';

      comma = strchr (dash + 1, ',');
      if (comma)
	*comma = '\0';

      first = decode_reg_name (str);
      if (first < 0)
	{
	  warning ("unknown register name: %s", str);
	  return;
	}

      last = decode_reg_name (dash + 1);
      if (last < 0)
	{
	  warning ("unknown register name: %s", dash + 1);
	  return;
	}

      *dash = '-';

      if (first > last)
	{
	  warning ("%s-%s is an empty range", str, dash + 1);
	  return;
	}

      for (i = first; i <= last; ++i)
	fixed_regs[i] = call_used_regs[i] = 1;

      if (!comma)
	break;

      *comma = ',';
      str = comma + 1;
    }
}

/* Called to register all of our global variables with the garbage
   collector.  */

static void
ia64_add_gc_roots ()
{
  ggc_add_rtx_root (&ia64_compare_op0, 1);
  ggc_add_rtx_root (&ia64_compare_op1, 1);
}

static void
ia64_init_machine_status (p)
     struct function *p;
{
  p->machine =
    (struct machine_function *) xcalloc (1, sizeof (struct machine_function));
}

static void
ia64_mark_machine_status (p)
     struct function *p;
{
  ggc_mark_rtx (p->machine->ia64_eh_epilogue_sp);
  ggc_mark_rtx (p->machine->ia64_eh_epilogue_bsp);
  ggc_mark_rtx (p->machine->ia64_gp_save);
}


/* Handle TARGET_OPTIONS switches.  */

void
ia64_override_options ()
{
  if (TARGET_AUTO_PIC)
    target_flags |= MASK_CONST_GP;

  if (ia64_fixed_range_string)
    fix_range (ia64_fixed_range_string);

  ia64_section_threshold = g_switch_set ? g_switch_value : IA64_DEFAULT_GVALUE;

  init_machine_status = ia64_init_machine_status;
  mark_machine_status = ia64_mark_machine_status;

  ia64_add_gc_roots ();
}

/* The following collection of routines emit instruction group stop bits as
   necessary to avoid dependencies.  */

/* Need to track some additional registers as far as serialization is
   concerned so we can properly handle br.call and br.ret.  We could
   make these registers visible to gcc, but since these registers are
   never explicitly used in gcc generated code, it seems wasteful to
   do so (plus it would make the call and return patterns needlessly
   complex).  */
#define REG_GP		(GR_REG (1))
#define REG_RP		(BR_REG (0))
#define REG_AR_CFM	(FIRST_PSEUDO_REGISTER + 1)
/* This is used for volatile asms which may require a stop bit immediately
   before and after them.  */
#define REG_VOLATILE	(FIRST_PSEUDO_REGISTER + 2)
#define AR_UNAT_BIT_0	(FIRST_PSEUDO_REGISTER + 3)
#define NUM_REGS	(AR_UNAT_BIT_0 + 64)

/* For each register, we keep track of how many times it has been
   written in the current instruction group.  If a register is written
   unconditionally (no qualifying predicate), WRITE_COUNT is set to 2
   and FIRST_PRED is ignored.  If a register is written if its
   qualifying predicate P is true, we set WRITE_COUNT to 1 and
   FIRST_PRED to P.  Later on, the same register may be written again
   by the complement of P (P+1 if P is even, P-1, otherwise) and when
   this happens, WRITE_COUNT gets set to 2.  The result of this is
   that whenever an insn attempts to write a register whose
   WRITE_COUNT is two, we need to issue a insn group barrier first.  */
struct reg_write_state
{
  char write_count;
  char written_by_fp;	/* Was register written by a floating-point insn?  */
  short first_pred;	/* 0 means ``no predicate'' */
};

/* Cumulative info for the current instruction group.  */
struct reg_write_state rws_sum[NUM_REGS];
/* Info for the current instruction.  This gets copied to rws_sum after a
   stop bit is emitted.  */
struct reg_write_state rws_insn[NUM_REGS];

/* Misc flags needed to compute RAW/WAW dependencies while we are traversing
   RTL for one instruction.  */
struct reg_flags
{
  unsigned int is_write : 1;	/* Is register being written?  */
  unsigned int is_fp : 1;	/* Is register used as part of an fp op?  */
  unsigned int is_branch : 1;	/* Is register used as part of a branch?  */
};

static void rws_update PARAMS ((struct reg_write_state *, int,
				struct reg_flags, int));
static int rws_access_regno PARAMS ((int, struct reg_flags, int));
static int rws_access_reg PARAMS ((rtx, struct reg_flags, int));
static int rtx_needs_barrier PARAMS ((rtx, struct reg_flags, int));

/* Update *RWS for REGNO, which is being written by the current instruction,
   with predicate PRED, and associated register flags in FLAGS.  */

static void
rws_update (rws, regno, flags, pred)
     struct reg_write_state *rws;
     int regno;
     struct reg_flags flags;
     int pred;
{
  rws[regno].write_count += pred ? 1 : 2;
  rws[regno].written_by_fp |= flags.is_fp;
  rws[regno].first_pred = pred;
}

/* Handle an access to register REGNO of type FLAGS using predicate register
   PRED.  Update rws_insn and rws_sum arrays.  Return 1 if this access creates
   a dependency with an earlier instruction in the same group.  */

static int
rws_access_regno (regno, flags, pred)
     int regno;
     struct reg_flags flags;
     int pred;
{
  int need_barrier = 0;

  if (regno >= NUM_REGS)
    abort ();

  if (flags.is_write)
    {
      int write_count;

      /* One insn writes same reg multiple times?  */
      if (rws_insn[regno].write_count > 0)
	abort ();

      /* Update info for current instruction.  */
      rws_update (rws_insn, regno, flags, pred);
      write_count = rws_sum[regno].write_count;

      switch (write_count)
	{
	case 0:
	  /* The register has not been written yet.  */
	  rws_update (rws_sum, regno, flags, pred);
	  break;

	case 1:
	  /* The register has been written via a predicate.  If this is
	     not a complementary predicate, then we need a barrier.  */
	  /* ??? This assumes that P and P+1 are always complementary
	     predicates for P even.  */
	  if ((rws_sum[regno].first_pred ^ 1) != pred)
	    need_barrier = 1;
	  rws_update (rws_sum, regno, flags, pred);
	  break;

	case 2:
	  /* The register has been unconditionally written already.  We
	     need a barrier.  */
	  need_barrier = 1;
	  break;

	default:
	  abort ();
	}
    }
  else
    {
      if (flags.is_branch)
	{
	  /* Branches have several RAW exceptions that allow to avoid
	     barriers.  */

	  if (REGNO_REG_CLASS (regno) == BR_REGS || regno == AR_PFS_REGNUM)
	    /* RAW dependencies on branch regs are permissible as long
	       as the writer is a non-branch instruction.  Since we
	       never generate code that uses a branch register written
	       by a branch instruction, handling this case is
	       easy.  */
	    return 0;

	  if (REGNO_REG_CLASS (regno) == PR_REGS
	      && ! rws_sum[regno].written_by_fp)
	    /* The predicates of a branch are available within the
	       same insn group as long as the predicate was written by
	       something other than a floating-point instruction.   */
	    return 0;
	}

      switch (rws_sum[regno].write_count)
	{
	case 0:
	  /* The register has not been written yet.  */
	  break;

	case 1:
	  /* The register has been written via a predicate.  If this is
	     not a complementary predicate, then we need a barrier.  */
	  /* ??? This assumes that P and P+1 are always complementary
	     predicates for P even.  */
	  if ((rws_sum[regno].first_pred ^ 1) != pred)
	    need_barrier = 1;
	  break;

	case 2:
	  /* The register has been unconditionally written already.  We
	     need a barrier.  */
	  need_barrier = 1;
	  break;

	default:
	  abort ();
	}
    }

  return need_barrier;
}

static int
rws_access_reg (reg, flags, pred)
     rtx reg;
     struct reg_flags flags;
     int pred;
{
  int regno = REGNO (reg);
  int n = HARD_REGNO_NREGS (REGNO (reg), GET_MODE (reg));

  if (n == 1)
    return rws_access_regno (regno, flags, pred);
  else
    {
      int need_barrier = 0;
      while (--n >= 0)
	need_barrier |= rws_access_regno (regno + n, flags, pred);
      return need_barrier;
    }
}

/* Handle an access to rtx X of type FLAGS using predicate register PRED.
   Return 1 is this access creates a dependency with an earlier instruction
   in the same group.  */

static int
rtx_needs_barrier (x, flags, pred)
     rtx x;
     struct reg_flags flags;
     int pred;
{
  int i, j;
  int is_complemented = 0;
  int need_barrier = 0;
  const char *format_ptr;
  struct reg_flags new_flags;
  rtx src, dst;
  rtx cond = 0;

  if (! x)
    return 0;

  new_flags = flags;

  switch (GET_CODE (x))
    {
    case SET:
      src = SET_SRC (x);
      switch (GET_CODE (src))
	{
	case CALL:
	  /* We don't need to worry about the result registers that
             get written by subroutine call.  */
	  need_barrier = rtx_needs_barrier (src, flags, pred);
	  return need_barrier;

	case IF_THEN_ELSE:
	  if (SET_DEST (x) == pc_rtx)
	    {
	      /* X is a conditional branch.  */
	      /* ??? This seems redundant, as the caller sets this bit for
		 all JUMP_INSNs.  */
	      new_flags.is_branch = 1;
	      need_barrier = rtx_needs_barrier (src, new_flags, pred);
	      return need_barrier;
	    }
	  else
	    {
	      /* X is a conditional move.  */
	      cond = XEXP (src, 0);
	      if (GET_CODE (cond) == EQ)
		is_complemented = 1;
	      cond = XEXP (cond, 0);
	      if (GET_CODE (cond) != REG
		  && REGNO_REG_CLASS (REGNO (cond)) != PR_REGS)
		abort ();

	      if (XEXP (src, 1) == SET_DEST (x)
		  || XEXP (src, 2) == SET_DEST (x))
		{
		  /* X is a conditional move that conditionally writes the
		     destination.  */

		  /* We need another complement in this case.  */
		  if (XEXP (src, 1) == SET_DEST (x))
		    is_complemented = ! is_complemented;

		  pred = REGNO (cond);
		  if (is_complemented)
		    ++pred;
		}

	      /* ??? If this is a conditional write to the dest, then this
		 instruction does not actually read one source.  This probably
		 doesn't matter, because that source is also the dest.  */
	      /* ??? Multiple writes to predicate registers are allowed
		 if they are all AND type compares, or if they are all OR
		 type compares.  We do not generate such instructions
		 currently.  */
	    }
	  /* ... fall through ... */

	default:
	  if (GET_RTX_CLASS (GET_CODE (src)) == '<'
	       && GET_MODE_CLASS (GET_MODE (XEXP (src, 0))) == MODE_FLOAT)
	    /* Set new_flags.is_fp to 1 so that we know we're dealing
	       with a floating point comparison when processing the
	       destination of the SET.  */
	    new_flags.is_fp = 1;
	  break;
	}
      need_barrier = rtx_needs_barrier (src, flags, pred);

      /* This instruction unconditionally uses a predicate register.  */
      if (cond)
	need_barrier |= rws_access_reg (cond, flags, 0);

      dst = SET_DEST (x);
      if (GET_CODE (dst) == ZERO_EXTRACT)
	{
	  need_barrier |= rtx_needs_barrier (XEXP (dst, 1), flags, pred);
	  need_barrier |= rtx_needs_barrier (XEXP (dst, 2), flags, pred);
	  dst = XEXP (dst, 0);
	}
      new_flags.is_write = 1;
      need_barrier |= rtx_needs_barrier (dst, new_flags, pred);
      break;

    case CALL:
      new_flags.is_write = 0;
      need_barrier |= rws_access_regno (AR_EC_REGNUM, new_flags, pred);

      /* Avoid multiple register writes, in case this is a pattern with
	 multiple CALL rtx.  This avoids an abort in rws_access_reg.  */
      /* ??? This assumes that no rtx other than CALL/RETURN sets REG_AR_CFM,
	 and that we don't have predicated calls/returns.  */
      if (! rws_insn[REG_AR_CFM].write_count)
	{
	  new_flags.is_write = 1;
	  need_barrier |= rws_access_regno (REG_RP, new_flags, pred);
	  need_barrier |= rws_access_regno (AR_PFS_REGNUM, new_flags, pred);
	  need_barrier |= rws_access_regno (REG_AR_CFM, new_flags, pred);
	}
      break;

    case COND_EXEC:
      /* X is a predicated instruction.  */

      cond = COND_EXEC_TEST (x);
      if (pred)
	abort ();
      need_barrier = rtx_needs_barrier (cond, flags, 0);

      if (GET_CODE (cond) == EQ)
	is_complemented = 1;
      cond = XEXP (cond, 0);
      if (GET_CODE (cond) != REG
	  && REGNO_REG_CLASS (REGNO (cond)) != PR_REGS)
	abort ();
      pred = REGNO (cond);
      if (is_complemented)
	++pred;

      need_barrier |= rtx_needs_barrier (COND_EXEC_CODE (x), flags, pred);
      return need_barrier;

    case CLOBBER:
#if 0
    case USE:
      /* We must handle USE here in case it occurs within a PARALLEL.
	 For instance, the mov ar.pfs= instruction has a USE which requires
	 a barrier between it and an immediately preceeding alloc.  */
#endif
      /* Clobber & use are for earlier compiler-phases only.  */
      break;

    case ASM_OPERANDS:
    case ASM_INPUT:
      /* We always emit stop bits for traditional asms.  We emit stop bits
	 for volatile extended asms if TARGET_VOL_ASM_STOP is true.  */
      if (GET_CODE (x) != ASM_OPERANDS
	  || (MEM_VOLATILE_P (x) && TARGET_VOL_ASM_STOP))
	{
	  /* Avoid writing the register multiple times if we have multiple
	     asm outputs.  This avoids an abort in rws_access_reg.  */
	  if (! rws_insn[REG_VOLATILE].write_count)
	    {
	      new_flags.is_write = 1;
	      rws_access_regno (REG_VOLATILE, new_flags, pred);
	    }
	  return 1;
	}

      /* For all ASM_OPERANDS, we must traverse the vector of input operands.
	 We can not just fall through here since then we would be confused
	 by the ASM_INPUT rtx inside ASM_OPERANDS, which do not indicate
	 traditional asms unlike their normal usage.  */

      for (i = ASM_OPERANDS_INPUT_LENGTH (x) - 1; i >= 0; --i)
	if (rtx_needs_barrier (ASM_OPERANDS_INPUT (x, i), flags, pred))
	  need_barrier = 1;
      break;

    case PARALLEL:
      for (i = XVECLEN (x, 0) - 1; i >= 0; --i)
	if (rtx_needs_barrier (XVECEXP (x, 0, i), flags, pred))
	  need_barrier = 1;
      break;

    case SUBREG:
      x = SUBREG_REG (x);
      /* FALLTHRU */
    case REG:
      if (REGNO (x) == AR_UNAT_REGNUM)
	{
	  for (i = 0; i < 64; ++i)
	    need_barrier |= rws_access_regno (AR_UNAT_BIT_0 + i, flags, pred);
	}
      else
	need_barrier = rws_access_reg (x, flags, pred);
      break;

    case MEM:
      /* Find the regs used in memory address computation.  */
      new_flags.is_write = 0;
      need_barrier = rtx_needs_barrier (XEXP (x, 0), new_flags, pred);
      break;

    case CONST_INT:   case CONST_DOUBLE:
    case SYMBOL_REF:  case LABEL_REF:     case CONST:
      break;

      /* Operators with side-effects.  */
    case POST_INC:    case POST_DEC:
      if (GET_CODE (XEXP (x, 0)) != REG)
	abort ();

      new_flags.is_write = 0;
      need_barrier  = rws_access_reg (XEXP (x, 0), new_flags, pred);
      new_flags.is_write = 1;
      need_barrier |= rws_access_reg (XEXP (x, 0), new_flags, pred);
      break;

    case POST_MODIFY:
      if (GET_CODE (XEXP (x, 0)) != REG)
	abort ();

      new_flags.is_write = 0;
      need_barrier  = rws_access_reg (XEXP (x, 0), new_flags, pred);
      need_barrier |= rtx_needs_barrier (XEXP (x, 1), new_flags, pred);
      new_flags.is_write = 1;
      need_barrier |= rws_access_reg (XEXP (x, 0), new_flags, pred);
      break;

      /* Handle common unary and binary ops for efficiency.  */
    case COMPARE:  case PLUS:    case MINUS:   case MULT:      case DIV:
    case MOD:      case UDIV:    case UMOD:    case AND:       case IOR:
    case XOR:      case ASHIFT:  case ROTATE:  case ASHIFTRT:  case LSHIFTRT:
    case ROTATERT: case SMIN:    case SMAX:    case UMIN:      case UMAX:
    case NE:       case EQ:      case GE:      case GT:        case LE:
    case LT:       case GEU:     case GTU:     case LEU:       case LTU:
      need_barrier = rtx_needs_barrier (XEXP (x, 0), new_flags, pred);
      need_barrier |= rtx_needs_barrier (XEXP (x, 1), new_flags, pred);
      break;

    case NEG:      case NOT:	        case SIGN_EXTEND:     case ZERO_EXTEND:
    case TRUNCATE: case FLOAT_EXTEND:   case FLOAT_TRUNCATE:  case FLOAT:
    case FIX:      case UNSIGNED_FLOAT: case UNSIGNED_FIX:    case ABS:
    case SQRT:     case FFS:
      need_barrier = rtx_needs_barrier (XEXP (x, 0), flags, pred);
      break;

    case UNSPEC:
      switch (XINT (x, 1))
	{
	case 1: /* st8.spill */
	case 2: /* ld8.fill */
	  {
	    HOST_WIDE_INT offset = INTVAL (XVECEXP (x, 0, 1));
	    HOST_WIDE_INT bit = (offset >> 3) & 63;

	    need_barrier = rtx_needs_barrier (XVECEXP (x, 0, 0), flags, pred);
	    new_flags.is_write = (XINT (x, 1) == 1);
	    need_barrier |= rws_access_regno (AR_UNAT_BIT_0 + bit,
					      new_flags, pred);
	    break;
	  }
	  
	case 3: /* stf.spill */
	case 4: /* ldf.spill */
	case 8: /* popcnt */
	  need_barrier = rtx_needs_barrier (XVECEXP (x, 0, 0), flags, pred);
	  break;

        case 12: /* mf */
        case 19: /* fetchadd_acq */
	case 20: /* mov = ar.bsp */
	case 21: /* flushrs */
          break;

        case 13: /* cmpxchg_acq */
	  need_barrier = rtx_needs_barrier (XVECEXP (x, 0, 1), flags, pred);
	  need_barrier |= rtx_needs_barrier (XVECEXP (x, 0, 2), flags, pred);
	  break;

	default:
	  abort ();
	}
      break;

    case UNSPEC_VOLATILE:
      switch (XINT (x, 1))
	{
	case 0: /* alloc */
	  /* Alloc must always be the first instruction.  Currently, we
	     only emit it at the function start, so we don't need to worry
	     about emitting a stop bit before it.  */
	  need_barrier = rws_access_regno (AR_PFS_REGNUM, flags, pred);

	  new_flags.is_write = 1;
	  need_barrier |= rws_access_regno (REG_AR_CFM, new_flags, pred);
	  return need_barrier;

	case 1: /* blockage */
	case 2: /* insn group barrier */
	  return 0;

	case 5: /* set_bsp  */
	  need_barrier = 1;
          break;

	case 7: /* pred.rel.mutex */
	  return 0;

	default:
	  abort ();
	}
      break;

    case RETURN:
      new_flags.is_write = 0;
      need_barrier  = rws_access_regno (REG_RP, flags, pred);
      need_barrier |= rws_access_regno (AR_PFS_REGNUM, flags, pred);

      new_flags.is_write = 1;
      need_barrier |= rws_access_regno (AR_EC_REGNUM, new_flags, pred);
      need_barrier |= rws_access_regno (REG_AR_CFM, new_flags, pred);
      break;

    default:
      format_ptr = GET_RTX_FORMAT (GET_CODE (x));
      for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
	switch (format_ptr[i])
	  {
	  case '0':	/* unused field */
	  case 'i':	/* integer */
	  case 'n':	/* note */
	  case 'w':	/* wide integer */
	  case 's':	/* pointer to string */
	  case 'S':	/* optional pointer to string */
	    break;

	  case 'e':
	    if (rtx_needs_barrier (XEXP (x, i), flags, pred))
	      need_barrier = 1;
	    break;

	  case 'E':
	    for (j = XVECLEN (x, i) - 1; j >= 0; --j)
	      if (rtx_needs_barrier (XVECEXP (x, i, j), flags, pred))
		need_barrier = 1;
	    break;

	  default:
	    abort ();
	  }
    }
  return need_barrier;
}

/* INSNS is an chain of instructions.  Scan the chain, and insert stop bits
   as necessary to eliminate dependendencies.  */

static void
emit_insn_group_barriers (insns)
     rtx insns;
{
  rtx insn, prev_insn;

  memset (rws_sum, 0, sizeof (rws_sum));

  prev_insn = 0;
  for (insn = insns; insn; insn = NEXT_INSN (insn))
    {
      int need_barrier = 0;
      struct reg_flags flags;

      memset (&flags, 0, sizeof (flags));
      switch (GET_CODE (insn))
	{
	case NOTE:
	  /* For very small loops we can wind up with extra stop bits
	     inside the loop because of not putting a stop after the
	     assignment to ar.lc before the loop label.  */
	  /* ??? Ideally we'd do this for any register used in the first
	     insn group that's been written recently.  */
          if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_BEG)
	    {
	      need_barrier = rws_access_regno (AR_LC_REGNUM, flags, 0);
	      if (need_barrier)
		{
		  emit_insn_after (gen_insn_group_barrier (), insn);
		  memset (rws_sum, 0, sizeof(rws_sum));
		  prev_insn = NULL_RTX;
		}
	    }
	  break;

	case CALL_INSN:
	  flags.is_branch = 1;
	  memset (rws_insn, 0, sizeof (rws_insn));
	  need_barrier = rtx_needs_barrier (PATTERN (insn), flags, 0);

	  if (need_barrier)
	    {
	      /* PREV_INSN null can happen if the very first insn is a
		 volatile asm.  */
	      if (prev_insn)
		emit_insn_after (gen_insn_group_barrier (), prev_insn);
	      memcpy (rws_sum, rws_insn, sizeof (rws_sum));
	    }

	  /* A call must end a group, otherwise the assembler might pack
	     it in with a following branch and then the function return
	     goes to the wrong place.  Do this unconditionally for 
	     unconditional calls, simply because it (1) looks nicer and
	     (2) keeps the data structures more accurate for the insns
	     following the call.  */

	  need_barrier = 1;
	  if (GET_CODE (PATTERN (insn)) == COND_EXEC)
	    {
	      rtx next_insn = insn;
	      do
		next_insn = next_nonnote_insn (next_insn);
	      while (next_insn
		     && GET_CODE (next_insn) == INSN
		     && (GET_CODE (PATTERN (next_insn)) == USE
			 || GET_CODE (PATTERN (next_insn)) == CLOBBER));
	      if (next_insn && GET_CODE (next_insn) != JUMP_INSN)
		need_barrier = 0;
	    }
	  if (need_barrier)
	    {
	      emit_insn_after (gen_insn_group_barrier (), insn);
	      memset (rws_sum, 0, sizeof (rws_sum));
	      prev_insn = NULL_RTX;
	    }
	  break;
	
	case JUMP_INSN:
	  flags.is_branch = 1;
	  /* FALLTHRU */

	case INSN:
	  if (GET_CODE (PATTERN (insn)) == USE)
	    /* Don't care about USE "insns"---those are used to
	       indicate to the optimizer that it shouldn't get rid of
	       certain operations.  */
	    break;
	  else
	    {
	      rtx pat = PATTERN (insn);

	      /* Ug.  Hack hacks hacked elsewhere.  */
	      switch (INSN_CODE (insn))
		{
		  /* We play dependency tricks with the epilogue in order
		     to get proper schedules.  Undo this for dv analysis.  */
		case CODE_FOR_epilogue_deallocate_stack:
		  pat = XVECEXP (pat, 0, 0);
		  break;

		  /* The pattern we use for br.cloop confuses the code above.
		     The second element of the vector is representative.  */
		case CODE_FOR_doloop_end_internal:
		  pat = XVECEXP (pat, 0, 1);
		  break;

		  /* We include ar.unat in the rtl pattern so that sched2
		     does not move the ar.unat save/restore after/before
		     a gr spill/fill.  However, we special case these
		     insns based on their unspec number so as to model
		     their precise ar.unat bit operations.  If we pass on
		     the use/clobber of the whole ar.unat register we'll
		     waste this effort.  */
		case CODE_FOR_gr_spill_internal:
		case CODE_FOR_gr_restore_internal:
		  pat = XVECEXP (pat, 0, 0);
		  break;

		default:
		  break;
		}

	      memset (rws_insn, 0, sizeof (rws_insn));
	      need_barrier |= rtx_needs_barrier (pat, flags, 0);

	      /* Check to see if the previous instruction was a volatile
		 asm.  */
	      if (! need_barrier)
		need_barrier = rws_access_regno (REG_VOLATILE, flags, 0);

	      if (need_barrier)
		{
		  /* PREV_INSN null can happen if the very first insn is a
		     volatile asm.  */
		  if (prev_insn)
		    emit_insn_after (gen_insn_group_barrier (), prev_insn);
		  memcpy (rws_sum, rws_insn, sizeof (rws_sum));
		}
	      prev_insn = insn;
	    }
	  break;

	case BARRIER:
	  /* A barrier doesn't imply an instruction group boundary.  */
	  break;

	case CODE_LABEL:
	  /* Leave prev_insn alone so the barrier gets generated in front
	     of the label, if one is needed.  */
	  break;

	default:
	  abort ();
	}
    }
}

/* Emit pseudo-ops for the assembler to describe predicate relations.
   At present this assumes that we only consider predicate pairs to
   be mutex, and that the assembler can deduce proper values from
   straight-line code.  */

static void
emit_predicate_relation_info (insns)
     rtx insns;
{
  int i;

  /* Make sure the CFG and global_live_at_start are correct.  */
  find_basic_blocks (insns, max_reg_num (), NULL);
  life_analysis (insns, NULL, 0);

  for (i = n_basic_blocks - 1; i >= 0; --i)
    {
      basic_block bb = BASIC_BLOCK (i);
      int r;
      rtx head = bb->head;

      /* We only need such notes at code labels.  */
      if (GET_CODE (head) != CODE_LABEL)
	continue;
      if (GET_CODE (NEXT_INSN (head)) == NOTE
	  && NOTE_LINE_NUMBER (NEXT_INSN (head)) == NOTE_INSN_BASIC_BLOCK)
	head = NEXT_INSN (head);

      for (r = PR_REG (0); r < PR_REG (64); r += 2)
	if (REGNO_REG_SET_P (bb->global_live_at_start, r))
	  {
	    rtx p = gen_rtx_REG (CCmode, r);
	    rtx n = emit_insn_after (gen_pred_rel_mutex (p), head);
	    if (head == bb->end)
	      bb->end = n;
	    head = n;
	  }
    }
}

/* Perform machine dependent operations on the rtl chain INSNS.  */

void
ia64_reorg (insns)
     rtx insns;
{
  /* If optimizing, we'll have split before scheduling.  */
  if (optimize == 0)
    split_all_insns (0);

  emit_predicate_relation_info (insns);
  emit_insn_group_barriers (insns);
}

/* Return true if REGNO is used by the epilogue.  */

int
ia64_epilogue_uses (regno)
     int regno;
{
  /* When a function makes a call through a function descriptor, we
     will write a (potentially) new value to "gp".  After returning
     from such a call, we need to make sure the function restores the
     original gp-value, even if the function itself does not use the
     gp anymore.  */
  if (regno == R_GR (1)
      && TARGET_CONST_GP
      && !(TARGET_AUTO_PIC || TARGET_NO_PIC))
    return 1;

  /* For functions defined with the syscall_linkage attribute, all input
     registers are marked as live at all function exits.  This prevents the
     register allocator from using the input registers, which in turn makes it
     possible to restart a system call after an interrupt without having to
     save/restore the input registers.  */

  if (IN_REGNO_P (regno)
      && (regno < IN_REG (current_function_args_info.words))
      && lookup_attribute ("syscall_linkage",
			   TYPE_ATTRIBUTES (TREE_TYPE (current_function_decl))))
    return 1;

  /* Conditional return patterns can't represent the use of `b0' as
     the return address, so we force the value live this way.  */
  if (regno == R_BR (0))
    return 1;

  if (regs_ever_live[AR_LC_REGNUM] && regno == AR_LC_REGNUM)
    return 1;
  if (! current_function_is_leaf && regno == AR_PFS_REGNUM)
    return 1;
  if (TEST_HARD_REG_BIT (current_frame_info.mask, AR_UNAT_REGNUM)
      && regno == AR_UNAT_REGNUM)
    return 1;

  return 0;
}

/* Return true if IDENTIFIER is a valid attribute for TYPE.  */

int
ia64_valid_type_attribute (type, attributes, identifier, args)
     tree type;
     tree attributes ATTRIBUTE_UNUSED;
     tree identifier;
     tree args;
{
  /* We only support an attribute for function calls.  */

  if (TREE_CODE (type) != FUNCTION_TYPE
      && TREE_CODE (type) != METHOD_TYPE)
    return 0;

  /* The "syscall_linkage" attribute says the callee is a system call entry
     point.  This affects ia64_epilogue_uses.  */

  if (is_attribute_p ("syscall_linkage", identifier))
    return args == NULL_TREE;

  return 0;
}

/* For ia64, SYMBOL_REF_FLAG set means that it is a function.

   We add @ to the name if this goes in small data/bss.  We can only put
   a variable in small data/bss if it is defined in this module or a module
   that we are statically linked with.  We can't check the second condition,
   but TREE_STATIC gives us the first one.  */

/* ??? If we had IPA, we could check the second condition.  We could support
   programmer added section attributes if the variable is not defined in this
   module.  */

/* ??? See the v850 port for a cleaner way to do this.  */

/* ??? We could also support own long data here.  Generating movl/add/ld8
   instead of addl,ld8/ld8.  This makes the code bigger, but should make the
   code faster because there is one less load.  This also includes incomplete
   types which can't go in sdata/sbss.  */

/* ??? See select_section.  We must put short own readonly variables in
   sdata/sbss instead of the more natural rodata, because we can't perform
   the DECL_READONLY_SECTION test here.  */

extern struct obstack * saveable_obstack;

void
ia64_encode_section_info (decl)
     tree decl;
{
  const char *symbol_str;

  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      SYMBOL_REF_FLAG (XEXP (DECL_RTL (decl), 0)) = 1;
      return;
    }

  /* Careful not to prod global register variables.  */
  if (TREE_CODE (decl) != VAR_DECL
      || GET_CODE (DECL_RTL (decl)) != MEM
      || GET_CODE (XEXP (DECL_RTL (decl), 0)) != SYMBOL_REF)
    return;
    
  symbol_str = XSTR (XEXP (DECL_RTL (decl), 0), 0);

  /* We assume that -fpic is used only to create a shared library (dso).
     With -fpic, no global data can ever be sdata.
     Without -fpic, global common uninitialized data can never be sdata, since
     it can unify with a real definition in a dso.  */
  /* ??? Actually, we can put globals in sdata, as long as we don't use gprel
     to access them.  The linker may then be able to do linker relaxation to
     optimize references to them.  Currently sdata implies use of gprel.  */
  if (! TARGET_NO_SDATA
      && TREE_STATIC (decl)
      && ! (DECL_ONE_ONLY (decl) || DECL_WEAK (decl))
      && ! (TREE_PUBLIC (decl)
	    && (flag_pic
		|| (DECL_COMMON (decl)
		    && (DECL_INITIAL (decl) == 0
			|| DECL_INITIAL (decl) == error_mark_node))))
      /* Either the variable must be declared without a section attribute,
	 or the section must be sdata or sbss.  */
      && (DECL_SECTION_NAME (decl) == 0
	  || ! strcmp (TREE_STRING_POINTER (DECL_SECTION_NAME (decl)),
		       ".sdata")
	  || ! strcmp (TREE_STRING_POINTER (DECL_SECTION_NAME (decl)),
		       ".sbss")))
    {
      HOST_WIDE_INT size = int_size_in_bytes (TREE_TYPE (decl));

      /* If the variable has already been defined in the output file, then it
	 is too late to put it in sdata if it wasn't put there in the first
	 place.  The test is here rather than above, because if it is already
	 in sdata, then it can stay there.  */

      if (TREE_ASM_WRITTEN (decl))
	;

      /* If this is an incomplete type with size 0, then we can't put it in
	 sdata because it might be too big when completed.  */
      else if (size > 0
	       && size <= (HOST_WIDE_INT) ia64_section_threshold
	       && symbol_str[0] != SDATA_NAME_FLAG_CHAR)
	{
	  size_t len = strlen (symbol_str);
	  char *newstr;

	  if (ggc_p)
	    newstr = ggc_alloc_string (NULL, len + 1);
	  else
	    newstr = obstack_alloc (saveable_obstack, len + 2);

	  *newstr = SDATA_NAME_FLAG_CHAR;
	  memcpy (newstr + 1, symbol_str, len + 1);

	  XSTR (XEXP (DECL_RTL (decl), 0), 0) = newstr;
	}
    }
  /* This decl is marked as being in small data/bss but it shouldn't
     be; one likely explanation for this is that the decl has been
     moved into a different section from the one it was in when
     ENCODE_SECTION_INFO was first called.  Remove the '@'.*/
  else if (symbol_str[0] == SDATA_NAME_FLAG_CHAR)
    {
      if (ggc_p)
	XSTR (XEXP (DECL_RTL (decl), 0), 0)
	  = ggc_alloc_string (symbol_str + 1, -1);
      else
        XSTR (XEXP (DECL_RTL (decl), 0), 0) = symbol_str + 1;
    }
}

/* Output assmebly directives for prologue regions.  */

/* This function processes a SET pattern looking for specific patterns
   which result in emitting an assembly directive required for unwinding.  */

static int
process_set (asm_out_file, pat)
     FILE *asm_out_file;
     rtx pat;
{
  rtx src = SET_SRC (pat);
  rtx dest = SET_DEST (pat);
  int src_regno, dest_regno;

  /* Look for the ALLOC insn.  */
  if (GET_CODE (src) == UNSPEC_VOLATILE
      && XINT (src, 1) == 0
      && GET_CODE (dest) == REG)
    {
      dest_regno = REGNO (dest);

      /* If this isn't the final destination for ar.pfs, the alloc
	 shouldn't have been marked frame related.  */
      if (dest_regno != current_frame_info.reg_save_ar_pfs)
	abort ();

      fprintf (asm_out_file, "\t.save ar.pfs, r%d\n",
	       ia64_dbx_register_number (dest_regno));
      return 1;
    }

  /* Look for SP = .... */
  if (GET_CODE (dest) == REG && REGNO (dest) == STACK_POINTER_REGNUM)
    {
      if (GET_CODE (src) == PLUS)
        {
	  rtx op0 = XEXP (src, 0);
	  rtx op1 = XEXP (src, 1);
	  if (op0 == dest && GET_CODE (op1) == CONST_INT)
	    {
	      if (INTVAL (op1) < 0)
		{
		  fputs ("\t.fframe ", asm_out_file);
		  fprintf (asm_out_file, HOST_WIDE_INT_PRINT_DEC,
			   -INTVAL (op1));
		  fputc ('\n', asm_out_file);
		}
	      else
		fprintf (asm_out_file, "\t.restore sp\n");
	    }
	  else
	    abort ();
	}
      else if (GET_CODE (src) == REG
	       && REGNO (src) == HARD_FRAME_POINTER_REGNUM)
	fprintf (asm_out_file, "\t.restore sp\n");
      else
	abort ();

      return 1;
    }

  /* Register move we need to look at.  */
  if (GET_CODE (dest) == REG && GET_CODE (src) == REG)
    {
      src_regno = REGNO (src);
      dest_regno = REGNO (dest);

      switch (src_regno)
	{
	case BR_REG (0):
	  /* Saving return address pointer.  */
	  if (dest_regno != current_frame_info.reg_save_b0)
	    abort ();
	  fprintf (asm_out_file, "\t.save rp, r%d\n",
		   ia64_dbx_register_number (dest_regno));
	  return 1;

	case PR_REG (0):
	  if (dest_regno != current_frame_info.reg_save_pr)
	    abort ();
	  fprintf (asm_out_file, "\t.save pr, r%d\n",
		   ia64_dbx_register_number (dest_regno));
	  return 1;

	case AR_UNAT_REGNUM:
	  if (dest_regno != current_frame_info.reg_save_ar_unat)
	    abort ();
	  fprintf (asm_out_file, "\t.save ar.unat, r%d\n",
		   ia64_dbx_register_number (dest_regno));
	  return 1;

	case AR_LC_REGNUM:
	  if (dest_regno != current_frame_info.reg_save_ar_lc)
	    abort ();
	  fprintf (asm_out_file, "\t.save ar.lc, r%d\n",
		   ia64_dbx_register_number (dest_regno));
	  return 1;

	case STACK_POINTER_REGNUM:
	  if (dest_regno != HARD_FRAME_POINTER_REGNUM
	      || ! frame_pointer_needed)
	    abort ();
	  fprintf (asm_out_file, "\t.vframe r%d\n",
		   ia64_dbx_register_number (dest_regno));
	  return 1;

	default:
	  /* Everything else should indicate being stored to memory.  */
	  abort ();
	}
    }

  /* Memory store we need to look at.  */
  if (GET_CODE (dest) == MEM && GET_CODE (src) == REG)
    {
      long off;
      rtx base;
      const char *saveop;

      if (GET_CODE (XEXP (dest, 0)) == REG)
	{
	  base = XEXP (dest, 0);
	  off = 0;
	}
      else if (GET_CODE (XEXP (dest, 0)) == PLUS
	       && GET_CODE (XEXP (XEXP (dest, 0), 1)) == CONST_INT)
	{
	  base = XEXP (XEXP (dest, 0), 0);
	  off = INTVAL (XEXP (XEXP (dest, 0), 1));
	}
      else
	abort ();

      if (base == hard_frame_pointer_rtx)
	{
	  saveop = ".savepsp";
	  off = - off;
	}
      else if (base == stack_pointer_rtx)
	saveop = ".savesp";
      else
	abort ();

      src_regno = REGNO (src);
      switch (src_regno)
	{
	case BR_REG (0):
	  if (current_frame_info.reg_save_b0 != 0)
	    abort ();
	  fprintf (asm_out_file, "\t%s rp, %ld\n", saveop, off);
	  return 1;

	case PR_REG (0):
	  if (current_frame_info.reg_save_pr != 0)
	    abort ();
	  fprintf (asm_out_file, "\t%s pr, %ld\n", saveop, off);
	  return 1;

	case AR_LC_REGNUM:
	  if (current_frame_info.reg_save_ar_lc != 0)
	    abort ();
	  fprintf (asm_out_file, "\t%s ar.lc, %ld\n", saveop, off);
	  return 1;

	case AR_PFS_REGNUM:
	  if (current_frame_info.reg_save_ar_pfs != 0)
	    abort ();
	  fprintf (asm_out_file, "\t%s ar.pfs, %ld\n", saveop, off);
	  return 1;

	case AR_UNAT_REGNUM:
	  if (current_frame_info.reg_save_ar_unat != 0)
	    abort ();
	  fprintf (asm_out_file, "\t%s ar.unat, %ld\n", saveop, off);
	  return 1;

	case GR_REG (4):
	case GR_REG (5):
	case GR_REG (6):
	case GR_REG (7):
	  fprintf (asm_out_file, "\t.save.g 0x%x\n",
		   1 << (src_regno - GR_REG (4)));
	  return 1;

	case BR_REG (1):
	case BR_REG (2):
	case BR_REG (3):
	case BR_REG (4):
	case BR_REG (5):
	  fprintf (asm_out_file, "\t.save.b 0x%x\n",
		   1 << (src_regno - BR_REG (1)));
	  return 1;

	case FR_REG (2):
	case FR_REG (3):
	case FR_REG (4):
	case FR_REG (5):
	  fprintf (asm_out_file, "\t.save.f 0x%x\n",
		   1 << (src_regno - FR_REG (2)));
	  return 1;

	case FR_REG (16): case FR_REG (17): case FR_REG (18): case FR_REG (19):
	case FR_REG (20): case FR_REG (21): case FR_REG (22): case FR_REG (23):
	case FR_REG (24): case FR_REG (25): case FR_REG (26): case FR_REG (27):
	case FR_REG (28): case FR_REG (29): case FR_REG (30): case FR_REG (31):
	  fprintf (asm_out_file, "\t.save.gf 0x0, 0x%x\n",
		   1 << (src_regno - FR_REG (12)));
	  return 1;

	default:
	  return 0;
	}
    }

  return 0;
}


/* This function looks at a single insn and emits any directives
   required to unwind this insn.  */
void
process_for_unwind_directive (asm_out_file, insn)
     FILE *asm_out_file;
     rtx insn;
{
  if ((flag_unwind_tables
       || (flag_exceptions && !exceptions_via_longjmp))
      && RTX_FRAME_RELATED_P (insn))
    {
      rtx pat;

      pat = find_reg_note (insn, REG_FRAME_RELATED_EXPR, NULL_RTX);
      if (pat)
	pat = XEXP (pat, 0);
      else
	pat = PATTERN (insn);

      switch (GET_CODE (pat))
        {
	case SET:
	  process_set (asm_out_file, pat);
	  break;

	case PARALLEL:
	  {
	    int par_index;
	    int limit = XVECLEN (pat, 0);
	    for (par_index = 0; par_index < limit; par_index++)
	      {
		rtx x = XVECEXP (pat, 0, par_index);
		if (GET_CODE (x) == SET)
		  process_set (asm_out_file, x);
	      }
	    break;
	  }

	default:
	  abort ();
	}
    }
}


void
ia64_init_builtins ()
{
  tree psi_type_node = build_pointer_type (integer_type_node);
  tree pdi_type_node = build_pointer_type (long_integer_type_node);
  tree endlink = tree_cons (NULL_TREE, void_type_node, NULL_TREE);

  /* __sync_val_compare_and_swap_si, __sync_bool_compare_and_swap_si */
  tree si_ftype_psi_si_si
    = build_function_type (integer_type_node,
                           tree_cons (NULL_TREE, psi_type_node,
                                      tree_cons (NULL_TREE, integer_type_node,
                                                 tree_cons (NULL_TREE,
							    integer_type_node,
                                                            endlink))));

  /* __sync_val_compare_and_swap_di, __sync_bool_compare_and_swap_di */
  tree di_ftype_pdi_di_di
    = build_function_type (long_integer_type_node,
                           tree_cons (NULL_TREE, pdi_type_node,
                                      tree_cons (NULL_TREE,
						 long_integer_type_node,
                                                 tree_cons (NULL_TREE,
							long_integer_type_node,
							endlink))));
  /* __sync_synchronize */
  tree void_ftype_void
    = build_function_type (void_type_node, endlink);

  /* __sync_lock_test_and_set_si */
  tree si_ftype_psi_si
    = build_function_type (integer_type_node,
                           tree_cons (NULL_TREE, psi_type_node,
                           tree_cons (NULL_TREE, integer_type_node, endlink)));

  /* __sync_lock_test_and_set_di */
  tree di_ftype_pdi_di
    = build_function_type (long_integer_type_node,
                           tree_cons (NULL_TREE, pdi_type_node,
                           tree_cons (NULL_TREE, long_integer_type_node,
				      endlink)));

  /* __sync_lock_release_si */
  tree void_ftype_psi
    = build_function_type (void_type_node, tree_cons (NULL_TREE, psi_type_node,
						      endlink));

  /* __sync_lock_release_di */
  tree void_ftype_pdi
    = build_function_type (void_type_node, tree_cons (NULL_TREE, pdi_type_node,
						      endlink));

#define def_builtin(name, type, code) \
  builtin_function ((name), (type), (code), BUILT_IN_MD, NULL_PTR)

  def_builtin ("__sync_val_compare_and_swap_si", si_ftype_psi_si_si,
	       IA64_BUILTIN_VAL_COMPARE_AND_SWAP_SI);
  def_builtin ("__sync_val_compare_and_swap_di", di_ftype_pdi_di_di,
	       IA64_BUILTIN_VAL_COMPARE_AND_SWAP_DI);
  def_builtin ("__sync_bool_compare_and_swap_si", si_ftype_psi_si_si,
	       IA64_BUILTIN_BOOL_COMPARE_AND_SWAP_SI);
  def_builtin ("__sync_bool_compare_and_swap_di", di_ftype_pdi_di_di,
	       IA64_BUILTIN_BOOL_COMPARE_AND_SWAP_DI);

  def_builtin ("__sync_synchronize", void_ftype_void,
	       IA64_BUILTIN_SYNCHRONIZE);

  def_builtin ("__sync_lock_test_and_set_si", si_ftype_psi_si,
	       IA64_BUILTIN_LOCK_TEST_AND_SET_SI);
  def_builtin ("__sync_lock_test_and_set_di", di_ftype_pdi_di,
	       IA64_BUILTIN_LOCK_TEST_AND_SET_DI);
  def_builtin ("__sync_lock_release_si", void_ftype_psi,
	       IA64_BUILTIN_LOCK_RELEASE_SI);
  def_builtin ("__sync_lock_release_di", void_ftype_pdi,
	       IA64_BUILTIN_LOCK_RELEASE_DI);

  def_builtin ("__builtin_ia64_bsp",
	       build_function_type (ptr_type_node, endlink),
	       IA64_BUILTIN_BSP);

  def_builtin ("__builtin_ia64_flushrs", 
	       build_function_type (void_type_node, endlink), 
	       IA64_BUILTIN_FLUSHRS);

  def_builtin ("__sync_fetch_and_add_si", si_ftype_psi_si,
	       IA64_BUILTIN_FETCH_AND_ADD_SI);
  def_builtin ("__sync_fetch_and_sub_si", si_ftype_psi_si,
	       IA64_BUILTIN_FETCH_AND_SUB_SI);
  def_builtin ("__sync_fetch_and_or_si", si_ftype_psi_si,
	       IA64_BUILTIN_FETCH_AND_OR_SI);
  def_builtin ("__sync_fetch_and_and_si", si_ftype_psi_si,
	       IA64_BUILTIN_FETCH_AND_AND_SI);
  def_builtin ("__sync_fetch_and_xor_si", si_ftype_psi_si,
	       IA64_BUILTIN_FETCH_AND_XOR_SI);
  def_builtin ("__sync_fetch_and_nand_si", si_ftype_psi_si,
	       IA64_BUILTIN_FETCH_AND_NAND_SI);

  def_builtin ("__sync_add_and_fetch_si", si_ftype_psi_si,
	       IA64_BUILTIN_ADD_AND_FETCH_SI);
  def_builtin ("__sync_sub_and_fetch_si", si_ftype_psi_si,
	       IA64_BUILTIN_SUB_AND_FETCH_SI);
  def_builtin ("__sync_or_and_fetch_si", si_ftype_psi_si,
	       IA64_BUILTIN_OR_AND_FETCH_SI);
  def_builtin ("__sync_and_and_fetch_si", si_ftype_psi_si,
	       IA64_BUILTIN_AND_AND_FETCH_SI);
  def_builtin ("__sync_xor_and_fetch_si", si_ftype_psi_si,
	       IA64_BUILTIN_XOR_AND_FETCH_SI);
  def_builtin ("__sync_nand_and_fetch_si", si_ftype_psi_si,
	       IA64_BUILTIN_NAND_AND_FETCH_SI);

  def_builtin ("__sync_fetch_and_add_di", di_ftype_pdi_di,
	       IA64_BUILTIN_FETCH_AND_ADD_DI);
  def_builtin ("__sync_fetch_and_sub_di", di_ftype_pdi_di,
	       IA64_BUILTIN_FETCH_AND_SUB_DI);
  def_builtin ("__sync_fetch_and_or_di", di_ftype_pdi_di,
	       IA64_BUILTIN_FETCH_AND_OR_DI);
  def_builtin ("__sync_fetch_and_and_di", di_ftype_pdi_di,
	       IA64_BUILTIN_FETCH_AND_AND_DI);
  def_builtin ("__sync_fetch_and_xor_di", di_ftype_pdi_di,
	       IA64_BUILTIN_FETCH_AND_XOR_DI);
  def_builtin ("__sync_fetch_and_nand_di", di_ftype_pdi_di,
	       IA64_BUILTIN_FETCH_AND_NAND_DI);

  def_builtin ("__sync_add_and_fetch_di", di_ftype_pdi_di,
	       IA64_BUILTIN_ADD_AND_FETCH_DI);
  def_builtin ("__sync_sub_and_fetch_di", di_ftype_pdi_di,
	       IA64_BUILTIN_SUB_AND_FETCH_DI);
  def_builtin ("__sync_or_and_fetch_di", di_ftype_pdi_di,
	       IA64_BUILTIN_OR_AND_FETCH_DI);
  def_builtin ("__sync_and_and_fetch_di", di_ftype_pdi_di,
	       IA64_BUILTIN_AND_AND_FETCH_DI);
  def_builtin ("__sync_xor_and_fetch_di", di_ftype_pdi_di,
	       IA64_BUILTIN_XOR_AND_FETCH_DI);
  def_builtin ("__sync_nand_and_fetch_di", di_ftype_pdi_di,
	       IA64_BUILTIN_NAND_AND_FETCH_DI);

#undef def_builtin
}

/* Expand fetch_and_op intrinsics.  The basic code sequence is:

     mf
     tmp = [ptr];
     do {
       ret = tmp;
       ar.ccv = tmp;
       tmp <op>= value;
       cmpxchgsz.acq tmp = [ptr], tmp
     } while (tmp != ret)
*/

static rtx
ia64_expand_fetch_and_op (binoptab, mode, arglist, target)
     optab binoptab;
     enum machine_mode mode;
     tree arglist;
     rtx target;
{
  rtx ret, label, tmp, ccv, insn, mem, value;
  tree arg0, arg1;

  arg0 = TREE_VALUE (arglist);
  arg1 = TREE_VALUE (TREE_CHAIN (arglist));
  mem = expand_expr (arg0, NULL_RTX, Pmode, 0);
  value = expand_expr (arg1, NULL_RTX, mode, 0);

  mem = gen_rtx_MEM (mode, force_reg (Pmode, mem));
  MEM_VOLATILE_P (mem) = 1;

  if (target && register_operand (target, mode))
    ret = target;
  else
    ret = gen_reg_rtx (mode);

  emit_insn (gen_mf ());

  /* Special case for fetchadd instructions.  */
  if (binoptab == add_optab && fetchadd_operand (value, VOIDmode))
    {
      if (mode == SImode)
        insn = gen_fetchadd_acq_si (ret, mem, value);
      else
        insn = gen_fetchadd_acq_di (ret, mem, value);
      emit_insn (insn);
      return ret;
    }

  tmp = gen_reg_rtx (mode);
  ccv = gen_rtx_REG (mode, AR_CCV_REGNUM);
  emit_move_insn (tmp, mem);

  label = gen_label_rtx ();
  emit_label (label);
  emit_move_insn (ret, tmp);
  emit_move_insn (ccv, tmp);

  /* Perform the specific operation.  Special case NAND by noticing
     one_cmpl_optab instead.  */
  if (binoptab == one_cmpl_optab)
    {
      tmp = expand_unop (mode, binoptab, tmp, NULL, OPTAB_WIDEN);
      binoptab = and_optab;
    }
  tmp = expand_binop (mode, binoptab, tmp, value, tmp, 1, OPTAB_WIDEN);

  if (mode == SImode)
    insn = gen_cmpxchg_acq_si (tmp, mem, tmp, ccv);
  else
    insn = gen_cmpxchg_acq_di (tmp, mem, tmp, ccv);
  emit_insn (insn);

  emit_cmp_and_jump_insns (tmp, ret, NE, 0, mode, 1, 0, label);

  return ret;
}

/* Expand op_and_fetch intrinsics.  The basic code sequence is:

     mf
     tmp = [ptr];
     do {
       old = tmp;
       ar.ccv = tmp;
       ret = tmp + value;
       cmpxchgsz.acq tmp = [ptr], ret
     } while (tmp != old)
*/

static rtx
ia64_expand_op_and_fetch (binoptab, mode, arglist, target)
     optab binoptab;
     enum machine_mode mode;
     tree arglist;
     rtx target;
{
  rtx old, label, tmp, ret, ccv, insn, mem, value;
  tree arg0, arg1;

  arg0 = TREE_VALUE (arglist);
  arg1 = TREE_VALUE (TREE_CHAIN (arglist));
  mem = expand_expr (arg0, NULL_RTX, Pmode, 0);
  value = expand_expr (arg1, NULL_RTX, mode, 0);

  mem = gen_rtx_MEM (mode, force_reg (Pmode, mem));
  MEM_VOLATILE_P (mem) = 1;

  if (target && ! register_operand (target, mode))
    target = NULL_RTX;

  emit_insn (gen_mf ());
  tmp = gen_reg_rtx (mode);
  old = gen_reg_rtx (mode);
  ccv = gen_rtx_REG (mode, AR_CCV_REGNUM);

  emit_move_insn (tmp, mem);

  label = gen_label_rtx ();
  emit_label (label);
  emit_move_insn (old, tmp);
  emit_move_insn (ccv, tmp);

  /* Perform the specific operation.  Special case NAND by noticing
     one_cmpl_optab instead.  */
  if (binoptab == one_cmpl_optab)
    {
      tmp = expand_unop (mode, binoptab, tmp, NULL, OPTAB_WIDEN);
      binoptab = and_optab;
    }
  ret = expand_binop (mode, binoptab, tmp, value, target, 1, OPTAB_WIDEN);

  if (mode == SImode)
    insn = gen_cmpxchg_acq_si (tmp, mem, ret, ccv);
  else
    insn = gen_cmpxchg_acq_di (tmp, mem, ret, ccv);
  emit_insn (insn);

  emit_cmp_and_jump_insns (tmp, old, NE, 0, mode, 1, 0, label);

  return ret;
}

/* Expand val_ and bool_compare_and_swap.  For val_ we want:

     ar.ccv = oldval
     mf
     cmpxchgsz.acq ret = [ptr], newval, ar.ccv
     return ret

   For bool_ it's the same except return ret == oldval.
*/

static rtx
ia64_expand_compare_and_swap (mode, boolp, arglist, target)
     enum machine_mode mode;
     int boolp;
     tree arglist;
     rtx target;
{
  tree arg0, arg1, arg2;
  rtx mem, old, new, ccv, tmp, insn;

  arg0 = TREE_VALUE (arglist);
  arg1 = TREE_VALUE (TREE_CHAIN (arglist));
  arg2 = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist)));
  mem = expand_expr (arg0, NULL_RTX, Pmode, 0);
  old = expand_expr (arg1, NULL_RTX, mode, 0);
  new = expand_expr (arg2, NULL_RTX, mode, 0);

  mem = gen_rtx_MEM (mode, force_reg (Pmode, mem));
  MEM_VOLATILE_P (mem) = 1;

  if (! register_operand (old, mode))
    old = copy_to_mode_reg (mode, old);
  if (! register_operand (new, mode))
    new = copy_to_mode_reg (mode, new);

  if (! boolp && target && register_operand (target, mode))
    tmp = target;
  else
    tmp = gen_reg_rtx (mode);

  ccv = gen_rtx_REG (mode, AR_CCV_REGNUM);
  emit_move_insn (ccv, old);
  emit_insn (gen_mf ());
  if (mode == SImode)
    insn = gen_cmpxchg_acq_si (tmp, mem, new, ccv);
  else
    insn = gen_cmpxchg_acq_di (tmp, mem, new, ccv);
  emit_insn (insn);

  if (boolp)
    {
      if (! target)
	target = gen_reg_rtx (mode);
      return emit_store_flag_force (target, EQ, tmp, old, mode, 1, 1);
    }
  else
    return tmp;
}

/* Expand lock_test_and_set.  I.e. `xchgsz ret = [ptr], new'.  */

static rtx
ia64_expand_lock_test_and_set (mode, arglist, target)
     enum machine_mode mode;
     tree arglist;
     rtx target;
{
  tree arg0, arg1;
  rtx mem, new, ret, insn;

  arg0 = TREE_VALUE (arglist);
  arg1 = TREE_VALUE (TREE_CHAIN (arglist));
  mem = expand_expr (arg0, NULL_RTX, Pmode, 0);
  new = expand_expr (arg1, NULL_RTX, mode, 0);

  mem = gen_rtx_MEM (mode, force_reg (Pmode, mem));
  MEM_VOLATILE_P (mem) = 1;
  if (! register_operand (new, mode))
    new = copy_to_mode_reg (mode, new);

  if (target && register_operand (target, mode))
    ret = target;
  else
    ret = gen_reg_rtx (mode);

  if (mode == SImode)
    insn = gen_xchgsi (ret, mem, new);
  else
    insn = gen_xchgdi (ret, mem, new);
  emit_insn (insn);

  return ret;
}

/* Expand lock_release.  I.e. `stsz.rel [ptr] = r0'.  */

static rtx
ia64_expand_lock_release (mode, arglist, target)
     enum machine_mode mode;
     tree arglist;
     rtx target ATTRIBUTE_UNUSED;
{
  tree arg0;
  rtx mem;

  arg0 = TREE_VALUE (arglist);
  mem = expand_expr (arg0, NULL_RTX, Pmode, 0);

  mem = gen_rtx_MEM (mode, force_reg (Pmode, mem));
  MEM_VOLATILE_P (mem) = 1;

  emit_move_insn (mem, const0_rtx);

  return const0_rtx;
}

rtx
ia64_expand_builtin (exp, target, subtarget, mode, ignore)
     tree exp;
     rtx target;
     rtx subtarget ATTRIBUTE_UNUSED;
     enum machine_mode mode ATTRIBUTE_UNUSED;
     int ignore ATTRIBUTE_UNUSED;
{
  tree fndecl = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
  unsigned int fcode = DECL_FUNCTION_CODE (fndecl);
  tree arglist = TREE_OPERAND (exp, 1);

  switch (fcode)
    {
    case IA64_BUILTIN_BOOL_COMPARE_AND_SWAP_SI:
    case IA64_BUILTIN_VAL_COMPARE_AND_SWAP_SI:
    case IA64_BUILTIN_LOCK_TEST_AND_SET_SI:
    case IA64_BUILTIN_LOCK_RELEASE_SI:
    case IA64_BUILTIN_FETCH_AND_ADD_SI:
    case IA64_BUILTIN_FETCH_AND_SUB_SI:
    case IA64_BUILTIN_FETCH_AND_OR_SI:
    case IA64_BUILTIN_FETCH_AND_AND_SI:
    case IA64_BUILTIN_FETCH_AND_XOR_SI:
    case IA64_BUILTIN_FETCH_AND_NAND_SI:
    case IA64_BUILTIN_ADD_AND_FETCH_SI:
    case IA64_BUILTIN_SUB_AND_FETCH_SI:
    case IA64_BUILTIN_OR_AND_FETCH_SI:
    case IA64_BUILTIN_AND_AND_FETCH_SI:
    case IA64_BUILTIN_XOR_AND_FETCH_SI:
    case IA64_BUILTIN_NAND_AND_FETCH_SI:
      mode = SImode;
      break;

    case IA64_BUILTIN_BOOL_COMPARE_AND_SWAP_DI:
    case IA64_BUILTIN_VAL_COMPARE_AND_SWAP_DI:
    case IA64_BUILTIN_LOCK_TEST_AND_SET_DI:
    case IA64_BUILTIN_LOCK_RELEASE_DI:
    case IA64_BUILTIN_FETCH_AND_ADD_DI:
    case IA64_BUILTIN_FETCH_AND_SUB_DI:
    case IA64_BUILTIN_FETCH_AND_OR_DI:
    case IA64_BUILTIN_FETCH_AND_AND_DI:
    case IA64_BUILTIN_FETCH_AND_XOR_DI:
    case IA64_BUILTIN_FETCH_AND_NAND_DI:
    case IA64_BUILTIN_ADD_AND_FETCH_DI:
    case IA64_BUILTIN_SUB_AND_FETCH_DI:
    case IA64_BUILTIN_OR_AND_FETCH_DI:
    case IA64_BUILTIN_AND_AND_FETCH_DI:
    case IA64_BUILTIN_XOR_AND_FETCH_DI:
    case IA64_BUILTIN_NAND_AND_FETCH_DI:
      mode = DImode;
      break;

    default:
      break;
    }

  switch (fcode)
    {
    case IA64_BUILTIN_BOOL_COMPARE_AND_SWAP_SI:
    case IA64_BUILTIN_BOOL_COMPARE_AND_SWAP_DI:
      return ia64_expand_compare_and_swap (mode, 1, arglist, target);

    case IA64_BUILTIN_VAL_COMPARE_AND_SWAP_SI:
    case IA64_BUILTIN_VAL_COMPARE_AND_SWAP_DI:
      return ia64_expand_compare_and_swap (mode, 0, arglist, target);

    case IA64_BUILTIN_SYNCHRONIZE:
      emit_insn (gen_mf ());
      return const0_rtx;

    case IA64_BUILTIN_LOCK_TEST_AND_SET_SI:
    case IA64_BUILTIN_LOCK_TEST_AND_SET_DI:
      return ia64_expand_lock_test_and_set (mode, arglist, target);

    case IA64_BUILTIN_LOCK_RELEASE_SI:
    case IA64_BUILTIN_LOCK_RELEASE_DI:
      return ia64_expand_lock_release (mode, arglist, target);

    case IA64_BUILTIN_BSP:
      if (! target || ! register_operand (target, DImode))
	target = gen_reg_rtx (DImode);
      emit_insn (gen_bsp_value (target));
      return target;

    case IA64_BUILTIN_FLUSHRS:
      emit_insn (gen_flushrs ());
      return const0_rtx;

    case IA64_BUILTIN_FETCH_AND_ADD_SI:
    case IA64_BUILTIN_FETCH_AND_ADD_DI:
      return ia64_expand_fetch_and_op (add_optab, mode, arglist, target);

    case IA64_BUILTIN_FETCH_AND_SUB_SI:
    case IA64_BUILTIN_FETCH_AND_SUB_DI:
      return ia64_expand_fetch_and_op (sub_optab, mode, arglist, target);

    case IA64_BUILTIN_FETCH_AND_OR_SI:
    case IA64_BUILTIN_FETCH_AND_OR_DI:
      return ia64_expand_fetch_and_op (ior_optab, mode, arglist, target);

    case IA64_BUILTIN_FETCH_AND_AND_SI:
    case IA64_BUILTIN_FETCH_AND_AND_DI:
      return ia64_expand_fetch_and_op (and_optab, mode, arglist, target);

    case IA64_BUILTIN_FETCH_AND_XOR_SI:
    case IA64_BUILTIN_FETCH_AND_XOR_DI:
      return ia64_expand_fetch_and_op (xor_optab, mode, arglist, target);

    case IA64_BUILTIN_FETCH_AND_NAND_SI:
    case IA64_BUILTIN_FETCH_AND_NAND_DI:
      return ia64_expand_fetch_and_op (one_cmpl_optab, mode, arglist, target);

    case IA64_BUILTIN_ADD_AND_FETCH_SI:
    case IA64_BUILTIN_ADD_AND_FETCH_DI:
      return ia64_expand_op_and_fetch (add_optab, mode, arglist, target);

    case IA64_BUILTIN_SUB_AND_FETCH_SI:
    case IA64_BUILTIN_SUB_AND_FETCH_DI:
      return ia64_expand_op_and_fetch (sub_optab, mode, arglist, target);

    case IA64_BUILTIN_OR_AND_FETCH_SI:
    case IA64_BUILTIN_OR_AND_FETCH_DI:
      return ia64_expand_op_and_fetch (ior_optab, mode, arglist, target);

    case IA64_BUILTIN_AND_AND_FETCH_SI:
    case IA64_BUILTIN_AND_AND_FETCH_DI:
      return ia64_expand_op_and_fetch (and_optab, mode, arglist, target);

    case IA64_BUILTIN_XOR_AND_FETCH_SI:
    case IA64_BUILTIN_XOR_AND_FETCH_DI:
      return ia64_expand_op_and_fetch (xor_optab, mode, arglist, target);

    case IA64_BUILTIN_NAND_AND_FETCH_SI:
    case IA64_BUILTIN_NAND_AND_FETCH_DI:
      return ia64_expand_op_and_fetch (one_cmpl_optab, mode, arglist, target);

    default:
      break;
    }

  return NULL_RTX;
}
