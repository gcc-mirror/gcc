/* Definitions of target machine for Mitsubishi D30V.
   Copyright (C) 1997, 1998, 1999, 2000, 2001 Free Software Foundation, Inc.
   Contributed by Cygnus Solutions.

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
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "recog.h"
#include "expr.h"
#include "obstack.h"
#include "tm_p.h"
#include "except.h"
#include "function.h"
#include "toplev.h"
#include "integrate.h"
#include "ggc.h"
#include "target.h"
#include "target-def.h"
#include "langhooks.h"

static void d30v_print_operand_memory_reference PARAMS ((FILE *, rtx));
static void d30v_build_long_insn PARAMS ((HOST_WIDE_INT, HOST_WIDE_INT,
					  rtx, rtx));
static struct machine_function * d30v_init_machine_status PARAMS ((void));
static void d30v_output_function_prologue PARAMS ((FILE *, HOST_WIDE_INT));
static void d30v_output_function_epilogue PARAMS ((FILE *, HOST_WIDE_INT));
static int d30v_adjust_cost PARAMS ((rtx, rtx, rtx, int));
static int d30v_issue_rate PARAMS ((void));

/* Define the information needed to generate branch and scc insns.  This is
   stored from the compare operation.  */

struct rtx_def *d30v_compare_op0;
struct rtx_def *d30v_compare_op1;

/* Cached value of d30v_stack_info */
static d30v_stack_t *d30v_stack_cache = (d30v_stack_t *)0;

/* Values of the -mbranch-cost=n string.  */
int d30v_branch_cost = D30V_DEFAULT_BRANCH_COST;
const char *d30v_branch_cost_string = (const char *)0;

/* Values of the -mcond-exec=n string.  */
int d30v_cond_exec = D30V_DEFAULT_MAX_CONDITIONAL_EXECUTE;
const char *d30v_cond_exec_string = (const char *)0;

/* Whether or not a hard register can accept a register */
unsigned char hard_regno_mode_ok[ (int)MAX_MACHINE_MODE ][FIRST_PSEUDO_REGISTER];

/* Whether to try and avoid moves between two different modes */
unsigned char modes_tieable_p[ (NUM_MACHINE_MODES) * (NUM_MACHINE_MODES) ];

/* Map register number to smallest register class.  */
enum reg_class regno_reg_class[FIRST_PSEUDO_REGISTER];

/* Map class letter into register class */
enum reg_class reg_class_from_letter[256];

/* Initialize the GCC target structure.  */
#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.hword\t"
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.word\t"

#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE d30v_output_function_prologue
#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE d30v_output_function_epilogue
#undef TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST d30v_adjust_cost
#undef TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE d30v_issue_rate

struct gcc_target targetm = TARGET_INITIALIZER;

/* Sometimes certain combinations of command options do not make
   sense on a particular target machine.  You can define a macro
   `OVERRIDE_OPTIONS' to take account of this.  This macro, if
   defined, is executed once just after all the command options have
   been parsed.

   Don't use this macro to turn on various extra optimizations for
   `-O'.  That is what `OPTIMIZATION_OPTIONS' is for.  */

void
override_options ()
{
  int regno, i, ok_p;
  enum machine_mode mode1, mode2;

  /* Set up the branch cost information */
  if (d30v_branch_cost_string)
    d30v_branch_cost = atoi (d30v_branch_cost_string);

  /* Set up max # instructions to use with conditional execution */
  if (d30v_cond_exec_string)
    d30v_cond_exec = atoi (d30v_cond_exec_string);

  /* Setup hard_regno_mode_ok/modes_tieable_p */
  for (mode1 = VOIDmode;
       (int)mode1 < NUM_MACHINE_MODES;
       mode1 = (enum machine_mode)((int)mode1 + 1))
    {
      int size = GET_MODE_SIZE (mode1);
      int large_p = size > UNITS_PER_WORD;
      int int_p = GET_MODE_CLASS (mode1) == MODE_INT;

      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	{
	  if (mode1 == VOIDmode)
	    ok_p = FALSE;

	  else if (GPR_P (regno))
	    {
	      if (!large_p)
		ok_p = TRUE;
	      else
		ok_p = (((regno - GPR_FIRST) & 1) == 0);
	    }

	  else if (FLAG_P (regno))
	    ok_p = (mode1 == CCmode);

	  else if (CR_P (regno))
	    ok_p = int_p && !large_p;

	  else if (ACCUM_P (regno))
	    ok_p = (mode1 == DImode);

	  else if (SPECIAL_REG_P (regno))
	    ok_p = (mode1 == SImode);

	  else
	    ok_p = FALSE;

	  hard_regno_mode_ok[ (int)mode1 ][ regno ] = ok_p;
	}

      /* A C expression that is nonzero if it is desirable to choose
	 register allocation so as to avoid move instructions between a
	 value of mode MODE1 and a value of mode MODE2.

	 If `HARD_REGNO_MODE_OK (R, MODE1)' and `HARD_REGNO_MODE_OK (R,
	 MODE2)' are ever different for any R, then `MODES_TIEABLE_P (MODE1,
	 MODE2)' must be zero. */
      for (mode2 = VOIDmode;
	   (int)mode2 <= NUM_MACHINE_MODES;
	   mode2 = (enum machine_mode)((int)mode2 + 1))
	{
	  if (mode1 == mode2)
	    ok_p = TRUE;

#if 0
	  else if (GET_MODE_CLASS (mode1) == MODE_INT
		   && GET_MODE_SIZE (mode1) <= UNITS_PER_WORD
		   && GET_MODE_CLASS (mode2) == MODE_INT
		   && GET_MODE_SIZE (mode2) <= UNITS_PER_WORD)
	    ok_p = TRUE;
#endif

	  else
	    ok_p = FALSE;

	  modes_tieable_p[ ((int)mode1 * (NUM_MACHINE_MODES)) + (int)mode2 ] = ok_p;
	}
    }

#if 0
  for (mode1 = VOIDmode;
       (int)mode1 < NUM_MACHINE_MODES;
       mode1 = (enum machine_mode)((int)mode1 + 1))
    {
      for (mode2 = VOIDmode;
	   (int)mode2 <= NUM_MACHINE_MODES;
	   mode2 = (enum machine_mode)((int)mode2 + 1))
	{
	  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	    if (ok_p
		&& (hard_regno_mode_ok[(int)mode1][regno]
		    != hard_regno_mode_ok[(int)mode2][regno]))
	      error ("bad modes_tieable_p for register %s, mode1 %s, mode2 %s",
		     reg_names[regno], GET_MODE_NAME (mode1),
		     GET_MODE_NAME (mode2));
	}
    }
#endif

  /* A C expression whose value is a register class containing hard
     register REGNO.  In general there is more than one such class;
     choose a class which is "minimal", meaning that no smaller class
     also contains the register. */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    {
      enum reg_class class;

      if (GPR_P (regno))
	class = (IN_RANGE_P (regno, GPR_FIRST+2, GPR_FIRST+62)
		 && ((regno - GPR_FIRST) & 1) == 0) ? EVEN_REGS : GPR_REGS;

      else if (regno == FLAG_F0)
	class = F0_REGS;

      else if (regno == FLAG_F1)
	class = F1_REGS;

      else if (FLAG_P (regno))
	class = OTHER_FLAG_REGS;

      else if (ACCUM_P (regno))
	class = ACCUM_REGS;

      else if (regno == CR_RPT_C)
	class = REPEAT_REGS;

      else if (CR_P (regno))
	class = CR_REGS;

      else if (SPECIAL_REG_P (regno))
	class = GPR_REGS;

      else
	class = NO_REGS;

      regno_reg_class[regno] = class;

#if 0
      {
	static const char *const names[] = REG_CLASS_NAMES;
	fprintf (stderr, "Register %s class is %s, can hold modes", reg_names[regno], names[class]);
	for (mode1 = VOIDmode;
	     (int)mode1 < NUM_MACHINE_MODES;
	     mode1 = (enum machine_mode)((int)mode1 + 1))
	  {
	    if (hard_regno_mode_ok[ (int)mode1 ][ regno ])
	      fprintf (stderr, " %s", GET_MODE_NAME (mode1));
	  }
	fprintf (stderr, "\n");
      }
#endif
    }

  /* A C expression which defines the machine-dependent operand
     constraint letters for register classes.  If CHAR is such a
     letter, the value should be the register class corresponding to
     it.  Otherwise, the value should be `NO_REGS'.  The register
     letter `r', corresponding to class `GENERAL_REGS', will not be
     passed to this macro; you do not need to handle it.

     The following letters are unavailable, due to being used as
     constraints:
	'0'..'9'
	'<', '>'
	'E', 'F', 'G', 'H'
	'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P'
	'Q', 'R', 'S', 'T', 'U'
	'V', 'X'
	'g', 'i', 'm', 'n', 'o', 'p', 'r', 's' */

  for (i = 0; i < 256; i++)
    reg_class_from_letter[i] = NO_REGS;

  reg_class_from_letter['a'] = ACCUM_REGS;
  reg_class_from_letter['b'] = BR_FLAG_REGS;
  reg_class_from_letter['c'] = CR_REGS;
  reg_class_from_letter['d'] = GPR_REGS;
  reg_class_from_letter['e'] = EVEN_REGS;
  reg_class_from_letter['f'] = FLAG_REGS;
  reg_class_from_letter['l'] = REPEAT_REGS;
  reg_class_from_letter['x'] = F0_REGS;
  reg_class_from_letter['y'] = F1_REGS;
  reg_class_from_letter['z'] = OTHER_FLAG_REGS;
}


/* Return true if a memory operand is a short memory operand.  */

int
short_memory_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) != MEM)
    return FALSE;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  return (d30v_legitimate_address_p (mode, XEXP (op, 0), reload_completed)
	  == 1);
}

/* Return true if a memory operand is a long operand.  */

int
long_memory_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) != MEM)
    return FALSE;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  return (d30v_legitimate_address_p (mode, XEXP (op, 0), reload_completed)
	  == 2);
}

/* Return true if a memory operand is valid for the D30V.  */

int
d30v_memory_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) != MEM)
    return FALSE;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  return (d30v_legitimate_address_p (mode, XEXP (op, 0), reload_completed)
	  != 0);
}

/* Return true if a memory operand uses a single register for the
   address.  */

int
single_reg_memory_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  rtx addr;

  if (GET_CODE (op) != MEM)
    return FALSE;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  addr = XEXP (op, 0);
  if (! d30v_legitimate_address_p (mode, addr, reload_completed))
    return FALSE;

  if (GET_CODE (addr) == SUBREG)
    addr = SUBREG_REG (addr);

  return (GET_CODE (addr) == REG);
}

/* Return true if a memory operand uses a constant address.  */

int
const_addr_memory_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) != MEM)
    return FALSE;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (! d30v_legitimate_address_p (mode, XEXP (op, 0), reload_completed))
    return FALSE;

  switch (GET_CODE (XEXP (op, 0)))
    {
    default:
      break;

    case SYMBOL_REF:
    case LABEL_REF:
    case CONST_INT:
    case CONST:
      return TRUE;
    }

  return FALSE;
}

/* Return true if operand is a memory reference suitable for a call.  */

int
call_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) != MEM)
    return FALSE;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (! d30v_legitimate_address_p (mode, XEXP (op, 0), reload_completed))
    return FALSE;

  switch (GET_CODE (XEXP (op, 0)))
    {
    default:
      break;

    case SUBREG:
      op = SUBREG_REG (op);
      if (GET_CODE (op) != REG)
	return FALSE;

      /* fall through */

    case REG:
      return (GPR_OR_PSEUDO_P (REGNO (XEXP (op, 0))));

    case SYMBOL_REF:
    case LABEL_REF:
    case CONST_INT:
    case CONST:
      return TRUE;
    }

  return FALSE;
}

/* Return true if operand is a GPR register.  */

int
gpr_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
	return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) != REG)
    return FALSE;

  return GPR_OR_PSEUDO_P (REGNO (op));
}

/* Return true if operand is an accumulator register.  */

int
accum_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
	return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) != REG)
    return FALSE;

  return ACCUM_OR_PSEUDO_P (REGNO (op));
}

/* Return true if operand is a GPR or an accumulator register.  */

int
gpr_or_accum_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
	return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) != REG)
    return FALSE;

  if (ACCUM_P (REGNO (op)))
    return TRUE;

  return GPR_OR_PSEUDO_P (REGNO (op));
}

/* Return true if operand is a CR register.  */

int
cr_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
	return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) != REG)
    return FALSE;

  return CR_OR_PSEUDO_P (REGNO (op));
}

/* Return true if operand is the repeat count register.  */

int
repeat_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
	return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) != REG)
    return FALSE;

  return (REGNO (op) == CR_RPT_C || REGNO (op) >= FIRST_PSEUDO_REGISTER);
}

/* Return true if operand is a FLAG register.  */

int
flag_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
	return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) != REG)
    return FALSE;

  return FLAG_OR_PSEUDO_P (REGNO (op));
}

/* Return true if operand is either F0 or F1.  */

int
br_flag_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
	return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) != REG)
    return FALSE;

  return BR_FLAG_OR_PSEUDO_P (REGNO (op));
}

/* Return true if operand is either F0/F1 or the constants 0/1.  */

int
br_flag_or_constant_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
	return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) == CONST_INT)
    return (INTVAL (op) == 0 || INTVAL (op) == 1);

  if (GET_CODE (op) != REG)
    return FALSE;

  return BR_FLAG_OR_PSEUDO_P (REGNO (op));
}

/* Return true if operand is either F0 or F1, or a GPR register.  */

int
gpr_or_br_flag_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
	return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) != REG)
    return FALSE;

  return GPR_OR_PSEUDO_P (REGNO (op)) || BR_FLAG_P (REGNO (op));
}

/* Return true if operand is the F0 register.  */

int
f0_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
	return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) != REG)
    return FALSE;

  return (REGNO (op) == FLAG_F0 || REGNO (op) >= FIRST_PSEUDO_REGISTER);
}

/* Return true if operand is the F1 register.  */

int
f1_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
	return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) != REG)
    return FALSE;

  return (REGNO (op) == FLAG_F1 || REGNO (op) >= FIRST_PSEUDO_REGISTER);
}

/* Return true if operand is the F1 register.  */

int
carry_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
	return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) != REG)
    return FALSE;

  return (REGNO (op) == FLAG_CARRY || REGNO (op) >= FIRST_PSEUDO_REGISTER);
}

/* Return true if operand is a register of any flavor or a 0 of the
   appropriate type.  */

int
reg_or_0_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  switch (GET_CODE (op))
    {
    default:
      break;

    case REG:
    case SUBREG:
      if (GET_MODE (op) != mode && mode != VOIDmode)
	return FALSE;

      return register_operand (op, mode);

    case CONST_INT:
      return INTVAL (op) == 0;

    case CONST_DOUBLE:
      return CONST_DOUBLE_HIGH (op) == 0 && CONST_DOUBLE_LOW (op) == 0;
    }

  return FALSE;
}

/* Return true if operand is a GPR register or a signed 6 bit immediate.  */

int
gpr_or_signed6_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
	return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) == CONST_INT)
    return IN_RANGE_P (INTVAL (op), -32, 31);

  if (GET_CODE (op) != REG)
    return FALSE;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  return GPR_OR_PSEUDO_P (REGNO (op));
}

/* Return true if operand is a GPR register or an unsigned 5 bit immediate.  */

int
gpr_or_unsigned5_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
	return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) == CONST_INT)
    return IN_RANGE_P (INTVAL (op), 0, 31);

  if (GET_CODE (op) != REG)
    return FALSE;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  return GPR_OR_PSEUDO_P (REGNO (op));
}

/* Return true if operand is a GPR register or an unsigned 6 bit immediate.  */

int
gpr_or_unsigned6_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
	return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) == CONST_INT)
    return IN_RANGE_P (INTVAL (op), 0, 63);

  if (GET_CODE (op) != REG)
    return FALSE;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  return GPR_OR_PSEUDO_P (REGNO (op));
}

/* Return true if operand is a GPR register or a constant of some form.  */

int
gpr_or_constant_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  switch (GET_CODE (op))
    {
    default:
      break;

    case CONST_INT:
    case SYMBOL_REF:
    case LABEL_REF:
    case CONST:
      return TRUE;

    case SUBREG:
      if (GET_CODE (SUBREG_REG (op)) != REG)
	return register_operand (op, mode);

      op = SUBREG_REG (op);
      /* fall through */

    case REG:
      if (GET_MODE (op) != mode && mode != VOIDmode)
	return FALSE;

      return GPR_OR_PSEUDO_P (REGNO (op));
    }

  return FALSE;
}

/* Return true if operand is a GPR register or a constant of some form,
   including a CONST_DOUBLE, which gpr_or_constant_operand doesn't recognize.  */

int
gpr_or_dbl_const_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  switch (GET_CODE (op))
    {
    default:
      break;

    case CONST_INT:
    case CONST_DOUBLE:
    case SYMBOL_REF:
    case LABEL_REF:
    case CONST:
      return TRUE;

    case SUBREG:
      if (GET_CODE (SUBREG_REG (op)) != REG)
	return register_operand (op, mode);

      op = SUBREG_REG (op);
      /* fall through */

    case REG:
      if (GET_MODE (op) != mode && mode != VOIDmode)
	return FALSE;

      return GPR_OR_PSEUDO_P (REGNO (op));
    }

  return FALSE;
}

/* Return true if operand is a gpr register or a valid memory operation.  */

int
gpr_or_memory_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  switch (GET_CODE (op))
    {
    default:
      break;

    case SUBREG:
      if (GET_CODE (SUBREG_REG (op)) != REG)
	return register_operand (op, mode);

      op = SUBREG_REG (op);
      /* fall through */

    case REG:
      if (GET_MODE (op) != mode && mode != VOIDmode)
	return FALSE;

      return GPR_OR_PSEUDO_P (REGNO (op));

    case MEM:
      return d30v_legitimate_address_p (mode, XEXP (op, 0), reload_completed);
    }

  return FALSE;
}

/* Return true if operand is something that can be an input for a move
   operation.  */

int
move_input_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  rtx subreg;
  enum rtx_code code;

  switch (GET_CODE (op))
    {
    default:
      break;

    case CONST_INT:
    case CONST_DOUBLE:
    case SYMBOL_REF:
    case LABEL_REF:
    case CONST:
      return TRUE;

    case SUBREG:
      if (GET_MODE (op) != mode && mode != VOIDmode)
        return FALSE;

      subreg = SUBREG_REG (op);
      code = GET_CODE (subreg);
      if (code == MEM)
	return d30v_legitimate_address_p ((int)mode, XEXP (subreg, 0),
					  reload_completed);

      return (code == REG);

    case REG:
      if (GET_MODE (op) != mode && mode != VOIDmode)
	return FALSE;

      return TRUE;

    case MEM:
      if (GET_CODE (XEXP (op, 0)) == ADDRESSOF)
	return TRUE;
      return d30v_legitimate_address_p (mode, XEXP (op, 0),
					reload_completed);
    }

  return FALSE;
}

/* Return true if operand is something that can be an output for a move
   operation.  */

int
move_output_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  rtx subreg;
  enum rtx_code code;

  switch (GET_CODE (op))
    {
    default:
      break;

    case SUBREG:
      if (GET_MODE (op) != mode && mode != VOIDmode)
        return FALSE;

      subreg = SUBREG_REG (op);
      code = GET_CODE (subreg);
      if (code == MEM)
	return d30v_legitimate_address_p ((int)mode, XEXP (subreg, 0),
					  reload_completed);

      return (code == REG);

    case REG:
      if (GET_MODE (op) != mode && mode != VOIDmode)
	return FALSE;

      return TRUE;

    case MEM:
      if (GET_CODE (XEXP (op, 0)) == ADDRESSOF)
	return TRUE;
      return d30v_legitimate_address_p (mode, XEXP (op, 0),
					reload_completed);
    }

  return FALSE;
}

/* Return true if operand is a signed 6 bit immediate.  */

int
signed6_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) == CONST_INT)
    return IN_RANGE_P (INTVAL (op), -32, 31);

  return FALSE;
}

/* Return true if operand is an unsigned 5 bit immediate.  */

int
unsigned5_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) == CONST_INT)
    return IN_RANGE_P (INTVAL (op), 0, 31);

  return FALSE;
}

/* Return true if operand is an unsigned 6 bit immediate.  */

int
unsigned6_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) == CONST_INT)
    return IN_RANGE_P (INTVAL (op), 0, 63);

  return FALSE;
}

/* Return true if operand is a constant with a single bit set.  */

int
bitset_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) == CONST_INT)
    return IN_RANGE_P (exact_log2 (INTVAL (op)), 0, 31);

  return FALSE;
}

/* Return true if the operator is a ==/!= test against f0 or f1 that can be
   used in conditional execution.  */

int
condexec_test_operator (op, mode)
     rtx op;
     enum machine_mode mode;
{
  rtx x0, x1;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) != EQ && GET_CODE (op) != NE)
    return FALSE;

  x0 = XEXP (op, 0);
  if (GET_CODE (x0) != REG || !BR_FLAG_OR_PSEUDO_P (REGNO (x0)))
    return FALSE;

  x1 = XEXP (op, 1);
  if (GET_CODE (x1) != CONST_INT || INTVAL (x1) != 0)
    return FALSE;

  return TRUE;
}

/* Return true if the operator is a ==/!= test against f0, f1, or a general
   register that can be used in a branch instruction.  */

int
condexec_branch_operator (op, mode)
     rtx op;
     enum machine_mode mode;
{
  rtx x0, x1;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) != EQ && GET_CODE (op) != NE)
    return FALSE;

  x0 = XEXP (op, 0);
  if (GET_CODE (x0) == REG)
    {
      int regno = REGNO (x0);
      if (!GPR_OR_PSEUDO_P (regno) && !BR_FLAG_P (regno))
	return FALSE;
    }
  /* Allow the optimizer to generate things like:
     (if_then_else (ne (const_int 1) (const_int 0))) */
  else if (GET_CODE (x0) != CONST_INT)
    return FALSE;

  x1 = XEXP (op, 1);
  if (GET_CODE (x1) != CONST_INT || INTVAL (x1) != 0)
    return FALSE;

  return TRUE;
}

/* Return true if the unary operator can be executed with conditional
   execution.  */

int
condexec_unary_operator (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  rtx op0;

  /* Only do this after register allocation, so that we can look at the register # */
  if (!reload_completed)
    return FALSE;

  if (GET_RTX_CLASS (GET_CODE (op)) != '1')
    return FALSE;

  op0 = XEXP (op, 0);
  if (GET_CODE (op0) == SUBREG)
    op0 = SUBREG_REG (op0);

  switch (GET_CODE (op))
    {
    default:
      break;

    case ABS:
    case NOT:
      if (GET_MODE (op) == SImode && GET_CODE (op0) == REG && GPR_P (REGNO (op0)))
	return TRUE;

      break;
    }

  return FALSE;
}

/* Return true if the add or subtraction can be executed with conditional
   execution.  */

int
condexec_addsub_operator (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  rtx op0, op1;

  /* Only do this after register allocation, so that we can look at the register # */
  if (!reload_completed)
    return FALSE;

  if (GET_RTX_CLASS (GET_CODE (op)) != '2' && GET_RTX_CLASS (GET_CODE (op)) != 'c')
    return FALSE;

  op0 = XEXP (op, 0);
  op1 = XEXP (op, 1);

  if (GET_CODE (op0) == SUBREG)
    op0 = SUBREG_REG (op0);

  if (GET_CODE (op1) == SUBREG)
    op1 = SUBREG_REG (op1);

  if (GET_CODE (op0) != REG)
    return FALSE;

  switch (GET_CODE (op))
    {
    default:
      break;

    case PLUS:
    case MINUS:
      return (GET_MODE (op) == SImode && GPR_P (REGNO (op0))
	      && gpr_or_constant_operand (op1, SImode));
    }

  return FALSE;
}

/* Return true if the binary operator can be executed with conditional
   execution.  We don't include add/sub here, since they have extra
   clobbers for the flags registers.  */

int
condexec_binary_operator (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  rtx op0, op1;

  /* Only do this after register allocation, so that we can look at the register # */
  if (!reload_completed)
    return FALSE;

  if (GET_RTX_CLASS (GET_CODE (op)) != '2' && GET_RTX_CLASS (GET_CODE (op)) != 'c')
    return FALSE;

  op0 = XEXP (op, 0);
  op1 = XEXP (op, 1);

  if (GET_CODE (op0) == SUBREG)
    op0 = SUBREG_REG (op0);

  if (GET_CODE (op1) == SUBREG)
    op1 = SUBREG_REG (op1);

  if (GET_CODE (op0) != REG)
    return FALSE;

  /* MULT is not included here, because it is an IU only instruction.  */
  switch (GET_CODE (op))
    {
    default:
      break;

    case AND:
    case IOR:
    case XOR:
    case ASHIFTRT:
    case LSHIFTRT:
    case ROTATERT:
      return (GET_MODE (op) == SImode && GPR_P (REGNO (op0))
	      && gpr_or_constant_operand (op1, SImode));

    case ASHIFT:
    case ROTATE:
      return (GET_MODE (op) == SImode && GPR_P (REGNO (op0))
	      && GET_CODE (op1) == CONST_INT);
    }

  return FALSE;
}

/* Return true if the shift/rotate left operator can be executed with
   conditional execution.  */

int
condexec_shiftl_operator (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  rtx op0, op1;

  /* Only do this after register allocation, so that we can look at the register # */
  if (!reload_completed)
    return FALSE;

  if (GET_RTX_CLASS (GET_CODE (op)) != '2' && GET_RTX_CLASS (GET_CODE (op)) != 'c')
    return FALSE;

  op0 = XEXP (op, 0);
  op1 = XEXP (op, 1);

  if (GET_CODE (op0) == SUBREG)
    op0 = SUBREG_REG (op0);

  if (GET_CODE (op1) == SUBREG)
    op1 = SUBREG_REG (op1);

  if (GET_CODE (op0) != REG)
    return FALSE;

  switch (GET_CODE (op))
    {
    default:
      break;

    case ASHIFT:
    case ROTATE:
      return (GET_MODE (op) == SImode && GPR_P (REGNO (op0))
	      && GET_CODE (op1) == NEG
	      && GET_CODE (XEXP (op1, 0)) == REG
	      && GPR_P (REGNO (XEXP (op1, 0))));
    }

  return FALSE;
}

/* Return true if the {sign,zero} extend operator from memory can be
   conditionally executed.  */

int
condexec_extend_operator (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  /* Only do this after register allocation, so that we can look at the register # */
  if (!reload_completed)
    return FALSE;

  if (GET_RTX_CLASS (GET_CODE (op)) != '1')
    return FALSE;

  switch (GET_CODE (op))
    {
    default:
      break;

    case SIGN_EXTEND:
    case ZERO_EXTEND:
      if ((GET_MODE (op) == SImode && GET_MODE (XEXP (op, 0)) == QImode)
	  || (GET_MODE (op) == SImode && GET_MODE (XEXP (op, 0)) == HImode)
	  || (GET_MODE (op) == HImode && GET_MODE (XEXP (op, 0)) == QImode))
	return TRUE;

      break;
    }

  return FALSE;
}

/* Return true for comparisons against 0 that can be turned into a
   bratnz/bratzr instruction.  */

int
branch_zero_operator (op, mode)
     rtx op;
     enum machine_mode mode;
{
  rtx x0, x1;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) != EQ && GET_CODE (op) != NE)
    return FALSE;

  x0 = XEXP (op, 0);
  if (GET_CODE (x0) != REG || !GPR_OR_PSEUDO_P (REGNO (x0)))
    return FALSE;

  x1 = XEXP (op, 1);
  if (GET_CODE (x1) != CONST_INT || INTVAL (x1) != 0)
    return FALSE;

  return TRUE;
}

/* Return true if an operand is simple, suitable for use as the destination of
   a conditional move */

int
cond_move_dest_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  rtx addr;

  if (mode != QImode && mode != HImode && mode != SImode && mode != SFmode)
    return FALSE;

  switch (GET_CODE (op))
    {
    default:
      break;

    case REG:
    case SUBREG:
      return gpr_operand (op, mode);

    /* Don't allow post dec/inc, since we might not get the side effects correct. */
    case MEM:
      addr = XEXP (op, 0);
      return (GET_CODE (addr) != POST_DEC
	      && GET_CODE (addr) != POST_INC
	      && d30v_legitimate_address_p (mode, addr, reload_completed));
    }

  return FALSE;
}

/* Return true if an operand is simple, suitable for use in a conditional move */

int
cond_move_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  rtx addr;

  if (mode != QImode && mode != HImode && mode != SImode && mode != SFmode)
    return FALSE;

  switch (GET_CODE (op))
    {
    default:
      break;

    case REG:
    case SUBREG:
      return gpr_operand (op, mode);

    case CONST_DOUBLE:
      return GET_MODE (op) == SFmode;

    case CONST_INT:
    case SYMBOL_REF:
    case LABEL_REF:
    case CONST:
      return TRUE;

    /* Don't allow post dec/inc, since we might not get the side effects correct. */
    case MEM:
      addr = XEXP (op, 0);
      return (GET_CODE (addr) != POST_DEC
	      && GET_CODE (addr) != POST_INC
	      && d30v_legitimate_address_p (mode, addr, reload_completed));
    }

  return FALSE;
}

/* Return true if an operand is simple, suitable for use in conditional execution.
   Unlike cond_move, we can allow auto inc/dec.  */

int
cond_exec_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (mode != QImode && mode != HImode && mode != SImode && mode != SFmode)
    return FALSE;

  switch (GET_CODE (op))
    {
    default:
      break;

    case REG:
    case SUBREG:
      return gpr_operand (op, mode);

    case CONST_DOUBLE:
      return GET_MODE (op) == SFmode;

    case CONST_INT:
    case SYMBOL_REF:
    case LABEL_REF:
    case CONST:
      return TRUE;

    case MEM:
      return memory_operand (op, mode);
    }

  return FALSE;
}

/* Return true if operand is a SI mode signed relational test.  */

int
srelational_si_operator (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  rtx x0, x1;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  switch (GET_CODE (op))
    {
    default:
      return FALSE;

    case EQ:
    case NE:
    case LT:
    case LE:
    case GT:
    case GE:
      break;
    }

  x0 = XEXP (op, 0);
  if (GET_CODE (x0) != REG && GET_CODE (x0) != SUBREG)
    return FALSE;

  if (GET_MODE (x0) != SImode)
    return FALSE;

  x1 = XEXP (op, 1);
  switch (GET_CODE (x1))
    {
    default:
      return FALSE;

    case REG:
    case SUBREG:
    case CONST_INT:
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST:
      break;
    }

  return TRUE;
}

/* Return true if operand is a SI mode unsigned relational test.  */

int
urelational_si_operator (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  rtx x0, x1;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  switch (GET_CODE (op))
    {
    default:
      return FALSE;

    case LTU:
    case LEU:
    case GTU:
    case GEU:
      break;
    }

  x0 = XEXP (op, 0);
  if (GET_CODE (x0) != REG && GET_CODE (x0) != SUBREG)
    return FALSE;

  if (GET_MODE (x0) != SImode)
    return FALSE;

  x1 = XEXP (op, 1);
  switch (GET_CODE (x1))
    {
    default:
      return FALSE;

    case REG:
    case SUBREG:
    case CONST_INT:
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST:
      break;
    }

  return TRUE;
}

/* Return true if operand is a DI mode relational test.  */

int
relational_di_operator (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  rtx x0, x1;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_RTX_CLASS (GET_CODE (op)) != '<')
    return FALSE;

  x0 = XEXP (op, 0);
  if (GET_CODE (x0) != REG && GET_CODE (x0) != SUBREG)
    return FALSE;

  if (GET_MODE (x0) != DImode)
    return FALSE;

  x1 = XEXP (op, 1);
  if (GET_CODE (x1) != REG && GET_CODE (x1) != SUBREG
      && GET_CODE (x1) != CONST_INT && GET_CODE (x1) != CONST_DOUBLE)
    return FALSE;

  return TRUE;
}


/* Calculate the stack information for the current function.

   D30V stack frames look like:

	high		|  ....				|
			+-------------------------------+
			| Argument word #19		|
			+-------------------------------+
			| Argument word #18		|
			+-------------------------------+
			| Argument word #17		|
			+-------------------------------+
			| Argument word #16		|
		Prev sp	+-------------------------------+
			|				|
			| Save for arguments 1..16 if	|
			| the func. uses stdarg/varargs	|
			|				|
			+-------------------------------+
			|				|
			| Save area for GPR registers	|
			|				|
			+-------------------------------+
			|				|
			| Save area for accumulators	|
			|				|
			+-------------------------------+
			|				|
			| Local variables		|
			|				|
			+-------------------------------+
			|				|
			| alloca space if used		|
			|				|
			+-------------------------------+
			|				|
			| Space for outgoing arguments	|
			|				|
	low	SP---->	+-------------------------------+
*/

d30v_stack_t *
d30v_stack_info ()
{
  static d30v_stack_t info, zero_info;
  d30v_stack_t *info_ptr = &info;
  tree fndecl		 = current_function_decl;
  tree fntype		 = TREE_TYPE (fndecl);
  int varargs_p		 = 0;
  tree cur_arg;
  tree next_arg;
  int saved_gprs;
  int saved_accs;
  int memrefs_2words;
  int memrefs_1word;
  unsigned char save_gpr_p[GPR_LAST];
  int i;

  /* If we've already calculated the values and reload is complete, just return now */
  if (d30v_stack_cache)
    return d30v_stack_cache;

  /* Zero all fields */
  info = zero_info;

  if (current_function_profile)
    regs_ever_live[GPR_LINK] = 1;

  /* Determine if this is a stdarg function */
  if (TYPE_ARG_TYPES (fntype) != 0
      && (TREE_VALUE (tree_last (TYPE_ARG_TYPES (fntype))) != void_type_node))
    varargs_p = 1;
  else
    {
      /* Find the last argument, and see if it is __builtin_va_alist.  */
      for (cur_arg = DECL_ARGUMENTS (fndecl); cur_arg != (tree)0; cur_arg = next_arg)
	{
	  next_arg = TREE_CHAIN (cur_arg);
	  if (next_arg == (tree)0)
	    {
	      if (DECL_NAME (cur_arg)
		  && !strcmp (IDENTIFIER_POINTER (DECL_NAME (cur_arg)), "__builtin_va_alist"))
		varargs_p = 1;

	      break;
	    }
	}
    }

  /* Calculate which registers need to be saved & save area size */
  saved_accs = 0;
  memrefs_2words = 0;
  memrefs_1word = 0;
  for (i = ACCUM_FIRST; i <= ACCUM_LAST; i++)
    {
      if (regs_ever_live[i] && !call_used_regs[i])
	{
	  info_ptr->save_p[i] = 2;
	  saved_accs++;
	  memrefs_2words++;
	}
    }

  saved_gprs = 0;
  for (i = GPR_FIRST; i <= GPR_LAST; i++)
    {
      if (regs_ever_live[i] && (!call_used_regs[i] || i == GPR_LINK))
	{
	  save_gpr_p[i] = 1;
	  saved_gprs++;
	}
      else
	save_gpr_p[i] = 0;
    }

  /* Determine which register pairs can be saved together with ld2w/st2w  */
  for (i = GPR_FIRST; i <= GPR_LAST; i++)
    {
      if (((i - GPR_FIRST) & 1) == 0 && save_gpr_p[i] && save_gpr_p[i+1])
	{
	  memrefs_2words++;
	  info_ptr->save_p[i++] = 2;
	}
      else if (save_gpr_p[i])
	{
	  memrefs_1word++;
	  info_ptr->save_p[i] = 1;
	}
    }

  /* Determine various sizes */
  info_ptr->varargs_p	 = varargs_p;
  info_ptr->varargs_size = ((varargs_p)
			    ? (GPR_ARG_LAST + 1 - GPR_ARG_FIRST) * UNITS_PER_WORD
			    : 0);

  info_ptr->accum_size	 = 2 * UNITS_PER_WORD * saved_accs;
  info_ptr->gpr_size	 = D30V_ALIGN (UNITS_PER_WORD * saved_gprs,
				       2 * UNITS_PER_WORD);
  info_ptr->vars_size    = D30V_ALIGN (get_frame_size (), 2 * UNITS_PER_WORD);
  info_ptr->parm_size    = D30V_ALIGN (current_function_outgoing_args_size,
				       2 * UNITS_PER_WORD);

  info_ptr->total_size	 = D30V_ALIGN ((info_ptr->gpr_size
					+ info_ptr->accum_size
					+ info_ptr->vars_size
					+ info_ptr->parm_size
					+ info_ptr->varargs_size
					+ current_function_pretend_args_size),
				       (STACK_BOUNDARY / BITS_PER_UNIT));

  info_ptr->save_offset  = (info_ptr->total_size
			    - (current_function_pretend_args_size
			       + info_ptr->varargs_size
			       + info_ptr->gpr_size
			       + info_ptr->accum_size));

  /* The link register is the last GPR saved, but there might be some padding
     bytes after it, so account for that.  */
  info_ptr->link_offset  = (info_ptr->total_size
			    - (current_function_pretend_args_size
			       + info_ptr->varargs_size
			       + (info_ptr->gpr_size
				  - UNITS_PER_WORD * saved_gprs)
			       + UNITS_PER_WORD));

  info_ptr->memrefs_varargs = info_ptr->varargs_size / (2 * UNITS_PER_WORD);
  info_ptr->memrefs_2words  = memrefs_2words;
  info_ptr->memrefs_1word   = memrefs_1word;

  if (reload_completed)
    d30v_stack_cache = info_ptr;

  return info_ptr;
}


/* Internal function to print all of the information about the stack */

void
debug_stack_info (info)
     d30v_stack_t *info;
{
  int i;

  if (!info)
    info = d30v_stack_info ();

  fprintf (stderr, "\nStack information for function %s:\n",
	   ((current_function_decl && DECL_NAME (current_function_decl))
	    ? IDENTIFIER_POINTER (DECL_NAME (current_function_decl))
	    : "<unknown>"));

  fprintf (stderr, "\tsave_offset     = %d\n", info->save_offset);
  fprintf (stderr, "\tmemrefs_varargs = %d\n", info->memrefs_varargs);
  fprintf (stderr, "\tmemrefs_2words  = %d\n", info->memrefs_2words);
  fprintf (stderr, "\tmemrefs_1word   = %d\n", info->memrefs_1word);
  fprintf (stderr, "\tvarargs_p       = %d\n", info->varargs_p);
  fprintf (stderr, "\tvarargs_size    = %d\n", info->varargs_size);
  fprintf (stderr, "\tvars_size       = %d\n", info->vars_size);
  fprintf (stderr, "\tparm_size       = %d\n", info->parm_size);
  fprintf (stderr, "\tgpr_size        = %d\n", info->gpr_size);
  fprintf (stderr, "\taccum_size      = %d\n", info->accum_size);
  fprintf (stderr, "\ttotal_size      = %d\n", info->total_size);
  fprintf (stderr, "\tsaved registers =");

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (info->save_p[i] == 2)
	{
	  fprintf (stderr, " %s-%s", reg_names[i], reg_names[i+1]);
	  i++;
	}
      else if (info->save_p[i])
	fprintf (stderr, " %s", reg_names[i]);
    }

  putc ('\n', stderr);
  fflush (stderr);
}


/* Return nonzero if this function is known to have a null or 1 instruction epilogue.  */

int
direct_return ()
{
  if (reload_completed)
    {
      d30v_stack_t *info = d30v_stack_info ();

      /* If no epilogue code is needed, can use just a simple jump */
      if (info->total_size == 0)
	return 1;

#if 0
      /* If just a small amount of local stack was allocated and no registers
         saved, skip forward branch */
      if (info->total_size == info->vars_size
	  && IN_RANGE_P (info->total_size, 1, 31))
	return 1;
#endif
    }

  return 0;
}


/* A C statement (sans semicolon) for initializing the variable CUM for the
   state at the beginning of the argument list.  The variable has type
   `CUMULATIVE_ARGS'.  The value of FNTYPE is the tree node for the data type
   of the function which will receive the args, or 0 if the args are to a
   compiler support library function.  The value of INDIRECT is nonzero when
   processing an indirect call, for example a call through a function pointer.
   The value of INDIRECT is zero for a call to an explicitly named function, a
   library function call, or when `INIT_CUMULATIVE_ARGS' is used to find
   arguments for the function being compiled.

   When processing a call to a compiler support library function, LIBNAME
   identifies which one.  It is a `symbol_ref' rtx which contains the name of
   the function, as a string.  LIBNAME is 0 when an ordinary C function call is
   being processed.  Thus, each time this macro is called, either LIBNAME or
   FNTYPE is nonzero, but never both of them at once.  */

void
d30v_init_cumulative_args (cum, fntype, libname, indirect, incoming)
     CUMULATIVE_ARGS *cum;
     tree fntype;
     rtx libname;
     int indirect;
     int incoming;
{
  *cum = GPR_ARG_FIRST;

  if (TARGET_DEBUG_ARG)
    {
      fprintf (stderr, "\ninit_cumulative_args:");
      if (indirect)
	fputs (" indirect", stderr);

      if (incoming)
	fputs (" incoming", stderr);

      if (fntype)
	{
	  tree ret_type = TREE_TYPE (fntype);
	  fprintf (stderr, " return=%s,",
		   tree_code_name[ (int)TREE_CODE (ret_type) ]);
	}

      if (libname && GET_CODE (libname) == SYMBOL_REF)
	fprintf (stderr, " libname=%s", XSTR (libname, 0));

      putc ('\n', stderr);
    }
}


/* If defined, a C expression that gives the alignment boundary, in bits, of an
   argument with the specified mode and type.  If it is not defined,
   `PARM_BOUNDARY' is used for all arguments.  */

int
d30v_function_arg_boundary (mode, type)
     enum machine_mode mode;
     tree type;
{
  int size = ((mode == BLKmode && type)
	      ? int_size_in_bytes (type)
	      : (int) GET_MODE_SIZE (mode));

  return (size > UNITS_PER_WORD) ? 2*UNITS_PER_WORD : UNITS_PER_WORD;
}


/* A C expression that controls whether a function argument is passed in a
   register, and which register.

   The arguments are CUM, which summarizes all the previous arguments; MODE,
   the machine mode of the argument; TYPE, the data type of the argument as a
   tree node or 0 if that is not known (which happens for C support library
   functions); and NAMED, which is 1 for an ordinary argument and 0 for
   nameless arguments that correspond to `...' in the called function's
   prototype.

   The value of the expression should either be a `reg' RTX for the hard
   register in which to pass the argument, or zero to pass the argument on the
   stack.

   For machines like the VAX and 68000, where normally all arguments are
   pushed, zero suffices as a definition.

   The usual way to make the ANSI library `stdarg.h' work on a machine where
   some arguments are usually passed in registers, is to cause nameless
   arguments to be passed on the stack instead.  This is done by making
   `FUNCTION_ARG' return 0 whenever NAMED is 0.

   You may use the macro `MUST_PASS_IN_STACK (MODE, TYPE)' in the definition of
   this macro to determine if this argument is of a type that must be passed in
   the stack.  If `REG_PARM_STACK_SPACE' is not defined and `FUNCTION_ARG'
   returns nonzero for such an argument, the compiler will abort.  If
   `REG_PARM_STACK_SPACE' is defined, the argument will be computed in the
   stack and then loaded into a register.  */

rtx
d30v_function_arg (cum, mode, type, named, incoming)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named;
     int incoming ATTRIBUTE_UNUSED;
{
  int size = ((mode == BLKmode && type)
	      ? int_size_in_bytes (type)
	      : (int) GET_MODE_SIZE (mode));
  int adjust = (size > UNITS_PER_WORD && (*cum & 1) != 0);
  rtx ret;

  /* Return a marker for use in the call instruction.  */
  if (mode == VOIDmode)
    ret = const0_rtx;

  else if (*cum + adjust <= GPR_ARG_LAST)
    ret = gen_rtx (REG, mode, *cum + adjust);

  else
    ret = NULL_RTX;

  if (TARGET_DEBUG_ARG)
    fprintf (stderr,
	     "function_arg: words = %2d, mode = %4s, named = %d, size = %3d, adjust = %1d, arg = %s\n",
	     *cum, GET_MODE_NAME (mode), named, size, adjust,
	     (ret) ? ((ret == const0_rtx) ? "<0>" : reg_names[ REGNO (ret) ]) : "memory");

  return ret;
}


/* A C expression for the number of words, at the beginning of an argument,
   must be put in registers.  The value must be zero for arguments that are
   passed entirely in registers or that are entirely pushed on the stack.

   On some machines, certain arguments must be passed partially in registers
   and partially in memory.  On these machines, typically the first N words of
   arguments are passed in registers, and the rest on the stack.  If a
   multi-word argument (a `double' or a structure) crosses that boundary, its
   first few words must be passed in registers and the rest must be pushed.
   This macro tells the compiler when this occurs, and how many of the words
   should go in registers.

   `FUNCTION_ARG' for these arguments should return the first register to be
   used by the caller for this argument; likewise `FUNCTION_INCOMING_ARG', for
   the called function.  */

int
d30v_function_arg_partial_nregs (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named ATTRIBUTE_UNUSED;
{
  int bytes = ((mode == BLKmode)
	       ? int_size_in_bytes (type)
	       : (int) GET_MODE_SIZE (mode));
  int words = (bytes + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
  int adjust = (bytes > UNITS_PER_WORD && (*cum & 1) != 0);
  int arg_num = *cum + adjust;
  int ret;

  ret = ((arg_num <= GPR_ARG_LAST && arg_num + words > GPR_ARG_LAST+1)
	 ? GPR_ARG_LAST - arg_num + 1
	 : 0);

  if (TARGET_DEBUG_ARG && ret)
    fprintf (stderr, "function_arg_partial_nregs: %d\n", ret);

  return ret;
}


/* A C expression that indicates when an argument must be passed by reference.
   If nonzero for an argument, a copy of that argument is made in memory and a
   pointer to the argument is passed instead of the argument itself.  The
   pointer is passed in whatever way is appropriate for passing a pointer to
   that type.

   On machines where `REG_PARM_STACK_SPACE' is not defined, a suitable
   definition of this macro might be
        #define FUNCTION_ARG_PASS_BY_REFERENCE\
        (CUM, MODE, TYPE, NAMED)  \
          MUST_PASS_IN_STACK (MODE, TYPE)  */

int
d30v_function_arg_pass_by_reference (cum, mode, type, named)
     CUMULATIVE_ARGS *cum ATTRIBUTE_UNUSED;
     enum machine_mode mode;
     tree type;
     int named ATTRIBUTE_UNUSED;
{
  int ret = MUST_PASS_IN_STACK (mode, type);

  if (TARGET_DEBUG_ARG && ret)
    fprintf (stderr, "function_arg_pass_by_reference: %d\n", ret);

  return ret;
}


/* A C statement (sans semicolon) to update the summarizer variable CUM to
   advance past an argument in the argument list.  The values MODE, TYPE and
   NAMED describe that argument.  Once this is done, the variable CUM is
   suitable for analyzing the *following* argument with `FUNCTION_ARG', etc.

   This macro need not do anything if the argument in question was passed on
   the stack.  The compiler knows how to track the amount of stack space used
   for arguments without any special help.  */

void
d30v_function_arg_advance (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named;
{
  int bytes = ((mode == BLKmode)
	       ? int_size_in_bytes (type)
	       : (int) GET_MODE_SIZE (mode));
  int words = D30V_ALIGN (bytes, UNITS_PER_WORD) / UNITS_PER_WORD;
  int adjust = (bytes > UNITS_PER_WORD && (*cum & 1) != 0);

  *cum += words + adjust;

  if (TARGET_DEBUG_ARG)
    fprintf (stderr,
	     "function_adv: words = %2d, mode = %4s, named = %d, size = %3d, adjust = %1d\n",
	     *cum, GET_MODE_NAME (mode), named, words * UNITS_PER_WORD, adjust);
}


/* If defined, is a C expression that produces the machine-specific code for a
   call to `__builtin_saveregs'.  This code will be moved to the very beginning
   of the function, before any parameter access are made.  The return value of
   this function should be an RTX that contains the value to use as the return
   of `__builtin_saveregs'.

   If this macro is not defined, the compiler will output an ordinary call to
   the library function `__builtin_saveregs'.  */

rtx
d30v_expand_builtin_saveregs ()
{
  int offset = UNITS_PER_WORD * (GPR_ARG_LAST + 1 - GPR_ARG_FIRST);

  if (TARGET_DEBUG_ARG)
    fprintf (stderr, "expand_builtin_saveregs: offset from ap = %d\n",
	     offset);

  return gen_rtx (PLUS, Pmode, virtual_incoming_args_rtx, GEN_INT (- offset));
}


/* This macro offers an alternative to using `__builtin_saveregs' and defining
   the macro `EXPAND_BUILTIN_SAVEREGS'.  Use it to store the anonymous register
   arguments into the stack so that all the arguments appear to have been
   passed consecutively on the stack.  Once this is done, you can use the
   standard implementation of varargs that works for machines that pass all
   their arguments on the stack.

   The argument ARGS_SO_FAR is the `CUMULATIVE_ARGS' data structure, containing
   the values that obtain after processing of the named arguments.  The
   arguments MODE and TYPE describe the last named argument--its machine mode
   and its data type as a tree node.

   The macro implementation should do two things: first, push onto the stack
   all the argument registers *not* used for the named arguments, and second,
   store the size of the data thus pushed into the `int'-valued variable whose
   name is supplied as the argument PRETEND_ARGS_SIZE.  The value that you
   store here will serve as additional offset for setting up the stack frame.

   Because you must generate code to push the anonymous arguments at compile
   time without knowing their data types, `SETUP_INCOMING_VARARGS' is only
   useful on machines that have just a single category of argument register and
   use it uniformly for all data types.

   If the argument SECOND_TIME is nonzero, it means that the arguments of the
   function are being analyzed for the second time.  This happens for an inline
   function, which is not actually compiled until the end of the source file.
   The macro `SETUP_INCOMING_VARARGS' should not generate any instructions in
   this case.  */

void
d30v_setup_incoming_varargs (cum, mode, type, pretend_size, second_time)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type ATTRIBUTE_UNUSED;
     int *pretend_size ATTRIBUTE_UNUSED;
     int second_time;
{
  if (TARGET_DEBUG_ARG)
    fprintf (stderr,
	     "setup_vararg: words = %2d, mode = %4s, second_time = %d\n",
	     *cum, GET_MODE_NAME (mode), second_time);
}


/* Create the va_list data type.  */

tree
d30v_build_va_list ()
{
  tree f_arg_ptr, f_arg_num, record, type_decl;
  tree int_type_node;

  record = (*lang_hooks.types.make_type) (RECORD_TYPE);
  type_decl = build_decl (TYPE_DECL, get_identifier ("__va_list_tag"), record);
  int_type_node = make_signed_type (INT_TYPE_SIZE);

  f_arg_ptr = build_decl (FIELD_DECL, get_identifier ("__va_arg_ptr"), 
			  ptr_type_node);
  f_arg_num = build_decl (FIELD_DECL, get_identifier ("__va_arg_num"),
			  int_type_node);

  DECL_FIELD_CONTEXT (f_arg_ptr) = record;
  DECL_FIELD_CONTEXT (f_arg_num) = record;

  TREE_CHAIN (record) = type_decl;
  TYPE_NAME (record) = type_decl;
  TYPE_FIELDS (record) = f_arg_ptr;
  TREE_CHAIN (f_arg_ptr) = f_arg_num;

  layout_type (record);

  /* The correct type is an array type of one element.  */
  return build_array_type (record, build_index_type (size_zero_node));
}


/* Expand __builtin_va_start to do the va_start macro.  */

void 
d30v_expand_builtin_va_start (valist, nextarg)
     tree valist;
     rtx nextarg ATTRIBUTE_UNUSED;
{
  HOST_WIDE_INT words;
  tree f_arg_ptr, f_arg_num;
  tree arg_ptr, arg_num, saveregs, t;

  f_arg_ptr = TYPE_FIELDS (TREE_TYPE (va_list_type_node));
  f_arg_num = TREE_CHAIN (f_arg_ptr);

  valist = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (valist)), valist);
  arg_ptr = build (COMPONENT_REF, TREE_TYPE (f_arg_ptr), valist, f_arg_ptr);
  arg_num = build (COMPONENT_REF, TREE_TYPE (f_arg_num), valist, f_arg_num);

  words = current_function_args_info;	/* __builtin_args_info (0) */

  /* (AP)->__va_arg_ptr = (int *) __builtin_saveregs (); */
  saveregs = make_tree (TREE_TYPE (arg_ptr), d30v_expand_builtin_saveregs ());
  t = build (MODIFY_EXPR, TREE_TYPE (arg_ptr), arg_ptr, saveregs);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* (AP)->__va_arg_num = __builtin_args_info (0) - 2; */
  t = build (PLUS_EXPR, TREE_TYPE (arg_num), build_int_2 (words, 0),
	     build_int_2 (-GPR_ARG_FIRST, 0));
  t = build (MODIFY_EXPR, TREE_TYPE (arg_num), arg_num, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
}


/* Expand __builtin_va_arg to do the va_arg macro.  */

rtx
d30v_expand_builtin_va_arg(valist, type)
     tree valist;
     tree type;
{
  tree f_arg_ptr, f_arg_num;
  tree arg_ptr, arg_num, t, ptr;
  int num, size;
  rtx lab_false, ptr_rtx, r;

  f_arg_ptr = TYPE_FIELDS (TREE_TYPE (va_list_type_node));
  f_arg_num = TREE_CHAIN (f_arg_ptr);

  valist = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (valist)), valist);
  arg_ptr = build (COMPONENT_REF, TREE_TYPE (f_arg_ptr), valist, f_arg_ptr);
  arg_num = build (COMPONENT_REF, TREE_TYPE (f_arg_num), valist, f_arg_num);

  size = int_size_in_bytes (type);

  lab_false = gen_label_rtx ();
  ptr_rtx = gen_reg_rtx (Pmode);

  /* if (sizeof (TYPE) > 4 && ((AP)->__va_arg_num & 1) != 0)
       (AP)->__va_arg_num++; */

  if (size > UNITS_PER_WORD) 
    {
      t = build (BIT_AND_EXPR, TREE_TYPE (arg_num), arg_num, 
		 build_int_2 (1, 0));

      emit_cmp_and_jump_insns (expand_expr (t, NULL_RTX, QImode, EXPAND_NORMAL),
			       GEN_INT (0), EQ, const1_rtx, QImode, 1,
			       lab_false);

      t = build (POSTINCREMENT_EXPR, TREE_TYPE (arg_num), arg_num,
		 build_int_2 (1, 0));
      TREE_SIDE_EFFECTS (t) = 1;
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

      emit_label (lab_false);
    }


  /* __ptr = (TYPE *)(((char *)(void *)((AP)->__va_arg_ptr 
	     + (AP)->__va_arg_num))); */

  t = build (MULT_EXPR, TREE_TYPE (arg_num), arg_num, build_int_2 (4, 0));
  t = build (PLUS_EXPR, ptr_type_node, arg_ptr, t);

  /* if (sizeof (TYPE) < 4)
       __ptr = (void *)__ptr + 4 - sizeof (TYPE); */

  if (size < UNITS_PER_WORD)
    t = build (PLUS_EXPR, ptr_type_node, t,
	       build_int_2 (UNITS_PER_WORD - size, 0));

  TREE_SIDE_EFFECTS (t) = 1;

  ptr = build1 (NOP_EXPR, build_pointer_type (type), t);
  t = build (MODIFY_EXPR, type, ptr, t);

  r = expand_expr (t, ptr_rtx, Pmode, EXPAND_NORMAL);
  if (r != ptr_rtx)
    emit_move_insn (ptr_rtx, r);


  /* (AP)->__va_arg_num += (sizeof (TYPE) + 3) / 4; */
  num = (size + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD;
  t = build (POSTINCREMENT_EXPR, TREE_TYPE (arg_num), arg_num, 
	     build_int_2 (num, 0));
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  return ptr_rtx;
}

/* Generate the assembly code for function entry.  FILE is a stdio
   stream to output the code to.  SIZE is an int: how many units of
   temporary storage to allocate.

   Refer to the array `regs_ever_live' to determine which registers to
   save; `regs_ever_live[I]' is nonzero if register number I is ever
   used in the function.  This function is responsible for knowing
   which registers should not be saved even if used.  */

static void
d30v_output_function_prologue (stream, size)
     FILE *stream ATTRIBUTE_UNUSED;
     HOST_WIDE_INT size ATTRIBUTE_UNUSED;
{
  /* For the d30v, move all of the prologue processing into separate
     insns.  */
}


/* Called after register allocation to add any instructions needed for
   the prologue.  Using a prologue insn is favored compared to putting
   all of the instructions in output_function_prologue (), since it
   allows the scheduler to intermix instructions with the saves of the
   caller saved registers.  In some cases, it might be necessary to
   emit a barrier instruction as the last insn to prevent such
   scheduling.  */

void
d30v_expand_prologue ()
{
  rtx sp = stack_pointer_rtx;
  d30v_stack_t *info = d30v_stack_info ();
  int i;
  rtx mem_di = NULL_RTX;
  rtx mem_si = NULL_RTX;
  int num_memrefs = (info->memrefs_2words
		     + info->memrefs_1word
		     + info->memrefs_varargs);

  if (TARGET_DEBUG_STACK)
    debug_stack_info (info);

  /* Grow the stack.  */
  if (info->total_size)
    emit_insn (gen_addsi3 (sp, sp, GEN_INT (- info->total_size)));

  /* If there is more than one save, use post-increment addressing which will
     result in smaller code, than would the normal references.  If there is
     only one save, just do the store as normal.  */

  if (num_memrefs > 1)
    {
      rtx save_tmp = gen_rtx (REG, Pmode, GPR_STACK_TMP);
      rtx post_inc = gen_rtx (POST_INC, Pmode, save_tmp);
      mem_di = gen_rtx (MEM, DImode, post_inc);
      mem_si = gen_rtx (MEM, SImode, post_inc);
      emit_insn (gen_addsi3 (save_tmp, sp, GEN_INT (info->save_offset)));
    }
  else if (num_memrefs == 1)
    {
      rtx addr = plus_constant (sp, info->save_offset);
      mem_di = gen_rtx (MEM, DImode, addr);
      mem_si = gen_rtx (MEM, SImode, addr);
    }

  /* Save the accumulators.  */
  for (i = ACCUM_FIRST; i <= ACCUM_LAST; i++)
    if (info->save_p[i])
      {
	rtx acc_tmp = gen_rtx (REG, DImode, GPR_ATMP_FIRST);
	emit_insn (gen_movdi (acc_tmp, gen_rtx (REG, DImode, i)));
	emit_insn (gen_movdi (mem_di, acc_tmp));
      }

  /* Save the GPR registers that are adjacent to each other with st2w.  */
  for (i = GPR_FIRST; i <= GPR_LAST; i += 2)
    if (info->save_p[i] == 2)
      emit_insn (gen_movdi (mem_di, gen_rtx (REG, DImode, i)));

  /* Save the GPR registers that need to be saved with a single word store.  */
  for (i = GPR_FIRST; i <= GPR_LAST; i++)
    if (info->save_p[i] == 1)
      emit_insn (gen_movsi (mem_si, gen_rtx (REG, SImode, i)));

  /* Save the argument registers if this function accepts variable args.  */
  if (info->varargs_p)
    {
      /* Realign r22 if an odd # of GPRs were saved.  */
      if ((info->memrefs_1word & 1) != 0)
	{
	  rtx save_tmp = XEXP (XEXP (mem_si, 0), 0);
	  emit_insn (gen_addsi3 (save_tmp, save_tmp, GEN_INT (UNITS_PER_WORD)));
	}

      for (i = GPR_ARG_FIRST; i <= GPR_ARG_LAST; i += 2)
	emit_insn (gen_movdi (mem_di, gen_rtx (REG, DImode, i)));
    }

  /* Update the frame pointer.  */
  if (frame_pointer_needed)
    emit_move_insn (frame_pointer_rtx, sp);

  /* Hack for now, to prevent scheduler from being too cleaver */
  emit_insn (gen_blockage ());
}


/* This function generates the assembly code for function exit.
   Args are as for output_function_prologue ().

   The function epilogue should not depend on the current stack
   pointer!  It should use the frame pointer only.  This is mandatory
   because of alloca; we also take advantage of it to omit stack
   adjustments before returning.  */

static void
d30v_output_function_epilogue (stream, size)
     FILE *stream ATTRIBUTE_UNUSED;
     HOST_WIDE_INT size ATTRIBUTE_UNUSED;
{
  /* For the d30v, move all processing to be as insns, but do any
     cleanup here, since it is done after handling all of the insns.  */
  d30v_stack_cache = (d30v_stack_t *)0;	/* reset stack cache */
}



/* Called after register allocation to add any instructions needed for
   the epilogue.  Using an epilogue insn is favored compared to putting
   all of the instructions in output_function_prologue(), since it
   allows the scheduler to intermix instructions with the saves of the
   caller saved registers.  In some cases, it might be necessary to
   emit a barrier instruction as the last insn to prevent such
   scheduling.  */

void
d30v_expand_epilogue ()
{
  rtx sp = stack_pointer_rtx;
  d30v_stack_t *info = d30v_stack_info ();
  int i;
  rtx mem_di = NULL_RTX;
  rtx mem_si = NULL_RTX;
  rtx post_inc;
  int extra_stack;

  /* Hack for now, to prevent scheduler from being too cleaver */
  emit_insn (gen_blockage ());

  /* Restore sp from fp.  */
  if (frame_pointer_needed)
    emit_move_insn (sp, frame_pointer_rtx);

  /* For the epilogue, use post-increment addressing all of the time.  First
     adjust the sp, to eliminate all of the stack, except for the save area.  */

  if (info->save_offset)
    emit_insn (gen_addsi3 (sp, sp, GEN_INT (info->save_offset)));

  post_inc = gen_rtx (POST_INC, Pmode, sp);
  mem_di = gen_rtx (MEM, DImode, post_inc);
  mem_si = gen_rtx (MEM, SImode, post_inc);

  /* Restore the accumulators.  */
  for (i = ACCUM_FIRST; i <= ACCUM_LAST; i++)
    if (info->save_p[i])
      {
	rtx acc_tmp = gen_rtx (REG, DImode, GPR_ATMP_FIRST);
	emit_insn (gen_movdi (acc_tmp, mem_di));
	emit_insn (gen_movdi (gen_rtx (REG, DImode, i), acc_tmp));
      }

  /* Restore the GPR registers that are adjacent to each other with ld2w.  */
  for (i = GPR_FIRST; i <= GPR_LAST; i += 2)
    if (info->save_p[i] == 2)
      emit_insn (gen_movdi (gen_rtx (REG, DImode, i), mem_di));

  /* Save the GPR registers that need to be saved with a single word store.  */
  extra_stack = 0;
  for (i = GPR_FIRST; i <= GPR_LAST; i++)
    if (info->save_p[i] == 1)
      {
	if (cfun->machine->eh_epilogue_sp_ofs && i == GPR_LINK)
	  extra_stack = 4;
	else
	  {
	    if (extra_stack)
	      {
	        emit_insn (gen_addsi3 (sp, sp, GEN_INT (extra_stack)));
		extra_stack = 0;
	      }
	    emit_insn (gen_movsi (gen_rtx (REG, SImode, i), mem_si));
	  }
      }

  /* Release any remaining stack that was allocated for saving the
     varargs registers or because an odd # of registers were stored.  */
  if ((info->memrefs_1word & 1) != 0)
    extra_stack += UNITS_PER_WORD;
  extra_stack += current_function_pretend_args_size + info->varargs_size;

  if (extra_stack)
    {
      if (cfun->machine->eh_epilogue_sp_ofs)
	emit_insn (gen_addsi3 (cfun->machine->eh_epilogue_sp_ofs,
			       cfun->machine->eh_epilogue_sp_ofs,
			       GEN_INT (extra_stack)));
      else
        emit_insn (gen_addsi3 (sp, sp, GEN_INT (extra_stack)));
    }
  if (cfun->machine->eh_epilogue_sp_ofs)
    emit_insn (gen_addsi3 (sp, sp, cfun->machine->eh_epilogue_sp_ofs));

  /* Now emit the return instruction.  */
  emit_jump_insn (gen_rtx_RETURN (VOIDmode));
}


/* A C statement or compound statement to output to FILE some assembler code to
   call the profiling subroutine `mcount'.  Before calling, the assembler code
   must load the address of a counter variable into a register where `mcount'
   expects to find the address.  The name of this variable is `LP' followed by
   the number LABELNO, so you would generate the name using `LP%d' in a
   `fprintf'.

   The details of how the address should be passed to `mcount' are determined
   by your operating system environment, not by GNU CC.  To figure them out,
   compile a small program for profiling using the system's installed C
   compiler and look at the assembler code that results.  */

void
d30v_function_profiler (stream, labelno)
     FILE *stream;
     int labelno ATTRIBUTE_UNUSED;
{
  fprintf (stream, "# profile\n");
}


/* Split a 64 bit item into an upper and a lower part.  We specifically do not
   want to call gen_highpart/gen_lowpart on CONST_DOUBLEs since it will give us
   the wrong part for floating point in cross compilers, and split_double does
   not handle registers.  Also abort if the register is not a general purpose
   register.  */

void
d30v_split_double (value, p_high, p_low)
     rtx value;
     rtx *p_high;
     rtx *p_low;
{
  int offset = 0;
  int regno;

  if (!reload_completed)
    abort ();

  switch (GET_CODE (value))
    {
    case SUBREG:
      if (GET_CODE (SUBREG_REG (value)) != REG)
	abort ();
      offset = subreg_regno_offset (REGNO (SUBREG_REG (value)),
				    GET_MODE (SUBREG_REG (value)),
				    SUBREG_BYTE (value),
				    GET_MODE (value));
      value = SUBREG_REG (value);

      /* fall through */

    case REG:
      regno = REGNO (value) + offset;
      if (!GPR_P (regno))
	abort ();

      *p_high = gen_rtx (REG, SImode, regno);
      *p_low =  gen_rtx (REG, SImode, regno+1);
      break;

    case CONST_INT:
    case CONST_DOUBLE:
      split_double (value, p_high, p_low);
      break;

    default:
      abort ();
    }
}


/* A C compound statement to output to stdio stream STREAM the assembler syntax
   for an instruction operand that is a memory reference whose address is X.  X
   is an RTL expression.  */

void
d30v_print_operand_address (stream, x)
     FILE *stream;
     rtx x;
{
  if (GET_CODE (x) == MEM)
    x = XEXP (x, 0);

  switch (GET_CODE (x))
    {
    default:
      break;

    case REG:
      fputs (reg_names[ REGNO (x) ], stream);
      return;

    case CONST_INT:
      fprintf (stream, "%ld", (long) INTVAL (x));
      return;

    /* We wrap simple symbol refs inside a parenthesis, so that a name
       like `r2' is not taken for a register name.  */
    case SYMBOL_REF:
      fputs ("(", stream);
      assemble_name (stream, XSTR (x, 0));
      fputs (")", stream);
      return;

    case LABEL_REF:
    case CONST:
      output_addr_const (stream, x);
      return;
    }

  fatal_insn ("bad insn to d30v_print_operand_address:", x);
}


/* Print a memory reference suitable for the ld/st instructions.  */

static void
d30v_print_operand_memory_reference (stream, x)
     FILE *stream;
     rtx x;
{
  rtx x0 = NULL_RTX;
  rtx x1 = NULL_RTX;

  switch (GET_CODE (x))
    {
    default:
      fatal_insn ("bad insn to d30v_print_operand_memory_reference:", x);
      break;

    case SUBREG:
    case REG:
    case POST_DEC:
    case POST_INC:
      x0 = x;
      break;

    case CONST_INT:
    case SYMBOL_REF:
    case LABEL_REF:
    case CONST:
      x1 = x;
      break;

    case PLUS:
      x0 = XEXP (x, 0);
      x1 = XEXP (x, 1);
      if (GET_CODE (x0) == CONST_INT || GET_CODE (x0) == SYMBOL_REF
	  || GET_CODE (x0) == CONST || GET_CODE (x0) == LABEL_REF)
	{
	  x0 = XEXP (x, 1);
	  x1 = XEXP (x, 0);
	}
      break;
    }

  fputs ("@(", stream);
  if (!x0)
    fputs (reg_names[GPR_R0], stream);

  else
    {
      const char *suffix = "";
      int offset0  = 0;

      if (GET_CODE (x0) == SUBREG)
	{
	  offset0 = subreg_regno_offset (REGNO (SUBREG_REG (x0)),
					 GET_MODE (SUBREG_REG (x0)),
					 SUBREG_BYTE (x0),
					 GET_MODE (x0));
	  x0 = SUBREG_REG (x0);
	}

      if (GET_CODE (x0) == POST_INC)
	{
	  x0 = XEXP (x0, 0);
	  suffix = "+";
	}
      else if (GET_CODE (x0) == POST_DEC)
	{
	  x0 = XEXP (x0, 0);
	  suffix = "-";
	}

      if (GET_CODE (x0) == REG && GPR_P (REGNO (x0)))
	fprintf (stream, "%s%s", reg_names[REGNO (x0) + offset0], suffix);
      else
	fatal_insn ("bad insn to d30v_print_operand_memory_reference:", x);
    }

  fputs (",", stream);

  if (!x1)
    fputs (reg_names[GPR_R0], stream);

  else
    {
      int offset1 = 0;

      switch (GET_CODE (x1))
	{
	case SUBREG:
	  offset1 = subreg_regno_offset (REGNO (SUBREG_REG (x1)),
					 GET_MODE (SUBREG_REG (x1)),
					 SUBREG_BYTE (x1),
					 GET_MODE (x1));
	  x1 = SUBREG_REG (x1);
	  if (GET_CODE (x1) != REG)
	    fatal_insn ("bad insn to d30v_print_operand_memory_reference:", x);

	  /* fall through */
	case REG:
	  fputs (reg_names[REGNO (x1) + offset1], stream);
	  break;

	case CONST_INT:
	  fprintf (stream, "%ld", (long) INTVAL (x1));
	  break;

	case SYMBOL_REF:
	case LABEL_REF:
	case CONST:
	  d30v_print_operand_address (stream, x1);
	  break;

	default:
	  fatal_insn ("bad insn to d30v_print_operand_memory_reference:", x);
	}
    }

  fputs (")", stream);
}


/* A C compound statement to output to stdio stream STREAM the assembler syntax
   for an instruction operand X.  X is an RTL expression.

   LETTER is a value that can be used to specify one of several ways of
   printing the operand.  It is used when identical operands must be printed
   differently depending on the context.  LETTER comes from the `%'
   specification that was used to request printing of the operand.  If the
   specification was just `%DIGIT' then LETTER is 0; if the specification was
   `%LTR DIGIT' then LETTER is the ASCII code for LTR.

   If X is a register, this macro should print the register's name.  The names
   can be found in an array `reg_names' whose type is `char *[]'.  `reg_names'
   is initialized from `REGISTER_NAMES'.

   When the machine description has a specification `%PUNCT' (a `%' followed by
   a punctuation character), this macro is called with a null pointer for X and
   the punctuation character for LETTER.

   Standard operand flags that are handled elsewhere:
	`='  Output a number unique to each instruction in the compilation.
	`a'  Substitute an operand as if it were a memory reference.
	`c'  Omit the syntax that indicates an immediate operand.
	`l'  Substitute a LABEL_REF into a jump instruction.
	`n'  Like %cDIGIT, except negate the value before printing.

   The d30v specific operand flags are:
	`.'  Print r0.
	`f'  Print a SF constant as an int.
	`s'  Subtract 32 and negate.
	`A'  Print accumulator number without an `a' in front of it.
	`B'  Print bit offset for BSET, etc. instructions.
	`E'  Print u if this is zero extend, nothing if this is sign extend.
	`F'  Emit /{f,t,x}{f,t,x} for executing a false condition.
	`L'  Print the lower half of a 64 bit item.
	`M'  Print a memory reference for ld/st instructions.
	`R'  Return appropriate cmp instruction for relational test.
	`S'  Subtract 32.
	`T'  Emit /{f,t,x}{f,t,x} for executing a true condition.
	`U'  Print the upper half of a 64 bit item.  */

void
d30v_print_operand (stream, x, letter)
     FILE *stream;
     rtx x;
     int letter;
{
  enum rtx_code code = (x) ? GET_CODE (x) : NIL;
  rtx split_values[2];
  REAL_VALUE_TYPE rv;
  long num;
  int log;

  switch (letter)
    {
    case '.':	/* Output r0 */
      fputs (reg_names[GPR_R0], stream);
      break;

    case 'f':	/* Print a SF floating constant as an int */
      if (GET_CODE (x) != CONST_DOUBLE)
	fatal_insn ("bad insn to d30v_print_operand, 'f' modifier:", x);

      REAL_VALUE_FROM_CONST_DOUBLE (rv, x);
      REAL_VALUE_TO_TARGET_SINGLE (rv, num);
      fprintf (stream, "%ld", num);
      break;

    case 'A':	/* Print accumulator number without an `a' in front of it.  */
      if (GET_CODE (x) != REG || !ACCUM_P (REGNO (x)))
	fatal_insn ("bad insn to d30v_print_operand, 'A' modifier:", x);

      putc ('0' + REGNO (x) - ACCUM_FIRST, stream);
      break;

    case 'M':	/* Print a memory reference for ld/st */
      if (GET_CODE (x) != MEM)
	fatal_insn ("bad insn to d30v_print_operand, 'M' modifier:", x);

      d30v_print_operand_memory_reference (stream, XEXP (x, 0));
      break;

    case 'L':	/* print lower part of 64 bit item. */
    case 'U':	/* print upper part of 64 bit item. */
      d30v_split_double (x, &split_values[0], &split_values[1]);
      d30v_print_operand (stream, split_values[ letter == 'L' ], '\0');
      break;

    case ':':   /* Output the condition for the current insn.  */
      x = current_insn_predicate;
      if (x == NULL_RTX)
	break;
      letter = 'T';
      /* FALLTHRU */

    case 'F':	/* Print an appropriate suffix for a false comparision.  */
    case 'T':	/* Print an appropriate suffix for a true  comparision.  */
      /* Note that the sense of appropriate suffix is for conditional execution
	 and opposite of what branches want.  Branches just use the inverse
	 operation.  */
      if ((GET_CODE (x) == NE || GET_CODE (x) == EQ)
	  && GET_MODE (x) == CCmode
	  && GET_CODE (XEXP (x, 0)) == REG
	  && (GPR_P (REGNO (XEXP (x, 0))) || BR_FLAG_P (REGNO (XEXP (x, 0))))
	  && GET_CODE (XEXP (x, 1)) == CONST_INT && INTVAL (XEXP (x, 1)) == 0)
	{
	  int true_false = (letter == 'T');

	  if (GET_CODE (x) == EQ)
	    true_false = !true_false;

	  if (REGNO (XEXP (x, 0)) == FLAG_F0)
	    fprintf (stream, "/%cx", (true_false) ? 'f' : 't');

	  else if (REGNO (XEXP (x, 0)) == FLAG_F1)
	    fprintf (stream, "/x%c", (true_false) ? 'f' : 't');

	  else
	    fputs ((true_false) ? "tnz" : "tzr", stream);
	}

      else if (GET_CODE (x) == REG && REGNO (x) == FLAG_F0)
	fprintf (stream, "/%cx", (letter == 'T') ? 't' : 'f');

      else if (GET_CODE (x) == REG && REGNO (x) == FLAG_F1)
	fprintf (stream, "/x%c", (letter == 'T') ? 't' : 'f');

      else if (GET_CODE (x) == REG && GPR_P (REGNO (x)))
	fputs ((letter == 'T') ? "tnz" : "tzr", stream);

      else
	fatal_insn ("bad insn to print_operand, 'F' or 'T' modifier:", x);
      break;

    case 'B':	/* emit offset single bit to change */
      if (GET_CODE (x) == CONST_INT && (log = exact_log2 (INTVAL (x))) >= 0)
	fprintf (stream, "%d", 31 - log);

      else if (GET_CODE (x) == CONST_INT && (log = exact_log2 (~ INTVAL (x))) >= 0)
	fprintf (stream, "%d", 31 - log);

      else
	fatal_insn ("bad insn to print_operand, 'B' modifier:", x);
      break;

    case 'E':	/* Print u if this is zero extend, nothing if sign extend. */
      if (GET_CODE (x) == ZERO_EXTEND)
	putc ('u', stream);
      else if (GET_CODE (x) != SIGN_EXTEND)
	fatal_insn ("bad insn to print_operand, 'E' modifier:", x);
      break;

    case 'R':	/* Return appropriate cmp instruction for relational test.  */
      switch (GET_CODE (x))
	{
	case EQ:  fputs ("cmpeq",  stream); break;
	case NE:  fputs ("cmpne",  stream); break;
	case LT:  fputs ("cmplt",  stream); break;
	case LE:  fputs ("cmple",  stream); break;
	case GT:  fputs ("cmpgt",  stream); break;
	case GE:  fputs ("cmpge",  stream); break;
	case LTU: fputs ("cmpult", stream); break;
	case LEU: fputs ("cmpule", stream); break;
	case GTU: fputs ("cmpugt", stream); break;
	case GEU: fputs ("cmpuge", stream); break;

	default:
	  fatal_insn ("bad insn to print_operand, 'R' modifier:", x);
	}
      break;

    case 's':	/* Subtract 32 and negate (for 64 bit shifts).  */
      if (GET_CODE (x) == CONST_INT)
	fprintf (stream, "%d", (int) (32 - INTVAL (x)));

      else
	fatal_insn ("bad insn to print_operand, 's' modifier:", x);
      break;

    case 'S':	/* Subtract 32.  */
      if (GET_CODE (x) == CONST_INT)
	fprintf (stream, "%d", (int)(INTVAL (x) - 32));

      else
	fatal_insn ("bad insn to print_operand, 's' modifier:", x);
      break;


    case 'z':	/* If arg is 0 or 0.0, print r0, otherwise print as normal */
      if ((GET_CODE (x) == CONST_INT && INTVAL (x) == 0)
	  || (GET_CODE (x) == CONST_DOUBLE && CONST_DOUBLE_LOW (x) == 0
	      && CONST_DOUBLE_HIGH (x) == 0))
	{
	  fputs (reg_names[GPR_FIRST], stream);
	  return;
	}

      /* fall through */

    case '\0':
      if (code == REG)
	fputs (reg_names[ REGNO (x) ], stream);

      else if (code == CONST_INT)
	fprintf (stream, "%d", (int)INTVAL (x));

      else if (code == MEM)
	d30v_print_operand_address (stream, XEXP (x, 0));

      else if (CONSTANT_ADDRESS_P (x))
	d30v_print_operand_address (stream, x);

      else
	fatal_insn ("bad insn in d30v_print_operand, 0 case", x);

      return;

    default:
      {
	char buf[80];

	sprintf (buf, "invalid asm template character '%%%c'", letter);
	fatal_insn (buf, x);
      }
    }
}


/* A C expression for the size in bytes of the trampoline, as an integer.  */

int
d30v_trampoline_size ()
{
  return 16;
}


/* Create a long instruction for building up a trampoline.  */

static void
d30v_build_long_insn (high_bits, low_bits, imm, mem)
     HOST_WIDE_INT high_bits;
     HOST_WIDE_INT low_bits;
     rtx imm;
     rtx mem;
{
  rtx reg = gen_reg_rtx (DImode);
  rtx high_word = gen_highpart (SImode, reg);
  rtx low_word = gen_lowpart (SImode, reg);
  rtx tmp1 = gen_reg_rtx (SImode);
  rtx tmp2 = gen_reg_rtx (SImode);
  rtx tmp3 = gen_reg_rtx (SImode);
  rtx tmp4 = gen_reg_rtx (SImode);
  rtx tmp5 = gen_reg_rtx (SImode);
  rtx tmp6 = gen_reg_rtx (SImode);

  imm = force_reg (SImode, imm);

  /* Stuff top 6 bits of immediate value into high word */
  emit_insn (gen_lshrsi3 (tmp1, imm, GEN_INT (26)));
  emit_insn (gen_andsi3 (tmp2, tmp1, GEN_INT (0x3F)));
  emit_insn (gen_iorsi3 (high_word, tmp2, GEN_INT (high_bits)));

  /* Now get the next 8 bits for building the low word */
  emit_insn (gen_andsi3 (tmp3, imm, GEN_INT (0x03FC0000)));
  emit_insn (gen_ashlsi3 (tmp4, tmp3, GEN_INT (2)));

  /* And the bottom 18 bits */
  emit_insn (gen_andsi3 (tmp5, imm, GEN_INT (0x0003FFFF)));
  emit_insn (gen_iorsi3 (tmp6, tmp4, tmp5));
  emit_insn (gen_iorsi3 (low_word, tmp6, GEN_INT (low_bits)));

  /* Store the instruction */
  emit_insn (gen_movdi (mem, reg));
}


/* A C statement to initialize the variable parts of a trampoline.  ADDR is an
   RTX for the address of the trampoline; FNADDR is an RTX for the address of
   the nested function; STATIC_CHAIN is an RTX for the static chain value that
   should be passed to the function when it is called.  */

void
d30v_initialize_trampoline (addr, fnaddr, static_chain)
     rtx addr;
     rtx fnaddr;
     rtx static_chain;
{
  /* The instruction space can only be accessed by ld2w/st2w.
     Generate on the fly:
	or r18,r0,<static-chain>
	jmp <fnaddr> */
  d30v_build_long_insn (0x83A80000 | ((STATIC_CHAIN_REGNUM - GPR_FIRST) << 12),
			0x80000000, static_chain,
			gen_rtx (MEM, DImode, addr));

  d30v_build_long_insn (0x80180000, 0x80000000, fnaddr,
			gen_rtx (MEM, DImode, plus_constant (addr, 8)));
}


/* A C compound statement with a conditional `goto LABEL;' executed if X (an
   RTX) is a legitimate memory address on the target machine for a memory
   operand of mode MODE.  */

#define XREGNO_OK_FOR_BASE_P(REGNO, STRICT_P)				\
((STRICT_P)								\
 ? REGNO_OK_FOR_BASE_P (REGNO)						\
 : GPR_OR_PSEUDO_P (REGNO))

int
d30v_legitimate_address_p (mode, x, strict_p)
     enum machine_mode mode;
     rtx x;
     int strict_p;
{
  rtx x0, x1;
  int ret = 0;

  switch (GET_CODE (x))
    {
    default:
      break;

    case SUBREG:
      x = SUBREG_REG (x);
      if (GET_CODE (x) != REG)
	break;

      /* fall through */

    case REG:
      ret = XREGNO_OK_FOR_BASE_P (REGNO (x), strict_p);
      break;

    case PLUS:
      x0 = XEXP (x, 0);
      x1 = XEXP (x, 1);

      if (GET_CODE (x0) == SUBREG)
	x0 = SUBREG_REG (x0);

      if (GET_CODE (x0) == POST_INC || GET_CODE (x0) == POST_DEC)
	x0 = XEXP (x0, 0);

      if (GET_CODE (x0) != REG || !XREGNO_OK_FOR_BASE_P (REGNO (x0), strict_p))
	break;

      switch (GET_CODE (x1))
	{
	default:
	  break;

	case SUBREG:
	  x1 = SUBREG_REG (x1);
	  if (GET_CODE (x1) != REG)
	    break;

	  /* fall through */

	case REG:
	  ret = XREGNO_OK_FOR_BASE_P (REGNO (x1), strict_p);
	  break;

	case CONST_INT:
	  ret = (IN_RANGE_P (INTVAL (x1), -32, 31)) ? 1 : 2;
	  break;

	case SYMBOL_REF:
	case LABEL_REF:
	case CONST:
	  ret = 2;
	  break;
	}
      break;

    case CONST_INT:
      ret = (IN_RANGE_P (INTVAL (x), -32, 31)) ? 1 : 2;
      break;

    case SYMBOL_REF:
    case LABEL_REF:
    case CONST:
      ret = 2;
      break;

    case POST_INC:
    case POST_DEC:
      x0 = XEXP (x, 0);
      if (GET_CODE (x0) == REG && XREGNO_OK_FOR_BASE_P (REGNO (x0), strict_p))
	ret = 1;
      break;
    }

  if (TARGET_DEBUG_ADDR)
    {
      fprintf (stderr, "\n========== GO_IF_LEGITIMATE_ADDRESS, mode = %s, result = %d, addresses are %sstrict\n",
	       GET_MODE_NAME (mode), ret, (strict_p) ? "" : "not ");
      debug_rtx (x);
    }

  return ret;
}


/* A C compound statement that attempts to replace X with a valid memory
   address for an operand of mode MODE.  WIN will be a C statement label
   elsewhere in the code; the macro definition may use

        GO_IF_LEGITIMATE_ADDRESS (MODE, X, WIN);

   to avoid further processing if the address has become legitimate.

   X will always be the result of a call to `break_out_memory_refs', and OLDX
   will be the operand that was given to that function to produce X.

   The code generated by this macro should not alter the substructure of X.  If
   it transforms X into a more legitimate form, it should assign X (which will
   always be a C variable) a new value.

   It is not necessary for this macro to come up with a legitimate address.
   The compiler has standard ways of doing so in all cases.  In fact, it is
   safe for this macro to do nothing.  But often a machine-dependent strategy
   can generate better code.  */

rtx
d30v_legitimize_address (x, oldx, mode, strict_p)
     rtx x;
     rtx oldx ATTRIBUTE_UNUSED;
     enum machine_mode mode ATTRIBUTE_UNUSED;
     int strict_p ATTRIBUTE_UNUSED;
{
  rtx ret = NULL_RTX;

  if (TARGET_DEBUG_ADDR)
    {
      if (ret)
	{
	  fprintf (stderr, "\n========== LEGITIMIZE_ADDRESS, transformed:\n");
	  debug_rtx (x);
	  fprintf (stderr, "\ninto:\n");
	  debug_rtx (ret);
	}
      else
	{
	  fprintf (stderr, "\n========== LEGITIMIZE_ADDRESS, did nothing with:\n");
	  debug_rtx (x);
	}
    }

  return ret;
}


/* A C statement or compound statement with a conditional `goto LABEL;'
   executed if memory address X (an RTX) can have different meanings depending
   on the machine mode of the memory reference it is used for or if the address
   is valid for some modes but not others.

   Autoincrement and autodecrement addresses typically have mode-dependent
   effects because the amount of the increment or decrement is the size of the
   operand being addressed.  Some machines have other mode-dependent addresses.
   Many RISC machines have no mode-dependent addresses.

   You may assume that ADDR is a valid address for the machine.  */

int
d30v_mode_dependent_address_p (addr)
     rtx addr;
{
  switch (GET_CODE (addr))
    {
    default:
      break;

    case POST_INC:
    case POST_DEC:
      return TRUE;
    }

  return FALSE;
}


/* Generate the appropriate comparison code for a test.  */

rtx
d30v_emit_comparison (test_int, result, arg1, arg2)
     int test_int;
     rtx result;
     rtx arg1;
     rtx arg2;
{
  enum rtx_code test = (enum rtx_code) test_int;
  enum machine_mode mode = GET_MODE (arg1);
  rtx rtx_test = gen_rtx (SET, VOIDmode, result, gen_rtx (test, CCmode, arg1, arg2));

  if (mode == SImode
      || (mode == DImode && (test == EQ || test == NE))
      || (mode == DImode && (test == LT || test == GE)
	  && GET_CODE (arg2) == CONST_INT && INTVAL (arg2) == 0))
    return rtx_test;

  else if (mode == DImode)
    return gen_rtx (PARALLEL, VOIDmode,
		    gen_rtvec (2,
			       rtx_test,
			       gen_rtx (CLOBBER, VOIDmode,
					gen_reg_rtx (CCmode))));

  else
    fatal_insn ("d30v_emit_comparison", rtx_test);
}


/* Return appropriate code to move 2 words.  Since DImode registers must start
   on even register numbers, there is no possibility of overlap.  */

const char *
d30v_move_2words (operands, insn)
     rtx operands[];
     rtx insn;
{
  if (GET_CODE (operands[0]) == REG && GPR_P (REGNO (operands[0])))
    {
      if (GET_CODE (operands[1]) == REG && GPR_P (REGNO (operands[1])))
	return "or %U0,%.,%U1\n\tor %L0,%.,%L1";

      else if (GET_CODE (operands[1]) == REG && ACCUM_P (REGNO (operands[1])))
	return "mvfacc %L0,%1,%.\n\tmvfacc %U0,%1,32";

      else if (GET_CODE (operands[1]) == MEM)
	return "ld2w %0,%M1";

      else if (GET_CODE (operands[1]) == CONST_INT
	       || GET_CODE (operands[1]) == CONST_DOUBLE)
	return "or %U0,%.,%U1\n\tor %L0,%.,%L1";
    }

  else if (GET_CODE (operands[0]) == REG && ACCUM_P (REGNO (operands[0])))
    {
      if (GET_CODE (operands[1]) == REG
	  && GPR_P (REGNO (operands[1])))
	return "mvtacc %0,%U1,%L1";

      if (GET_CODE (operands[1]) == CONST_INT
	  && INTVAL (operands[1]) == 0)
	return "mvtacc %0,%.,%.";
    }

  else if (GET_CODE (operands[0]) == MEM
	   && GET_CODE (operands[1]) == REG
	   && GPR_P (REGNO (operands[1])))
    return "st2w %1,%M0";

  fatal_insn ("bad call to d30v_move_2words", insn);
}


/* Emit the code to do a conditional move instruction.  Return FALSE
   if the conditional move could not be executed.  */

int
d30v_emit_cond_move (dest, test, true_value, false_value)
     rtx dest;
     rtx test;
     rtx true_value;
     rtx false_value;
{
  rtx br_reg;
  enum machine_mode mode = GET_MODE (dest);
  int two_mem_moves_p = FALSE;

  if (GET_CODE (dest) == MEM)
    {
      if (!reg_or_0_operand (true_value, mode))
	return FALSE;

      if (rtx_equal_p (dest, false_value))
	two_mem_moves_p = TRUE;

      else if (!reg_or_0_operand (false_value, mode))
	return FALSE;
    }

  /* We used to try to optimize setting 0/1 by using mvfsys, but that turns out
     to be slower than just doing the conditional execution.  */

  br_reg = gen_reg_rtx (CCmode);
  emit_insn (d30v_emit_comparison (GET_CODE (test), br_reg,
				   d30v_compare_op0, d30v_compare_op1));

  if (!two_mem_moves_p)
    emit_insn (gen_rtx_SET (VOIDmode,
			    dest,
			    gen_rtx_IF_THEN_ELSE (mode,
						  gen_rtx_NE (CCmode, br_reg,
							      const0_rtx),
						  true_value,
						  false_value)));
  else
    {
      /* Emit conditional stores as two separate stores.  This avoids a problem
         where you have a conditional store, and one of the arms of the
         conditional store is spilled to memory.  */
      emit_insn (gen_rtx_SET (VOIDmode,
			      dest,
			      gen_rtx_IF_THEN_ELSE (mode,
						    gen_rtx_NE (CCmode, br_reg,
								const0_rtx),
						    true_value,
						    dest)));

      emit_insn (gen_rtx_SET (VOIDmode,
			      dest,
			      gen_rtx_IF_THEN_ELSE (mode,
						    gen_rtx_EQ (CCmode, br_reg,
								const0_rtx),
						    false_value,
						    dest)));
	 
    }

  return TRUE;
}


/* In rare cases, correct code generation requires extra machine dependent
   processing between the second jump optimization pass and delayed branch
   scheduling.  On those machines, define this macro as a C statement to act on
   the code starting at INSN.  */

void
d30v_machine_dependent_reorg (insn)
     rtx insn ATTRIBUTE_UNUSED;
{
}


/* A C statement (sans semicolon) to update the integer variable COST based on
   the relationship between INSN that is dependent on DEP_INSN through the
   dependence LINK.  The default is to make no adjustment to COST.  This can be
   used for example to specify to the scheduler that an output- or
   anti-dependence does not incur the same cost as a data-dependence.  */

/* For the d30v, try to insure that the source operands for a load/store are
   set 2 cycles before the memory reference.  */

static int
d30v_adjust_cost (insn, link, dep_insn, cost)
     rtx insn;
     rtx link ATTRIBUTE_UNUSED;
     rtx dep_insn;
     int cost;
{
  rtx set_dep = single_set (dep_insn);
  rtx set_insn = single_set (insn);

  if (set_dep != NULL_RTX && set_insn != NULL_RTX
      && GET_CODE (SET_DEST (set_dep)) == REG)
    {
      rtx reg = SET_DEST (set_dep);
      rtx mem;

      if ((GET_CODE (mem = SET_SRC (set_insn)) == MEM
	   && reg_mentioned_p (reg, XEXP (mem, 0)))
	  || (GET_CODE (mem = SET_DEST (set_insn)) == MEM
	      && reg_mentioned_p (reg, XEXP (mem, 0))))
	{
	  return cost + 2;
	}
    }

  return cost;
}

/* Function which returns the number of insns that can be
   scheduled in the same machine cycle.  This must be constant
   over an entire compilation.  The default is 1.  */
static int
d30v_issue_rate ()
{
  return 2;
}


/* Routine to allocate, mark and free a per-function,
   machine specific structure.  */

static struct machine_function *
d30v_init_machine_status ()
{
  return ggc_alloc_cleared (sizeof (machine_function));
}

/* Do anything needed before RTL is emitted for each function.  */

void
d30v_init_expanders ()
{
  /* Arrange to save and restore machine status around nested functions.  */
  init_machine_status = d30v_init_machine_status;
}

/* Find the current function's return address.

   ??? It would be better to arrange things such that if we would ordinarily
   have been a leaf function and we didn't spill the hard reg that we
   wouldn't have to save the register in the prolog.  But it's not clear
   how to get the right information at the right time.  */

rtx
d30v_return_addr ()
{
  return get_hard_reg_initial_val (Pmode, GPR_LINK);
}
