/* Subroutines for assembler code output on the DSP1610.
   Copyright (C) 1994, 1995, 1997, 1998, 2001 Free Software Foundation, Inc.
   Contributed by Michael Collison (collison@isisinc.net).

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

/* Some output-actions in dsp1600.md need these.  */
#include "config.h"
#include "system.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "output.h"
#include "insn-attr.h"
#include "tree.h"
#include "expr.h"
#include "function.h"
#include "flags.h"
#include "ggc.h"
#include "toplev.h"
#include "recog.h"
#include "tm_p.h"
#include "target.h"
#include "target-def.h"

const char *text_seg_name;
const char *rsect_text;
const char *data_seg_name;
const char *rsect_data;
const char *bss_seg_name;
const char *rsect_bss;
const char *const_seg_name;
const char *rsect_const;

const char *chip_name;
const char *save_chip_name;

/* Save the operands of a compare. The 16xx has not lt or gt, so
   in these cases we swap the operands and reverse the condition.  */

rtx dsp16xx_compare_op0;
rtx dsp16xx_compare_op1;
rtx (*dsp16xx_compare_gen) PARAMS (());

static const char *fp;
static const char *sp;
static const char *rr;
static const char *a1h;

struct dsp16xx_frame_info current_frame_info;
struct dsp16xx_frame_info zero_frame_info;

rtx dsp16xx_addhf3_libcall = (rtx) 0;
rtx dsp16xx_subhf3_libcall = (rtx) 0;
rtx dsp16xx_mulhf3_libcall = (rtx) 0;
rtx dsp16xx_divhf3_libcall = (rtx) 0;
rtx dsp16xx_cmphf3_libcall = (rtx) 0;
rtx dsp16xx_fixhfhi2_libcall = (rtx) 0;
rtx dsp16xx_floathihf2_libcall = (rtx) 0;
rtx dsp16xx_neghf2_libcall = (rtx) 0;

rtx dsp16xx_mulhi3_libcall = (rtx) 0;
rtx dsp16xx_udivqi3_libcall = (rtx) 0;
rtx dsp16xx_udivhi3_libcall = (rtx) 0;
rtx dsp16xx_divqi3_libcall = (rtx) 0;
rtx dsp16xx_divhi3_libcall = (rtx) 0;
rtx dsp16xx_modqi3_libcall = (rtx) 0;
rtx dsp16xx_modhi3_libcall = (rtx) 0;
rtx dsp16xx_umodqi3_libcall = (rtx) 0;
rtx dsp16xx_umodhi3_libcall = (rtx) 0;
rtx dsp16xx_ashrhi3_libcall = (rtx) 0;
rtx dsp16xx_ashlhi3_libcall = (rtx) 0;
rtx dsp16xx_ucmphi2_libcall = (rtx) 0;
rtx dsp16xx_lshrhi3_libcall = (rtx) 0;

static const char *const himode_reg_name[] = HIMODE_REGISTER_NAMES;

#define SHIFT_INDEX_1   0
#define SHIFT_INDEX_4   1
#define SHIFT_INDEX_8   2
#define SHIFT_INDEX_16  3

static const char *const ashift_right_asm[] = 
{
  "%0=%0>>1",
  "%0=%0>>4",
  "%0=%0>>8",
  "%0=%0>>16"
};

static const char *const ashift_right_asm_first[] = 
{
  "%0=%1>>1",
  "%0=%1>>4",
  "%0=%1>>8",
  "%0=%1>>16"
};

static const char *const ashift_left_asm[] = 
{
  "%0=%0<<1",
  "%0=%0<<4",
  "%0=%0<<8",
  "%0=%0<<16"
};

static const char *const ashift_left_asm_first[] = 
{
  "%0=%1<<1",
  "%0=%1<<4",
  "%0=%1<<8",
  "%0=%1<<16"
};

static const char *const lshift_right_asm[] = 
{
  "%0=%0>>1\n\t%0=%b0&0x7fff",
  "%0=%0>>4\n\t%0=%b0&0x0fff",
  "%0=%0>>8\n\t%0=%b0&0x00ff",
  "%0=%0>>16\n\t%0=%b0&0x0000"
};

static const char *const lshift_right_asm_first[] = 
{
  "%0=%1>>1\n\t%0=%b0&0x7fff",
  "%0=%1>>4\n\t%0=%b0&0x0fff",
  "%0=%1>>8\n\t%0=%b0&0x00ff",
  "%0=%1>>16\n\t%0=%b0&0x0000"
};

static int reg_save_size PARAMS ((void));
static void dsp16xx_output_function_prologue PARAMS ((FILE *, HOST_WIDE_INT));
static void dsp16xx_output_function_epilogue PARAMS ((FILE *, HOST_WIDE_INT));

/* Initialize the GCC target structure.  */
#undef TARGET_ASM_BYTE_OP
#define TARGET_ASM_BYTE_OP "\tint\t"
#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP NULL
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP NULL

#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE dsp16xx_output_function_prologue
#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE dsp16xx_output_function_epilogue

struct gcc_target targetm = TARGET_INITIALIZER;

int 
hard_regno_mode_ok (regno, mode)
     int regno;
     enum machine_mode mode;
{
  switch ((int) mode)
    {
    case VOIDmode:
      return 1;
      
      /* We can't use the c0-c2 for QImode, since they are only
	 8 bits in length.  */

    case QImode:
      if (regno != REG_C0 && regno != REG_C1 && regno != REG_C2)
	return 1;
      else
	return 0;
      
      /* We only allow a0, a1, y, and p to be allocated for 32-bit modes.
         Additionally we allow the virtual ybase registers to be used for 32-bit
	 modes.  */
      
    case HFmode:
    case SFmode:
    case DFmode:
    case XFmode:
    case HImode:
    case SImode:
    case DImode:
      if (regno == REG_A0 || regno == REG_A1 || regno == REG_Y || regno == REG_PROD
	  || (IS_YBASE_REGISTER_WINDOW(regno) && ((regno & 1) == 0)))
	return 1;
      else
	return 0;
      
    default:
      return 0;
    }
}

enum reg_class
dsp16xx_reg_class_from_letter (c)
     int c;
{
  switch (c)
    {
    case 'A':
      return ACCUM_REGS;

    case 'l':
      return A0_REG;

    case 'C':
      return A1_REG;
      
    case 'h':
      return ACCUM_HIGH_REGS;
      
    case 'j':
      return A0H_REG;
      
    case 'k':
      return A0L_REG;
      
    case 'q':
      return A1H_REG;
      
    case 'u':
      return A1L_REG;
      
    case 'x':
      return X_REG;

    case 'y':
      return YH_REG;

    case 'z':
      return YL_REG;

    case 't':
      return P_REG;

    case 'Z':
      return Y_OR_P_REGS;

    case 'd':
      return ACCUM_Y_OR_P_REGS;

    case 'a':
      return Y_ADDR_REGS;

    case 'B':
      return (TARGET_BMU ? BMU_REGS : NO_REGS);

    case 'Y':
      return YBASE_VIRT_REGS;

    case 'v':
      return PH_REG;

    case 'w':
      return PL_REG;

    case 'W':
      return J_REG;

    case 'e':
      return YBASE_ELIGIBLE_REGS;

    case 'b':
      return ACCUM_LOW_REGS;

    case 'c':
      return NON_YBASE_REGS;

    case 'f':
      return Y_REG;

    case 'D':
      return SLOW_MEM_LOAD_REGS;

    default:
      return NO_REGS;
    }
}

/* Return the class number of the smallest class containing
   reg number REGNO.  */

int 
regno_reg_class(regno)
     int regno;
{
  switch (regno)
    {
    case REG_A0L:
      return (int) A0L_REG;
    case REG_A1L:
      return (int) A1L_REG;
      
    case REG_A0:
      return (int) A0H_REG;
    case REG_A1:
      return (int) A1H_REG;
      
    case REG_X:
      return (int) X_REG;
      
    case REG_Y:
      return (int) YH_REG;
    case REG_YL:
      return (int) YL_REG;
      
    case REG_PROD:
      return (int) PH_REG;
    case REG_PRODL:
      return (int) PL_REG;
      
    case REG_R0: case REG_R1: case REG_R2: case REG_R3:
      return (int) Y_ADDR_REGS;
      
    case REG_J:
      return (int) J_REG;
    case REG_K:
      return (int) GENERAL_REGS;
      
    case REG_YBASE:
      return (int) GENERAL_REGS;
      
    case REG_PT:
      return (int) GENERAL_REGS;
      
    case REG_AR0: case REG_AR1: case REG_AR2: case REG_AR3:
      return (int) BMU_REGS;
      
    case REG_C0: case REG_C1: case REG_C2:
      return (int) GENERAL_REGS;
      
    case REG_PR:
      return (int) GENERAL_REGS;
      
    case REG_RB:
      return (int) GENERAL_REGS;
      
    case REG_YBASE0: case REG_YBASE1: case REG_YBASE2: case REG_YBASE3:
    case REG_YBASE4: case REG_YBASE5: case REG_YBASE6: case REG_YBASE7:
    case REG_YBASE8: case REG_YBASE9: case REG_YBASE10: case REG_YBASE11:
    case REG_YBASE12: case REG_YBASE13: case REG_YBASE14: case REG_YBASE15:
    case REG_YBASE16: case REG_YBASE17: case REG_YBASE18: case REG_YBASE19:
    case REG_YBASE20: case REG_YBASE21: case REG_YBASE22: case REG_YBASE23:
    case REG_YBASE24: case REG_YBASE25: case REG_YBASE26: case REG_YBASE27:
    case REG_YBASE28: case REG_YBASE29: case REG_YBASE30: case REG_YBASE31:
      return (int) YBASE_VIRT_REGS;
      
    default:
      return (int) NO_REGS;
    }
}

/* A C expression for the maximum number of consecutive registers of class CLASS
   needed to hold a value of mode MODE.  */

int
class_max_nregs(class, mode)
     enum reg_class class ATTRIBUTE_UNUSED;
     enum machine_mode mode;
{
    return (GET_MODE_SIZE(mode));
}

enum reg_class
limit_reload_class (mode, class)
     enum machine_mode mode ATTRIBUTE_UNUSED;
     enum reg_class class;
{
  return class;
}

int
dsp16xx_register_move_cost (from, to)
     enum reg_class from, to;
{
  if (from == A0H_REG || from == A0L_REG || from == A0_REG ||
      from == A1H_REG || from == ACCUM_HIGH_REGS || from == A1L_REG ||
      from == ACCUM_LOW_REGS || from == A1_REG || from == ACCUM_REGS)
    {
      if (to == Y_REG || to == P_REG)
	return 4;
      else
	return 2;
    }

  if (to == A0H_REG || to == A0L_REG || to == A0_REG ||
      to == A1H_REG || to == ACCUM_HIGH_REGS || to == A1L_REG ||
      to == ACCUM_LOW_REGS || to == A1_REG || to == ACCUM_REGS)
    {
      return 2;
    }

  if (from == YBASE_VIRT_REGS)
    {
      if (to == YBASE_VIRT_REGS)
	return 16;

      if (to == X_REG || to == YH_REG || to == YL_REG ||
	  to == Y_REG || to == PL_REG || to == PH_REG ||
	  to == P_REG || to == Y_ADDR_REGS || to == YBASE_ELIGIBLE_REGS ||
	  to == Y_OR_P_REGS)
	{
	  return 8;
	}
      else
	return 10;
    }

  if (to == YBASE_VIRT_REGS)
    {
      if (from == X_REG || from == YH_REG || from == YL_REG ||
	  from == Y_REG || from == PL_REG || from == PH_REG ||
	  from == P_REG || from == Y_ADDR_REGS || from == YBASE_ELIGIBLE_REGS ||
	  from == Y_OR_P_REGS)
	{
	  return 8;
	}
      else
	return 10;
    }

  return 8;
}

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.
   Also, we must ensure that a PLUS is reloaded either
   into an accumulator or an address register.  */

enum reg_class
preferred_reload_class (x, class)
     rtx x;
     enum reg_class class;
{
  /* The ybase registers cannot have constants copied directly
     to them.  */

  if (CONSTANT_P (x))
    {
      switch ((int) class)
	{
	case YBASE_VIRT_REGS:
	  return (!reload_in_progress ? NO_REGS : class);

	case ACCUM_LOW_OR_YBASE_REGS:
	  return ACCUM_LOW_REGS;

	case ACCUM_OR_YBASE_REGS:
	  return ACCUM_REGS;

	case X_OR_YBASE_REGS:
	  return X_REG;

	case Y_OR_YBASE_REGS:
	  return Y_REG;

	case ACCUM_LOW_YL_PL_OR_YBASE_REGS:
	  return YL_OR_PL_OR_ACCUM_LOW_REGS;

	case P_OR_YBASE_REGS:
	  return P_REG;

	case ACCUM_Y_P_OR_YBASE_REGS:
	  return ACCUM_Y_OR_P_REGS;

	case Y_ADDR_OR_YBASE_REGS:
	  return Y_ADDR_REGS;

	case YBASE_OR_NOHIGH_YBASE_ELIGIBLE_REGS:
	  return NON_HIGH_YBASE_ELIGIBLE_REGS;;
	  
	case YBASE_OR_YBASE_ELIGIBLE_REGS:
	  return YBASE_ELIGIBLE_REGS;

	case NO_HIGH_ALL_REGS:
	  return NOHIGH_NON_YBASE_REGS;

	case ALL_REGS:
	  return NON_YBASE_REGS;

	default:
	  return class;
	}
    }

  /* If x is not an accumulator or a ybase register, restrict the class of registers
     we can copy the register into.  */

  if (REG_P (x) && !IS_ACCUM_REG (REGNO (x)) && !IS_YBASE_REGISTER_WINDOW (REGNO (x)))
    {
      switch ((int) class)
	{
	case NO_REGS:
	case A0H_REG: case A0L_REG: case A0_REG: case A1H_REG:
	case ACCUM_HIGH_REGS: case A1L_REG: case ACCUM_LOW_REGS: 
	case A1_REG: case ACCUM_REGS:
	  return class;

	case X_REG: 
	  return (!reload_in_progress ? NO_REGS : class);

	case X_OR_ACCUM_LOW_REGS: 
	  return ACCUM_LOW_REGS;

	case X_OR_ACCUM_REGS:
	  return ACCUM_REGS;

	case YH_REG:
	  return (!reload_in_progress ? NO_REGS : class);

	case YH_OR_ACCUM_HIGH_REGS:
	  return ACCUM_HIGH_REGS;

	case X_OR_YH_REGS: 
	case YL_REG:
	  return (!reload_in_progress ? NO_REGS : class);

	case YL_OR_ACCUM_LOW_REGS: 
	  return ACCUM_LOW_REGS;

	case X_OR_YL_REGS:
	case X_OR_Y_REGS: case Y_REG:
	  return (!reload_in_progress ? NO_REGS : class);

	case ACCUM_OR_Y_REGS: 
	  return ACCUM_REGS;

	case PH_REG:
	case X_OR_PH_REGS: case PL_REG: 
	  return (!reload_in_progress ? NO_REGS : class);

	case PL_OR_ACCUM_LOW_REGS:
 	  return ACCUM_LOW_REGS;

	case X_OR_PL_REGS:
	  return (!reload_in_progress ? NO_REGS : class);

	case YL_OR_PL_OR_ACCUM_LOW_REGS: 
 	  return ACCUM_LOW_REGS;

	case P_REG:
	  return (!reload_in_progress ? NO_REGS : class);

	case ACCUM_OR_P_REGS: 
	  return ACCUM_REGS;

	case YL_OR_P_REGS:
	  return (!reload_in_progress ? NO_REGS : class);

	case ACCUM_LOW_OR_YL_OR_P_REGS: 
 	  return ACCUM_LOW_REGS;

	case Y_OR_P_REGS:
	  return (!reload_in_progress ? NO_REGS : class);

	case ACCUM_Y_OR_P_REGS: 
	  return ACCUM_REGS;

	case NO_FRAME_Y_ADDR_REGS:
	case Y_ADDR_REGS:
	  return (!reload_in_progress ? NO_REGS : class);

	case ACCUM_LOW_OR_Y_ADDR_REGS:
 	  return ACCUM_LOW_REGS;

	case ACCUM_OR_Y_ADDR_REGS: 
	  return ACCUM_REGS;

	case X_OR_Y_ADDR_REGS:
	case Y_OR_Y_ADDR_REGS: 
	case P_OR_Y_ADDR_REGS:
	  return (!reload_in_progress ? NO_REGS : class);

	case NON_HIGH_YBASE_ELIGIBLE_REGS: 
 	  return ACCUM_LOW_REGS;

	case YBASE_ELIGIBLE_REGS:
	  return ACCUM_REGS;

	case J_REG:
	case J_OR_DAU_16_BIT_REGS:
	case BMU_REGS: 
	  return (!reload_in_progress ? NO_REGS : class);

	case YBASE_VIRT_REGS:
	  if (IS_YBASE_ELIGIBLE_REG (REGNO (x)))
	    return class;
	  else
	    return (!reload_in_progress ? NO_REGS : class);

	case ACCUM_LOW_OR_YBASE_REGS:
	  if (IS_YBASE_ELIGIBLE_REG (REGNO (x)))
	    return class;
	  else
	    return ACCUM_LOW_REGS;

	case ACCUM_OR_YBASE_REGS:
	  if (IS_YBASE_ELIGIBLE_REG (REGNO (x)))
	    return class;
	  else
	    return ACCUM_REGS;

	case X_OR_YBASE_REGS:
	case Y_OR_YBASE_REGS:
	  if (IS_YBASE_ELIGIBLE_REG (REGNO (x)))
	    return YBASE_VIRT_REGS;
	  else
	    return (!reload_in_progress ? NO_REGS : class);

	case ACCUM_LOW_YL_PL_OR_YBASE_REGS:
	  if (IS_YBASE_ELIGIBLE_REG (REGNO (x)))
	    return ACCUM_LOW_OR_YBASE_REGS;
	  else
	    return ACCUM_LOW_REGS;

	case P_OR_YBASE_REGS:
	  if (IS_YBASE_ELIGIBLE_REG (REGNO (x)))
	    return YBASE_VIRT_REGS;
	  else
	    return (!reload_in_progress ? NO_REGS : class);

	case ACCUM_Y_P_OR_YBASE_REGS:
	  if (IS_YBASE_ELIGIBLE_REG (REGNO (x)))
	    return ACCUM_OR_YBASE_REGS;
	  else
	    return ACCUM_REGS;

	case Y_ADDR_OR_YBASE_REGS:
	  if (IS_YBASE_ELIGIBLE_REG (REGNO (x)))
	    return YBASE_VIRT_REGS;
	  else
	    return (!reload_in_progress ? NO_REGS : class);

	case YBASE_OR_NOHIGH_YBASE_ELIGIBLE_REGS:
	  if (IS_YBASE_ELIGIBLE_REG (REGNO (x)))
	    return ACCUM_LOW_OR_YBASE_REGS;
	  else
	    return ACCUM_LOW_REGS;

	case YBASE_OR_YBASE_ELIGIBLE_REGS:
	  if (IS_YBASE_ELIGIBLE_REG (REGNO (x)))
	    return ACCUM_OR_YBASE_REGS;
	  else
	    return ACCUM_REGS;

	case NO_HIGH_ALL_REGS:
	  if (IS_YBASE_ELIGIBLE_REG (REGNO (x)))
	    return ACCUM_LOW_OR_YBASE_REGS;
	  else
	    return ACCUM_LOW_REGS;

	case ALL_REGS: 
	  if (IS_YBASE_ELIGIBLE_REG (REGNO (x)))
	    return ACCUM_OR_YBASE_REGS;
	  else
	    return ACCUM_REGS;

	case NOHIGH_NON_ADDR_REGS:
	    return ACCUM_LOW_REGS;

	case NON_ADDR_REGS:
	case SLOW_MEM_LOAD_REGS:
	    return ACCUM_REGS;

	case NOHIGH_NON_YBASE_REGS:
	    return ACCUM_LOW_REGS;

	case NO_ACCUM_NON_YBASE_REGS:
	  return (!reload_in_progress ? NO_REGS : class);

	case NON_YBASE_REGS:
	    return ACCUM_REGS;

	default:
	  return class;
	}
    }

  /* If x (the input) is a ybase register, restrict the class of registers
     we can copy the register into.  */

  if (REG_P (x) && !TARGET_RESERVE_YBASE
      && IS_YBASE_REGISTER_WINDOW (REGNO(x)))
    {
      switch ((int) class)
	{
	case NO_REGS:
	case A0H_REG: case A0L_REG: case A0_REG: case A1H_REG:
	case ACCUM_HIGH_REGS: case A1L_REG: case ACCUM_LOW_REGS: 
	case A1_REG: case ACCUM_REGS: case X_REG: 
	case X_OR_ACCUM_LOW_REGS: case X_OR_ACCUM_REGS:
	case YH_REG: case YH_OR_ACCUM_HIGH_REGS:
	case X_OR_YH_REGS: case YL_REG:
	case YL_OR_ACCUM_LOW_REGS: case X_OR_YL_REGS:
	case X_OR_Y_REGS: case Y_REG:
	case ACCUM_OR_Y_REGS: case PH_REG:
	case X_OR_PH_REGS: case PL_REG: 
	case PL_OR_ACCUM_LOW_REGS: case X_OR_PL_REGS:
	case YL_OR_PL_OR_ACCUM_LOW_REGS: case P_REG:
	case ACCUM_OR_P_REGS: case YL_OR_P_REGS:
	case ACCUM_LOW_OR_YL_OR_P_REGS: case Y_OR_P_REGS:
	case ACCUM_Y_OR_P_REGS: case NO_FRAME_Y_ADDR_REGS:
	case Y_ADDR_REGS: case ACCUM_LOW_OR_Y_ADDR_REGS:
	case ACCUM_OR_Y_ADDR_REGS: case X_OR_Y_ADDR_REGS:
	case Y_OR_Y_ADDR_REGS: case P_OR_Y_ADDR_REGS:
	case NON_HIGH_YBASE_ELIGIBLE_REGS: case YBASE_ELIGIBLE_REGS:
	default:
	  return class;

	case J_REG:
	  return (!reload_in_progress ? NO_REGS : class);

	case J_OR_DAU_16_BIT_REGS:
	  return ACCUM_HIGH_REGS;

	case BMU_REGS: 
	case YBASE_VIRT_REGS:
	  return (!reload_in_progress ? NO_REGS : class);

	case ACCUM_LOW_OR_YBASE_REGS:
	  return ACCUM_LOW_REGS;

	case ACCUM_OR_YBASE_REGS:
	  return ACCUM_REGS;

	case X_OR_YBASE_REGS:
	  return X_REG;

	case Y_OR_YBASE_REGS:
	  return Y_REG;

	case ACCUM_LOW_YL_PL_OR_YBASE_REGS:
	  return YL_OR_PL_OR_ACCUM_LOW_REGS; 

	case P_OR_YBASE_REGS:
	  return P_REG;

	case ACCUM_Y_P_OR_YBASE_REGS:
	  return ACCUM_Y_OR_P_REGS;

	case Y_ADDR_OR_YBASE_REGS:
	  return Y_ADDR_REGS;

	case YBASE_OR_NOHIGH_YBASE_ELIGIBLE_REGS:
	  return NON_HIGH_YBASE_ELIGIBLE_REGS;

	case YBASE_OR_YBASE_ELIGIBLE_REGS:
	  return YBASE_ELIGIBLE_REGS;

	case NO_HIGH_ALL_REGS:
	  return NON_HIGH_YBASE_ELIGIBLE_REGS;

	case ALL_REGS: 
	  return YBASE_ELIGIBLE_REGS;

	case NOHIGH_NON_ADDR_REGS:
	  return ACCUM_LOW_OR_YL_OR_P_REGS;

	case NON_ADDR_REGS:
	  return ACCUM_Y_OR_P_REGS;

	case SLOW_MEM_LOAD_REGS:
	  return ACCUM_OR_Y_ADDR_REGS;

	case NOHIGH_NON_YBASE_REGS:
    	  return NON_HIGH_YBASE_ELIGIBLE_REGS;

    	case NO_ACCUM_NON_YBASE_REGS:
	  return Y_ADDR_REGS;

    	case NON_YBASE_REGS:
	  return YBASE_ELIGIBLE_REGS;
	}
    }

  if (GET_CODE (x) == PLUS)
    {
      if (GET_MODE (x) == QImode
	  && REG_P (XEXP (x,0))
	  && (XEXP (x,0) == frame_pointer_rtx
	      || XEXP (x,0) == stack_pointer_rtx)
	  && (GET_CODE (XEXP (x,1)) == CONST_INT))
	{
	  if (class == ACCUM_HIGH_REGS)
	    return class;

	  /* If the accumulators are not part of the class
	     being reloaded into, return NO_REGS.  */
#if 0
	  if (!reg_class_subset_p (ACCUM_REGS, class))
	    return (!reload_in_progress ? NO_REGS : class);
#endif
	  if (reg_class_subset_p (ACCUM_HIGH_REGS, class))
	    return ACCUM_HIGH_REGS;

	  /* We will use accumulator 'a1l' for reloading a
	     PLUS.  We can only use one accumulator because
	     'reload_inqi' only allows one alternative to be
	     used.  */

	  else if (class == ACCUM_LOW_REGS)
	    return A1L_REG;
	  else if (class == A0L_REG)
	    return NO_REGS;
	  else
	    return class;
	}

      if (class == NON_YBASE_REGS || class == YBASE_ELIGIBLE_REGS)
	return Y_ADDR_REGS;
      else
	return class;
    }
  else if (GET_CODE (x) == MEM)
    {
      /* We can't copy from a memory location into a
	 ybase register.  */
      if (reg_class_subset_p(YBASE_VIRT_REGS, class))
	{
	  switch ((int) class)
	    {
	    case YBASE_VIRT_REGS:
	      return (!reload_in_progress ? NO_REGS : class);

	    case ACCUM_LOW_OR_YBASE_REGS:
	      return ACCUM_LOW_REGS;

	    case ACCUM_OR_YBASE_REGS:
	      return ACCUM_REGS;

	    case X_OR_YBASE_REGS:
	      return X_REG;

	    case Y_OR_YBASE_REGS:
	      return Y_REG;

	    case ACCUM_LOW_YL_PL_OR_YBASE_REGS:
	      return YL_OR_PL_OR_ACCUM_LOW_REGS;

	    case P_OR_YBASE_REGS:
	      return P_REG;

	    case ACCUM_Y_P_OR_YBASE_REGS:
	      return ACCUM_Y_OR_P_REGS;

	    case Y_ADDR_OR_YBASE_REGS:
	      return Y_ADDR_REGS;

	    case YBASE_OR_NOHIGH_YBASE_ELIGIBLE_REGS:
	      return NON_HIGH_YBASE_ELIGIBLE_REGS;
	  
	    case YBASE_OR_YBASE_ELIGIBLE_REGS:
	      return YBASE_ELIGIBLE_REGS;

	    case NO_HIGH_ALL_REGS:
	      return NOHIGH_NON_YBASE_REGS;

	    case ALL_REGS:
	      return NON_YBASE_REGS;

	    default:
	      return class;
	    }
	}
      else
	return class;
    }
  else
    return class;
}
	
/* Return the register class of a scratch register needed to copy IN into
   or out of a register in CLASS in MODE.  If it can be done directly,
   NO_REGS is returned.  */

enum reg_class
secondary_reload_class (class, mode, in)
     enum reg_class class;
     enum machine_mode mode;
     rtx in;
{
  int regno = -1;

  if (GET_CODE (in) == REG || GET_CODE (in) == SUBREG)
    regno = true_regnum (in);

  /* If we are reloading a plus into a high accumulator register,
     we need a scratch low accumulator, because the low half gets
     clobbered.  */

  if (class == ACCUM_HIGH_REGS 
      || class == A1H_REG
      || class == A0H_REG)
    {
      if (GET_CODE (in) == PLUS && mode == QImode)
	return ACCUM_LOW_REGS;
    }

  if (class == ACCUM_HIGH_REGS 
      || class == ACCUM_LOW_REGS
      || class == A1L_REG
      || class == A0L_REG
      || class == A1H_REG
      || class == A0H_REG)
    {
      if (GET_CODE (in) == PLUS && mode == QImode)
	{
	  rtx addr0 = XEXP (in, 0);
	  rtx addr1 = XEXP (in, 1);
	  
	  /* If we are reloading a plus (reg:QI) (reg:QI)
	     we need an additional register.  */ 
	  if (REG_P (addr0) && REG_P (addr1))
	    return NO_REGS;
	}
    }

  /* We can place anything into ACCUM_REGS and can put ACCUM_REGS
     into anything.  */

  if ((class == ACCUM_REGS || class == ACCUM_HIGH_REGS ||
       class == ACCUM_LOW_REGS || class == A0H_REG || class == A0L_REG ||
       class == A1H_REG || class == A1_REG) || 
      (regno >= REG_A0 && regno < REG_A1L + 1))
    return NO_REGS;

  if (class == ACCUM_OR_YBASE_REGS && REG_P(in)
      && IS_YBASE_ELIGIBLE_REG(regno))
    {
      return NO_REGS;
    }

  /* We can copy the ybase registers into:
     r0-r3, a0-a1, y, p, & x or the union of
     any of these.  */

  if (!TARGET_RESERVE_YBASE && IS_YBASE_REGISTER_WINDOW(regno))
    {
      switch ((int) class)
	{
	case (int) X_REG:
	case (int) X_OR_ACCUM_LOW_REGS:
	case (int) X_OR_ACCUM_REGS:
	case (int) YH_REG:
	case (int) YH_OR_ACCUM_HIGH_REGS:
	case (int) X_OR_YH_REGS:
	case (int) YL_REG:
	case (int) YL_OR_ACCUM_LOW_REGS:
	case (int) X_OR_Y_REGS:
	case (int) X_OR_YL_REGS:
	case (int) Y_REG:
	case (int) ACCUM_OR_Y_REGS:
	case (int) PH_REG:
	case (int) X_OR_PH_REGS:
	case (int) PL_REG:
	case (int) PL_OR_ACCUM_LOW_REGS:
	case (int) X_OR_PL_REGS:
	case (int) YL_OR_PL_OR_ACCUM_LOW_REGS:
	case (int) P_REG:
	case (int) ACCUM_OR_P_REGS:
	case (int) YL_OR_P_REGS:
	case (int) ACCUM_LOW_OR_YL_OR_P_REGS:
	case (int) Y_OR_P_REGS:
	case (int) ACCUM_Y_OR_P_REGS:
	case (int) Y_ADDR_REGS:
	case (int) ACCUM_LOW_OR_Y_ADDR_REGS:
	case (int) ACCUM_OR_Y_ADDR_REGS:
	case (int) X_OR_Y_ADDR_REGS:
	case (int) Y_OR_Y_ADDR_REGS:
	case (int) P_OR_Y_ADDR_REGS:
	case (int) YBASE_ELIGIBLE_REGS:
	  return NO_REGS;

	default:
	  return ACCUM_HIGH_REGS;
	}
    }

  /* We can copy r0-r3, a0-a1, y, & p
     directly to the ybase registers. In addition
     we can use any of the ybase virtual registers
     as the secondary reload registers when copying
     between any of these registers.  */

  if (!TARGET_RESERVE_YBASE && regno != -1)
    {
      switch (regno)
	{
	case REG_A0:
	case REG_A0L:
	case REG_A1:
	case REG_A1L:
	case REG_X:
	case REG_Y:
	case REG_YL:
	case REG_PROD:
	case REG_PRODL:
	case REG_R0:
	case REG_R1:
	case REG_R2:
	case REG_R3:
	  if (class == YBASE_VIRT_REGS)
	    return NO_REGS;
	  else
	    {
	      switch ((int) class)
		{
		case (int) X_REG:
		case (int) X_OR_ACCUM_LOW_REGS:
		case (int) X_OR_ACCUM_REGS:
		case (int) YH_REG:
		case (int) YH_OR_ACCUM_HIGH_REGS:
		case (int) X_OR_YH_REGS:
		case (int) YL_REG:
		case (int) YL_OR_ACCUM_LOW_REGS:
		case (int) X_OR_Y_REGS:
		case (int) X_OR_YL_REGS:
		case (int) Y_REG:
		case (int) ACCUM_OR_Y_REGS:
		case (int) PH_REG:
		case (int) X_OR_PH_REGS:
		case (int) PL_REG:
		case (int) PL_OR_ACCUM_LOW_REGS:
		case (int) X_OR_PL_REGS:
		case (int) YL_OR_PL_OR_ACCUM_LOW_REGS:
		case (int) P_REG:
		case (int) ACCUM_OR_P_REGS:
		case (int) YL_OR_P_REGS:
		case (int) ACCUM_LOW_OR_YL_OR_P_REGS:
		case (int) Y_OR_P_REGS:
		case (int) ACCUM_Y_OR_P_REGS:
		case (int) Y_ADDR_REGS:
		case (int) ACCUM_LOW_OR_Y_ADDR_REGS:
		case (int) ACCUM_OR_Y_ADDR_REGS:
		case (int) X_OR_Y_ADDR_REGS:
		case (int) Y_OR_Y_ADDR_REGS:
		case (int) P_OR_Y_ADDR_REGS:
		case (int) YBASE_ELIGIBLE_REGS:
		  return YBASE_VIRT_REGS;

		default:
		  break;
		}
	    }
	}
    }

  /* Memory or constants can be moved from or to any register
     except the ybase virtual registers.  */
  if (regno == -1 && GET_CODE(in) != PLUS)
    {
      if (class == YBASE_VIRT_REGS)
	return NON_YBASE_REGS;
      else
        return NO_REGS;
    }

  if (GET_CODE (in) == PLUS && mode == QImode)
    {
      rtx addr0 = XEXP (in, 0);
      rtx addr1 = XEXP (in, 1);

      /* If we are reloading a plus (reg:QI) (reg:QI)
	 we need a low accumulator, not a high one.  */
      if (REG_P (addr0) && REG_P (addr1))
	return ACCUM_LOW_REGS;
    }

#if 0
  if (REG_P(in))
    return ACCUM_REGS;
#endif

  /* Otherwise, we need a high accumulator(s).  */
  return ACCUM_HIGH_REGS;
}

int
symbolic_address_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (symbolic_address_p (op));
}

int
symbolic_address_p (op)
     rtx op;
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF:
    case LABEL_REF:
      return 1;

    case CONST:
      op = XEXP (op, 0);
      return ((GET_CODE (XEXP (op, 0)) == SYMBOL_REF
	       || GET_CODE (XEXP (op, 0)) == LABEL_REF)
	      && GET_CODE (XEXP (op, 1)) == CONST_INT
              && INTVAL (XEXP (op,1)) < 0x20);

    default:
      return 0;
    }
}

/* For a Y address space operand we allow only *rn, *rn++, *rn--.
   This routine only recognizes *rn, the '<>' constraints recognize
   (*rn++), and (*rn--).  */

int
Y_address_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (memory_address_p (mode, op) && !symbolic_address_p (op));
}	     

int
sp_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
    return (GET_CODE (op) == PLUS
	    && (XEXP (op, 0) == stack_pointer_rtx
		|| XEXP (op, 0) == frame_pointer_rtx)
	    && GET_CODE (XEXP (op,1)) == CONST_INT);
}

int
sp_operand2 (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if ((GET_CODE (op) == PLUS 
       && (XEXP (op, 0) == stack_pointer_rtx
	   || XEXP (op, 0) == frame_pointer_rtx)
       && (REG_P (XEXP (op,1))
	   && IS_ADDRESS_REGISTER (REGNO (XEXP(op, 1))))))
    return 1;
  else if ((GET_CODE (op) == PLUS
       && (XEXP (op, 1) == stack_pointer_rtx
	   || XEXP (op, 1) == frame_pointer_rtx)
       && (REG_P (XEXP (op,0))
	   && IS_ADDRESS_REGISTER (REGNO (XEXP(op, 1))))))
    return 1;
  else
    return 0;
}

int
nonmemory_arith_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (immediate_operand (op, mode) || arith_reg_operand (op, mode));
}

int
arith_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  && (GET_CODE (op) != REG
	      || REGNO (op) >= FIRST_PSEUDO_REGISTER
	      || (!(IS_YBASE_REGISTER_WINDOW (REGNO (op)))
		  && REGNO (op) != FRAME_POINTER_REGNUM)));
}

int
call_address_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
    if (symbolic_address_p (op) || REG_P(op))
    {
	return 1;
    }

    return 0;
}

int
dsp16xx_comparison_operator (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  return ((mode == VOIDmode || GET_MODE (op) == mode)
	  && GET_RTX_CLASS (GET_CODE (op)) == '<'
	  && (GET_CODE(op) != GE && GET_CODE (op) != LT &&
	      GET_CODE (op) != GEU && GET_CODE (op) != LTU));
}

void
notice_update_cc(exp)
     rtx exp;
{
    if (GET_CODE (exp) == SET)
    {
	/* Jumps do not alter the cc's.  */

	if (SET_DEST (exp) == pc_rtx)
	    return;

	/* Moving register or memory into a register:
	   it doesn't alter the cc's, but it might invalidate
	   the RTX's which we remember the cc's came from.
	   (Note that moving a constant 0 or 1 MAY set the cc's).  */
	if (REG_P (SET_DEST (exp))
	    && (REG_P (SET_SRC (exp)) || GET_CODE (SET_SRC (exp)) == MEM))
	{
	    if (cc_status.value1
		&& reg_overlap_mentioned_p (SET_DEST (exp), cc_status.value1))
		cc_status.value1 = 0;
	    if (cc_status.value2
		&& reg_overlap_mentioned_p (SET_DEST (exp), cc_status.value2))
		cc_status.value2 = 0;
	    return;
	}
	/* Moving register into memory doesn't alter the cc's.
	   It may invalidate the RTX's which we remember the cc's came from.  */
	if (GET_CODE (SET_DEST (exp)) == MEM && REG_P (SET_SRC (exp)))
	{
	    if (cc_status.value1 && GET_CODE (cc_status.value1) == MEM)
		cc_status.value1 = 0;
	    if (cc_status.value2 && GET_CODE (cc_status.value2) == MEM)
		cc_status.value2 = 0;
	    return;
	}
	/* Function calls clobber the cc's.  */
	else if (GET_CODE (SET_SRC (exp)) == CALL)
	{
	    CC_STATUS_INIT;
	    return;
	}
	/* Tests and compares set the cc's in predictable ways.  */
	else if (SET_DEST (exp) == cc0_rtx)
	{
	    CC_STATUS_INIT;
	    cc_status.value1 = SET_SRC (exp);
	    return;
	}
	/* Certain instructions effect the condition codes.  */
	else if (GET_MODE_CLASS (GET_MODE (SET_SRC (exp))) == MODE_INT)
	    switch (GET_CODE (SET_SRC (exp)))
	    {
	    case PLUS: 
	    case MINUS:
	      if (REG_P (SET_DEST (exp)))
		{
		  /* Address registers don't set the condition codes.  */
		  if (IS_ADDRESS_REGISTER (REGNO (SET_DEST (exp))))
		    {
		      CC_STATUS_INIT;
		      break;
		    }
		}
	    case ASHIFTRT: 
	    case LSHIFTRT:
	    case ASHIFT: 
	    case AND: 
	    case IOR: 
	    case XOR:
	    case MULT:
	    case NEG:
	    case NOT:
	      cc_status.value1 = SET_SRC (exp);
	      cc_status.value2 = SET_DEST (exp);
	      break;
	      
	    default:
	      CC_STATUS_INIT;
	    }
	else
	{
	    CC_STATUS_INIT;
	}
    }
    else if (GET_CODE (exp) == PARALLEL
	     && GET_CODE (XVECEXP (exp, 0, 0)) == SET)
    {
	if (SET_DEST (XVECEXP (exp, 0, 0)) == pc_rtx)
	    return;

	if (SET_DEST (XVECEXP (exp, 0, 0)) == cc0_rtx)
	{
	    CC_STATUS_INIT;
	    cc_status.value1 = SET_SRC (XVECEXP (exp, 0, 0));
	    return;
	}

	CC_STATUS_INIT;
    }
    else
    {
	CC_STATUS_INIT;
    }
}

int
dsp16xx_makes_calls ()
{
  rtx insn;

  for (insn = get_insns (); insn; insn = next_insn (insn))
    if (GET_CODE (insn) == CALL_INSN)
      return (1);

  return 0;
}

long
compute_frame_size (size)
     int size;
{
  long total_size;
  long var_size;
  long args_size;
  long extra_size;
  long reg_size;

  /* This value is needed to compute reg_size.  */
  current_frame_info.function_makes_calls = !leaf_function_p ();

  reg_size = 0;
  extra_size = 0;
  var_size = size;
  args_size = current_function_outgoing_args_size;
  reg_size = reg_save_size ();  

  total_size = var_size + args_size + extra_size + reg_size;


  /* Save other computed information.  */
  current_frame_info.total_size  = total_size;
  current_frame_info.var_size    = var_size;
  current_frame_info.args_size   = args_size;
  current_frame_info.extra_size  = extra_size;
  current_frame_info.reg_size    = reg_size;
  current_frame_info.initialized = reload_completed;
  current_frame_info.reg_size	 = reg_size / UNITS_PER_WORD;

  if (reg_size)
    {
      unsigned long offset = args_size + var_size + reg_size;
      current_frame_info.sp_save_offset = offset;
      current_frame_info.fp_save_offset = offset - total_size;
    }

  return total_size;
}

int
dsp16xx_call_saved_register (regno)
     int regno;
{
#if 0
  if (regno == REG_PR && current_frame_info.function_makes_calls)
    return 1;
#endif
  return (regs_ever_live[regno] && !call_used_regs[regno] &&
	  !IS_YBASE_REGISTER_WINDOW(regno));
}

int
ybase_regs_ever_used ()
{
  int regno;
  int live = 0;

  for (regno = REG_YBASE0; regno <= REG_YBASE31; regno++)
    if (regs_ever_live[regno])
      {
	live = 1;
	break;
      }

  return live;
}

static void 
dsp16xx_output_function_prologue (file, size)
     FILE *file;
     HOST_WIDE_INT size;
{
  int regno;
  long total_size;
  fp = reg_names[FRAME_POINTER_REGNUM];
  sp = reg_names[STACK_POINTER_REGNUM];
  rr = reg_names[RETURN_ADDRESS_REGNUM];   /* return address register */
  a1h = reg_names[REG_A1];
  
  total_size = compute_frame_size (size);
  
  fprintf (file, "\t/* FUNCTION PROLOGUE: */\n");
  fprintf (file, "\t/* total=%ld, vars= %ld, regs= %d, args=%d, extra= %ld */\n",
	   current_frame_info.total_size,
	   current_frame_info.var_size,
	   current_frame_info.reg_size,
	   current_function_outgoing_args_size,
	   current_frame_info.extra_size);
  
  fprintf (file, "\t/* fp save offset= %ld, sp save_offset= %ld */\n\n",
	   current_frame_info.fp_save_offset,
	   current_frame_info.sp_save_offset);
  /* Set up the 'ybase' register window.  */
  
  if (ybase_regs_ever_used())
    {
      fprintf (file, "\t%s=%s\n", a1h, reg_names[REG_YBASE]);
      if (TARGET_YBASE_HIGH)
	fprintf (file, "\t%s=%sh-32\n", reg_names[REG_A1], a1h);
      else
	fprintf (file, "\t%s=%sh+32\n", reg_names[REG_A1], a1h);
      fprintf (file, "\t%s=%s\n", reg_names[REG_YBASE], a1h);
    }
  
  if (current_frame_info.var_size)
    {
      if (current_frame_info.var_size == 1)
	fprintf (file, "\t*%s++\n", sp);
      else
        {
	  if (SMALL_INTVAL(current_frame_info.var_size) && ((current_frame_info.var_size & 0x8000) == 0))
	    fprintf (file, "\t%s=%ld\n\t*%s++%s\n", reg_names[REG_J], current_frame_info.var_size, sp, reg_names[REG_J]);
	  else
	    fatal_error ("stack size > 32k");
	}
    }
  
  /* Save any registers this function uses, unless they are
     used in a call, in which case we don't need to.  */
  
  for(regno = 0; regno < FIRST_PSEUDO_REGISTER; ++ regno)
    if (dsp16xx_call_saved_register (regno)) 
      {
	fprintf (file, "\tpush(*%s)=%s\n", sp, reg_names[regno]);
      }

  /* For debugging purposes, we want the return address to be at a predictable
     location.  */
  if (current_frame_info.function_makes_calls)
    fprintf (file, "\tpush(*%s)=%s\n", sp, reg_names[RETURN_ADDRESS_REGNUM]);

  if (current_frame_info.args_size)
    {
      if (current_frame_info.args_size == 1)
	fprintf (file, "\t*%s++\n", sp);
      else
	error ("stack size > 32k");
    }
   
  if (frame_pointer_needed)
    {
      fprintf (file, "\t%s=%s\n", a1h, sp);
      fprintf (file, "\t%s=%s\n", fp, a1h);  /* Establish new base frame */
      fprintf (file, "\t%s=%ld\n", reg_names[REG_J], -total_size);
      fprintf (file, "\t*%s++%s\n", fp, reg_names[REG_J]);
    }
  
  fprintf (file, "\t/* END FUNCTION PROLOGUE: */\n\n");
}

void
init_emulation_routines ()
{
 dsp16xx_addhf3_libcall = (rtx) 0;
 dsp16xx_subhf3_libcall = (rtx) 0;
 dsp16xx_mulhf3_libcall = (rtx) 0;
 dsp16xx_divhf3_libcall = (rtx) 0;
 dsp16xx_cmphf3_libcall = (rtx) 0;
 dsp16xx_fixhfhi2_libcall = (rtx) 0;
 dsp16xx_floathihf2_libcall = (rtx) 0;
 dsp16xx_neghf2_libcall = (rtx) 0;

 dsp16xx_mulhi3_libcall = (rtx) 0;
 dsp16xx_udivqi3_libcall = (rtx) 0;
 dsp16xx_udivhi3_libcall = (rtx) 0;
 dsp16xx_divqi3_libcall = (rtx) 0;
 dsp16xx_divhi3_libcall = (rtx) 0;
 dsp16xx_modqi3_libcall = (rtx) 0;
 dsp16xx_modhi3_libcall = (rtx) 0;
 dsp16xx_umodqi3_libcall = (rtx) 0;
 dsp16xx_umodhi3_libcall = (rtx) 0;
 dsp16xx_ashrhi3_libcall = (rtx) 0;
 dsp16xx_ashlhi3_libcall = (rtx) 0;
 dsp16xx_ucmphi2_libcall = (rtx) 0;
 dsp16xx_lshrhi3_libcall = (rtx) 0;

}
static void
dsp16xx_output_function_epilogue (file, size)
     FILE *file;
     HOST_WIDE_INT size ATTRIBUTE_UNUSED;
{
  int regno;
  
  fp = reg_names[FRAME_POINTER_REGNUM];
  sp = reg_names[STACK_POINTER_REGNUM];
  rr = reg_names[RETURN_ADDRESS_REGNUM];   /* return address register */
  a1h = reg_names[REG_A1];
  
  fprintf (file, "\n\t/* FUNCTION EPILOGUE: */\n");
  
  if (current_frame_info.args_size)
    {
      if (current_frame_info.args_size == 1)
	fprintf (file, "\t*%s--\n", sp);
      else
	{
	  fprintf (file, "\t%s=%ld\n\t*%s++%s\n", 
		   reg_names[REG_J], -current_frame_info.args_size, sp, reg_names[REG_J]);
	}
    }
  
  if (ybase_regs_ever_used())
    {
      fprintf (file, "\t%s=%s\n", a1h, reg_names[REG_YBASE]);
      if (TARGET_YBASE_HIGH)
	fprintf (file, "\t%s=%sh+32\n", reg_names[REG_A1], a1h);
      else
	fprintf (file, "\t%s=%sh-32\n", reg_names[REG_A1], a1h);
      fprintf (file, "\t%s=%s\n", reg_names[REG_YBASE], a1h);
    }

  if (current_frame_info.function_makes_calls)
    fprintf (file, "\t%s=pop(*%s)\n", reg_names[RETURN_ADDRESS_REGNUM], sp);
  
  for (regno = FIRST_PSEUDO_REGISTER - 1; regno >= 0; --regno)
    if (dsp16xx_call_saved_register(regno))
      {
	fprintf (file, "\t%s=pop(*%s)\n", reg_names[regno], sp);
      }
  
  if (current_frame_info.var_size)
    {
      if (current_frame_info.var_size == 1)
	fprintf (file, "\t*%s--\n", sp);
      else
	{
	  fprintf (file, "\t%s=%ld\n\t*%s++%s\n", 
		   reg_names[REG_J], -current_frame_info.var_size, sp, reg_names[REG_J]);
	}
    }
  
  fprintf (file, "\treturn\n");
  /* Reset the frame info for the next function.  */
  current_frame_info = zero_frame_info;
  init_emulation_routines ();
}

/* Emit insns to move operands[1] into operands[0].

   Return 1 if we have written out everything that needs to be done to
   do the move.  Otherwise, return 0 and the caller will emit the move
   normally.  */

int
emit_move_sequence (operands, mode)
     rtx *operands;
     enum machine_mode mode;
{
  register rtx operand0 = operands[0];
  register rtx operand1 = operands[1];

  /* We can only store registers to memory.  */

  if (GET_CODE (operand0) == MEM && GET_CODE (operand1) != REG)
    operands[1] = force_reg (mode, operand1);

  return 0;
}

void
double_reg_from_memory (operands)
     rtx operands[];
{
    rtx xoperands[4];

    if (GET_CODE(XEXP(operands[1],0)) == POST_INC)
    {
	output_asm_insn ("%u0=%1", operands);
	output_asm_insn ("%w0=%1", operands);
    }
    else if (GET_CODE(XEXP(operands[1],0)) == POST_DEC)
    {
	xoperands[1] = XEXP (XEXP (operands[1], 0), 0);
	xoperands[0] = operands[0];
	
	/* We can't use j anymore since the compiler can allocate it.  */
/*	output_asm_insn ("j=-3\n\t%u0=*%1++\n\t%w0=*%1++j", xoperands); */
	output_asm_insn ("%u0=*%1++\n\t%w0=*%1--\n\t*%1--\n\t*%1--", xoperands);
    }
    else if (GET_CODE(XEXP(operands[1],0)) == PLUS)
    {
      rtx addr;
      int offset = 0;

      output_asm_insn ("%u0=%1", operands);


      /* In order to print out the least significant word we must
	 use 'offset + 1'.  */
      addr = XEXP (operands[1], 0);
      if (GET_CODE (XEXP(addr,0)) == CONST_INT)
	offset = INTVAL(XEXP(addr,0)) + 1;
      else if (GET_CODE (XEXP(addr,1)) == CONST_INT)
	offset = INTVAL(XEXP(addr,1)) + 1;

      fprintf (asm_out_file, "\t%s=*(%d)\n", reg_names[REGNO(operands[0]) + 1], offset + 31);
    }
    else
    {
	xoperands[1] = XEXP(operands[1],0);
	xoperands[0] = operands[0];

	output_asm_insn ("%u0=*%1++\n\t%w0=*%1--", xoperands);
    }
}


void
double_reg_to_memory (operands)
     rtx operands[];
{
    rtx xoperands[4];

    if (GET_CODE(XEXP(operands[0],0)) == POST_INC)
    {
	output_asm_insn ("%0=%u1", operands);
	output_asm_insn ("%0=%w1", operands);
    }
    else if (GET_CODE(XEXP(operands[0],0)) == POST_DEC)
    {
	xoperands[0] = XEXP (XEXP (operands[0], 0), 0);
	xoperands[1] = operands[1];
	
	/* We can't use j anymore since the compiler can allocate it.  */

/*	output_asm_insn ("j=-3\n\t*%0++=%u1\n\t*%0++j=%w1", xoperands); */
	output_asm_insn ("*%0++=%u1\n\t*%0--=%w1\n\t*%0--\n\t*%0--", xoperands);

    }
    else if (GET_CODE(XEXP(operands[0],0)) == PLUS)
    {
      rtx addr;
      int offset = 0;

      output_asm_insn ("%0=%u1", operands);

      /* In order to print out the least significant word we must
	 use 'offset + 1'.  */
      addr = XEXP (operands[0], 0);
      if (GET_CODE (XEXP(addr,0)) == CONST_INT)
	offset = INTVAL(XEXP(addr,0)) + 1;
      else if (GET_CODE (XEXP(addr,1)) == CONST_INT)
	offset = INTVAL(XEXP(addr,1)) + 1;
      else
	fatal_error ("invalid addressing mode");

      fprintf (asm_out_file, "\t*(%d)=%s\n", offset + 31, reg_names[REGNO(operands[1]) + 1]);
    }
    else
    {
	xoperands[0] = XEXP(operands[0],0);
	xoperands[1] = operands[1];

	output_asm_insn ("*%0++=%u1\n\t*%0--=%w1", xoperands);
    }
}

void
override_options ()
{
  char *tmp;

  if (chip_name == (char *) 0)
    chip_name = DEFAULT_CHIP_NAME;

  if (text_seg_name == (char *) 0)
    text_seg_name = DEFAULT_TEXT_SEG_NAME;
  
  if (data_seg_name == (char *) 0)
    data_seg_name = DEFAULT_DATA_SEG_NAME;
  
  if (bss_seg_name == (char *) 0)
    bss_seg_name = DEFAULT_BSS_SEG_NAME;
  
  if (const_seg_name == (char *) 0)
    const_seg_name = DEFAULT_CONST_SEG_NAME;
  
  save_chip_name = xstrdup (chip_name);

  rsect_text = tmp = (char *) xmalloc (strlen(".rsect ") + 
				       strlen(text_seg_name) + 3);
  sprintf (tmp, ".rsect \"%s\"", text_seg_name);

  rsect_data = tmp = (char *) xmalloc (strlen(".rsect ") + 
				       strlen(data_seg_name) + 3);
  sprintf (tmp, ".rsect \"%s\"", data_seg_name);

  rsect_bss = tmp = (char *) xmalloc (strlen(".rsect ") + 
				      strlen(bss_seg_name) + 3);
  sprintf (tmp,  ".rsect \"%s\"", bss_seg_name);

  rsect_const = tmp = (char *) xmalloc (strlen(".rsect ") + 
					strlen(const_seg_name) + 3);
  sprintf (tmp, ".rsect \"%s\"", const_seg_name);
  
  /* Mark our global variables for GC.  */
  ggc_add_rtx_root (&dsp16xx_addhf3_libcall, 1);
  ggc_add_rtx_root (&dsp16xx_subhf3_libcall, 1);
  ggc_add_rtx_root (&dsp16xx_mulhf3_libcall, 1);
  ggc_add_rtx_root (&dsp16xx_divhf3_libcall, 1);
  ggc_add_rtx_root (&dsp16xx_cmphf3_libcall, 1);
  ggc_add_rtx_root (&dsp16xx_fixhfhi2_libcall, 1);
  ggc_add_rtx_root (&dsp16xx_floathihf2_libcall, 1);
  ggc_add_rtx_root (&dsp16xx_neghf2_libcall, 1);
  ggc_add_rtx_root (&dsp16xx_mulhi3_libcall, 1);
  ggc_add_rtx_root (&dsp16xx_udivqi3_libcall, 1);
  ggc_add_rtx_root (&dsp16xx_udivhi3_libcall, 1);
  ggc_add_rtx_root (&dsp16xx_divqi3_libcall, 1);
  ggc_add_rtx_root (&dsp16xx_divhi3_libcall, 1);
  ggc_add_rtx_root (&dsp16xx_modqi3_libcall, 1);
  ggc_add_rtx_root (&dsp16xx_modhi3_libcall, 1);
  ggc_add_rtx_root (&dsp16xx_umodqi3_libcall, 1);
  ggc_add_rtx_root (&dsp16xx_umodhi3_libcall, 1);
  ggc_add_rtx_root (&dsp16xx_ashrhi3_libcall, 1);
  ggc_add_rtx_root (&dsp16xx_ashlhi3_libcall, 1);
  ggc_add_rtx_root (&dsp16xx_ucmphi2_libcall, 1);
  ggc_add_rtx_root (&dsp16xx_lshrhi3_libcall, 1);
}

int
next_cc_user_unsigned (insn)
     rtx insn;
{
  switch (next_cc_user_code (insn))
    {
    case GTU:
    case GEU:
    case LTU:
    case LEU:
      return 1;
    default:
      return 0;
    }
}

enum rtx_code
next_cc_user_code (insn)
     rtx insn;
{
  /* If no insn could be found we assume that the jump has been
     deleted and the compare will be deleted later.  */

  if (!(insn = next_cc0_user (insn)))
    return (enum rtx_code) 0;
  else if (GET_CODE (insn) == JUMP_INSN
	   && GET_CODE (PATTERN (insn)) == SET
	   && GET_CODE (SET_SRC (PATTERN (insn))) == IF_THEN_ELSE)
    return GET_CODE (XEXP (SET_SRC (PATTERN (insn)), 0));
  else if (GET_CODE (insn) == INSN
	   && GET_CODE (PATTERN (insn)) == SET
	   && comparison_operator (SET_SRC (PATTERN (insn)), VOIDmode))
    return GET_CODE (SET_SRC (PATTERN (insn)));
  else
    abort ();
}

void
print_operand(file, op, letter)
     FILE *file;
     rtx op;
     int letter;
{
    enum rtx_code code;

    code = GET_CODE(op);

    switch (letter)
    {
       case 'I':
	  code = reverse_condition (code);
	  /* Fallthrough */

       case 'C':
          if (code == EQ) 
          { 
	      fputs ("eq", file); 
	      return; 
	  }
          else if (code == NE)  
	  { 
	      fputs ("ne", file); 
	      return; 
	  }
          else if (code == GT || code == GTU)  
	  { 
	      fputs ("gt", file); 
	      return; 
	  }
          else if (code == LT || code == LTU)  
	  { 
	      fputs ("mi", file); 
	      return; 
	  }
          else if (code == GE || code == GEU)  
	  {
	      fputs ("pl", file); 
	      return; 
	  }
          else if (code == LE || code == LEU)  
	  { 
	      fputs ("le", file); 
	      return; 
	  }
          else 
	      abort ();
	  break;

       default:
          break;  
    }

    if (code == REG)
    {
	/* Print the low half of a 32-bit register pair.  */
        if (letter == 'w')
           fprintf (file, "%s", reg_names[REGNO (op) + 1]);
        else if (letter == 'u' || !letter)
           fprintf (file, "%s", reg_names[REGNO (op)]);
	else if (letter == 'b')
	    fprintf (file, "%sh", reg_names[REGNO (op)]);
	else if (letter == 'm')
	  fprintf (file, "%s", himode_reg_name[REGNO (op)]);
        else
	  output_operand_lossage ("bad register extension code");
    }
    else if (code == MEM)
      output_address (XEXP(op,0));
    else if (code == CONST_INT)
      { 
	HOST_WIDE_INT val = INTVAL (op);

        if (letter == 'H')
	  fprintf (file, HOST_WIDE_INT_PRINT_HEX, val & 0xffff);
	else if (letter == 'h')
	  fprintf (file, HOST_WIDE_INT_PRINT_DEC, val);
        else if (letter == 'U')
	  fprintf (file, HOST_WIDE_INT_PRINT_HEX, (val >> 16) & 0xffff);
        else
           output_addr_const(file, op);
    }
    else if (code == CONST_DOUBLE && GET_MODE(op) != DImode)
    {
	  union { double d; int i[2]; } u;
	  union { float f; int i; } u1;
	  u.i[0] = CONST_DOUBLE_LOW (op);
	  u.i[1] = CONST_DOUBLE_HIGH (op);
	  u1.f = u.d;
          fprintf (file, "0x%x", u1.i);
    }
    else if (code == CONST)
      {
	rtx addr = XEXP (op, 0);
	
	if (GET_CODE (addr) != PLUS)
	  {
	    output_addr_const(file, op);
	    return;
	  }
	
	if ((GET_CODE (XEXP (addr, 0)) == SYMBOL_REF
	     || GET_CODE (XEXP (addr, 0)) == LABEL_REF)
	    && (GET_CODE (XEXP (addr, 1)) == CONST_INT))
	  {
	    int n = INTVAL (XEXP(addr, 1));
	    output_addr_const (file, XEXP (addr, 0));
	    
	    if (n >= 0)
	      fprintf (file, "+");
	    
	    n = (int) (short) n;
	    fprintf (file, "%d", n);
	  }
	else if ((GET_CODE (XEXP (addr, 1)) == SYMBOL_REF
		  || GET_CODE (XEXP (addr, 1)) == LABEL_REF)
		 && (GET_CODE (XEXP (addr, 0)) == CONST_INT))
	  {
	    int n = INTVAL (XEXP(addr, 0));
	    output_addr_const (file, XEXP (addr, 1));
	    
	    if (n >= 0)
	      fprintf (file, "+");
	    
	    n = (int) (short) n;
	    fprintf (file, "%d", n);
	  }
	else
	  output_addr_const(file, op);
      }
    else
      output_addr_const (file, op);
}


void
print_operand_address(file, addr)
     FILE *file;
     rtx addr;
{
  rtx base;
  int offset = 0;;
  
  switch (GET_CODE (addr))
    {
    case REG:
      fprintf (file, "*%s", reg_names[REGNO (addr)]);
      break;
    case POST_DEC:
      fprintf (file, "*%s--", reg_names[REGNO (XEXP (addr, 0))]);
      break;
    case POST_INC:
      fprintf (file, "*%s++", reg_names[REGNO (XEXP (addr, 0))]);
      break;
    case PLUS:
      if (GET_CODE (XEXP(addr,0)) == CONST_INT)
	offset = INTVAL(XEXP(addr,0)), base = XEXP(addr,1);
      else if (GET_CODE (XEXP(addr,1)) == CONST_INT)
	offset = INTVAL(XEXP(addr,1)), base = XEXP(addr,0);
      else
	abort();
      if (GET_CODE (base) == REG && REGNO(base) == STACK_POINTER_REGNUM)
	{
	  if (offset >= -31 && offset <= 0)
	    offset = 31 + offset;
	  else
	    fatal_error ("invalid offset in ybase addressing");
	}
      else
	fatal_error ("invalid register in ybase addressing");
      
      fprintf (file, "*(%d)", offset);
      break;
      
    default:
      if (FITS_5_BITS (addr))
	fprintf (file, "*(0x%x)", (INTVAL (addr) & 0x20));
      else
	output_addr_const (file, addr);
    }
}

void
output_dsp16xx_float_const (operands)
     rtx *operands;
{
  rtx src = operands[1];
  
#if HOST_FLOAT_FORMAT == TARGET_FLOAT_FORMAT
  REAL_VALUE_TYPE d;
  long value;
  
  REAL_VALUE_FROM_CONST_DOUBLE (d, src);
  REAL_VALUE_TO_TARGET_SINGLE (d, value);
  
  operands[1] = GEN_INT (value);
  output_asm_insn ("%u0=%U1\n\t%w0=%H1", operands);
#else
  fatal_error ("inline float constants not supported on this host");
#endif
}

static int
reg_save_size ()
{
  int reg_save_size = 0;
  int regno;

  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (dsp16xx_call_saved_register (regno))
      {
	reg_save_size += UNITS_PER_WORD;
      }

  /* If the function makes calls we will save need to save the 'pr' register.  */
  if (current_frame_info.function_makes_calls)
    reg_save_size += 1;

  return (reg_save_size);
}

#if 0
int
dsp16xx_starting_frame_offset()
{
  int reg_save_size = 0;
 int regno;
 
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (dsp16xx_call_saved_register (regno))
      {
	reg_save_size += UNITS_PER_WORD;
      }

  return (reg_save_size);
}
#endif

int
initial_frame_pointer_offset()
{
  int offset = 0;
  
  offset = compute_frame_size (get_frame_size());

#ifdef STACK_GROWS_DOWNWARD
  return (offset);
#else
  return (-offset);
#endif
}

/* Generate the minimum number of 1600 core shift instructions
   to shift by 'shift_amount'.  */

#if 0
void
emit_1600_core_shift (shift_op, operands, shift_amount, mode)
     enum rtx_code shift_op;
     rtx *operands;
     int shift_amount;
     enum machine_mode mode;
{
  int quotient;
  int i;
  int first_shift_emitted = 0;
  
  while (shift_amount != 0)
    {
      if (shift_amount/16)
	{
	  quotient = shift_amount/16;
	  shift_amount = shift_amount - (quotient * 16);
	  for (i = 0; i < quotient; i++)
	    emit_insn (gen_rtx_SET (VOIDmode, operands[0],
				    gen_rtx (shift_op, mode, 
					     first_shift_emitted
					     ? operands[0] : operands[1],
					     GEN_INT (16))));
	  first_shift_emitted = 1;
	}
      else if (shift_amount/8)
	{
	  quotient = shift_amount/8;
	  shift_amount = shift_amount - (quotient * 8);
	  for (i = 0; i < quotient; i++)
	    emit_insn (gen_rtx_SET (VOIDmode, operands[0],
				    gen_rtx (shift_op, mode, 
					     first_shift_emitted
					     ? operands[0] : operands[1],
					     GEN_INT (8))));
	  first_shift_emitted = 1;
	}
      else if (shift_amount/4)
	{
	  quotient = shift_amount/4;
	  shift_amount = shift_amount - (quotient * 4);
	  for (i = 0; i < quotient; i++)
	    emit_insn (gen_rtx_SET (VOIDmode, operands[0],
				    gen_rtx (shift_op, mode, 
					     first_shift_emitted
					     ? operands[0] : operands[1],
					     GEN_INT (4))));
	  first_shift_emitted = 1;
	}
      else if (shift_amount/1)
	{
	  quotient = shift_amount/1;
	  shift_amount = shift_amount - (quotient * 1);
	  for (i = 0; i < quotient; i++)
	    emit_insn (gen_rtx_SET (VOIDmode, operands[0],
				    gen_rtx (shift_op, mode, 
					     first_shift_emitted
					     ? operands[0] : operands[1],
					     GEN_INT (1))));
	  first_shift_emitted = 1;
	}
    }
}
#else
void
emit_1600_core_shift (shift_op, operands, shift_amount)
     enum rtx_code shift_op;
     rtx *operands;
     int shift_amount;
{
  int quotient;
  int i;
  int first_shift_emitted = 0;
  const char * const *shift_asm_ptr;
  const char * const *shift_asm_ptr_first;

  if (shift_op == ASHIFT)
    {
      shift_asm_ptr = ashift_left_asm;
      shift_asm_ptr_first = ashift_left_asm_first;
    }
  else if (shift_op == ASHIFTRT)
    {
      shift_asm_ptr = ashift_right_asm;
      shift_asm_ptr_first = ashift_right_asm_first;
    }
  else if (shift_op == LSHIFTRT)
    {
      shift_asm_ptr = lshift_right_asm;
      shift_asm_ptr_first = lshift_right_asm_first;
    }
  else
    fatal_error ("invalid shift operator in emit_1600_core_shift");

  while (shift_amount != 0)
    {
      if (shift_amount/16)
	{
	  quotient = shift_amount/16;
	  shift_amount = shift_amount - (quotient * 16);
	  for (i = 0; i < quotient; i++)
	    output_asm_insn ((first_shift_emitted ? shift_asm_ptr[SHIFT_INDEX_16]
			      : shift_asm_ptr_first[SHIFT_INDEX_16]), operands);
	  first_shift_emitted = 1;
	}
      else if (shift_amount/8)
	{
	  quotient = shift_amount/8;
	  shift_amount = shift_amount - (quotient * 8);
	  for (i = 0; i < quotient; i++)
	    output_asm_insn ((first_shift_emitted ? shift_asm_ptr[SHIFT_INDEX_8]
			      : shift_asm_ptr_first[SHIFT_INDEX_8]), operands);
	  first_shift_emitted = 1;
	}
      else if (shift_amount/4)
	{
	  quotient = shift_amount/4;
	  shift_amount = shift_amount - (quotient * 4);
	  for (i = 0; i < quotient; i++)
	    output_asm_insn ((first_shift_emitted ? shift_asm_ptr[SHIFT_INDEX_4]
			      : shift_asm_ptr_first[SHIFT_INDEX_4]), operands);
	  first_shift_emitted = 1;
	}
      else if (shift_amount/1)
	{
	  quotient = shift_amount/1;
	  shift_amount = shift_amount - (quotient * 1);
	  for (i = 0; i < quotient; i++)
	    output_asm_insn ((first_shift_emitted ? shift_asm_ptr[SHIFT_INDEX_1]
			      : shift_asm_ptr_first[SHIFT_INDEX_1]), operands);
	  first_shift_emitted = 1;
	}
    }
}
#endif

int
num_1600_core_shifts (shift_amount)
int shift_amount;
{
  int quotient;
  int i;
  int first_shift_emitted = 0;
  int num_shifts = 0;

  while (shift_amount != 0)
    {
      if (shift_amount/16)
	{
	  quotient = shift_amount/16;
	  shift_amount = shift_amount - (quotient * 16);
	  for (i = 0; i < quotient; i++)
	    num_shifts++;
	  first_shift_emitted = 1;
	}
      else if (shift_amount/8)
	{
	  quotient = shift_amount/8;
	  shift_amount = shift_amount - (quotient * 8);
	  for (i = 0; i < quotient; i++)
	    num_shifts++;

	  first_shift_emitted = 1;
	}
      else if (shift_amount/4)
	{
	  quotient = shift_amount/4;
	  shift_amount = shift_amount - (quotient * 4);
	  for (i = 0; i < quotient; i++)
	    num_shifts++;

	  first_shift_emitted = 1;
	}
      else if (shift_amount/1)
	{
	  quotient = shift_amount/1;
	  shift_amount = shift_amount - (quotient * 1);
	  for (i = 0; i < quotient; i++)
	    num_shifts++;

	  first_shift_emitted = 1;
	}
    }
  return num_shifts;
}

void
asm_output_common(file, name, size, rounded)
     FILE *file;
     const char *name;
     int size ATTRIBUTE_UNUSED;
     int rounded;
{
    bss_section ();
    ASM_GLOBALIZE_LABEL (file, name);
    assemble_name (file, name);
    fputs (":", file);
    if (rounded > 1)
	fprintf (file, "%d * int\n", rounded);
    else
	fprintf (file, "int\n");
}

void
asm_output_local(file, name, size, rounded)
     FILE *file;
     const char *name;
     int size ATTRIBUTE_UNUSED;
     int rounded;
{
    bss_section ();
    assemble_name (file, name);
    fputs (":", file);
    if (rounded > 1)
	fprintf (file, "%d * int\n", rounded);
    else
	fprintf (file, "int\n");
}

int
dsp16xx_address_cost (addr)
     rtx addr;
{
    switch (GET_CODE (addr))
    {
	  default:
	     break;

	  case REG:
	     return 1;

	  case CONST:
	     {
	        rtx offset = const0_rtx;
	        addr = eliminate_constant_term (addr, &offset);

	        if (GET_CODE (addr) == LABEL_REF)
	            return 2;

	        if (GET_CODE (addr) != SYMBOL_REF)
	            return 4;

	        if (INTVAL (offset) == 0)
	            return 2;
             }
	     /* fall through */

	  case POST_INC: case POST_DEC:
	     return (GET_MODE (addr) == QImode ? 1 : 2);

	  case SYMBOL_REF: case LABEL_REF:
	     return 2;

	  case PLUS:
	  {
	     register rtx plus0 = XEXP (addr, 0);
	     register rtx plus1 = XEXP (addr, 1);
	     
	     if (GET_CODE (plus0) != REG && GET_CODE (plus1) == REG)
	     {
		 plus0 = XEXP (addr, 1);
		 plus1 = XEXP (addr, 0);
	     }
	     
	     if (GET_CODE (plus0) != REG)
		 break;
	     
	     switch (GET_CODE (plus1))
	     {
		   default:
		      break;
		 
		   case CONST_INT:
		      return 4;

		   case CONST:
		   case SYMBOL_REF:
		   case LABEL_REF:
		      return dsp16xx_address_cost (plus1) + 1;
	     }
	  }
     }
	     
     return 4;
}


/* Determine whether a function argument is passed in a register, and
   which register.

   The arguments are CUM, which summarizes all the previous
   arguments; MODE, the machine mode of the argument; TYPE,
   the data type of the argument as a tree node or 0 if that is not known
   (which happens for C support library functions); and NAMED,
   which is 1 for an ordinary argument and 0 for nameless arguments that
   correspond to `...' in the called function's prototype.

   The value of the expression should either be a `reg' RTX for the
   hard register in which to pass the argument, or zero to pass the
   argument on the stack.

   On the dsp1610 the first four words of args are normally in registers
   and the rest are pushed. If we a long or on float mode, the argument
   must begin on an even register boundary

   Note that FUNCTION_ARG and FUNCTION_INCOMING_ARG were different.
   For structures that are passed in memory, but could have been
   passed in registers, we first load the structure into the
   register, and then when the last argument is passed, we store
   the registers into the stack locations.  This fixes some bugs
   where GCC did not expect to have register arguments, followed.  */

struct rtx_def *
dsp16xx_function_arg (args_so_far, mode, type, named)
     CUMULATIVE_ARGS args_so_far;
     enum machine_mode mode;
     tree type;
     int named;
{
  if (TARGET_REGPARM)
    {
      if ((args_so_far & 1) != 0
	  && (mode == HImode || GET_MODE_CLASS(mode) == MODE_FLOAT))
	args_so_far++;

      if (type == void_type_node)
	return (struct rtx_def *) 0;

      if (named && args_so_far < 4 && !MUST_PASS_IN_STACK (mode,type))
	return gen_rtx_REG (mode, args_so_far + FIRST_REG_FOR_FUNCTION_ARG);
      else
	return (struct rtx_def *) 0;
    }
  else
    return (struct rtx_def *) 0;
}

/* Advance the argument to the next argument position.  */

void
dsp16xx_function_arg_advance (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;	/* current arg information */
     enum machine_mode mode;	/* current arg mode */
     tree type;			/* type of the argument or 0 if lib support */
     int named ATTRIBUTE_UNUSED;/* whether or not the argument was named */
{
  if (TARGET_REGPARM)
    {
      if ((*cum & 1) != 0
	  && (mode == HImode || GET_MODE_CLASS(mode) == MODE_FLOAT))
	*cum += 1;

      if (mode != BLKmode)
	*cum += GET_MODE_SIZE (mode);
      else
	*cum += int_size_in_bytes (type);
    }
}

void
coff_dsp16xx_file_start (file)
     FILE *file;
{
  fprintf (file, "#include <%s.h>\n", save_chip_name);
}

void
luxworks_dsp16xx_file_start (file)
     FILE *file;
{
  char *temp_filename;
  int len, err_code;


  fprintf (file, "\t.debug ");
  err_code = (TARGET_DEBUG) ? fprintf (file, "yes, ") : fprintf (file, "no, ");
  err_code = (TARGET_SAVE_TEMPS) ? fprintf (file, "asm, ") : fprintf (file, "temp, ");
  len = strlen (main_input_filename);
  temp_filename = (char *) xmalloc (len + 2);
  strcpy (temp_filename, main_input_filename);
#ifdef __CYGWIN32__
    p = temp_filename;
    while (*p != '\0') {
    if (*p == '\\')
        *p = '/';
         p++;
         }
#endif
    fprintf (file, "\"%s\"\n", temp_filename);

  fprintf (file, "#include <%s.h>\n", save_chip_name);

   /*
    * Add dummy sections, so that they always exist in the 
    * object code. These have been created so that the number and
    * type of sections remain consistent with and without -g option. Note
    * that the .data, .text, .const and .bss are always created when -g
    * is provided as an option.  */
   fprintf (file, "\t.rsect \".text\" , nodelete\n");
   fprintf (file, "\t.rsect \".data\" , nodelete\n");
   fprintf (file, "\t.rsect \".const\" , nodelete\n");
   fprintf (file, "\t.rsect \".bss\" , nodelete\n");
}

rtx
gen_tst_reg (x)
     rtx x;
{
  enum machine_mode mode;

  mode = GET_MODE (x);

  if (mode == QImode)
    emit_insn (gen_rtx_PARALLEL
	       (VOIDmode,
		gen_rtvec (2, gen_rtx_SET (VOIDmode, cc0_rtx, x),
			   gen_rtx_CLOBBER (VOIDmode,
					    gen_rtx_SCRATCH (QImode)))));
  else if (mode == HImode)
    emit_insn (gen_rtx_SET (VOIDmode, cc0_rtx, x));
  else
    fatal_error ("invalid mode for gen_tst_reg");

  return cc0_rtx;
}

rtx
gen_compare_reg (code, x, y)
     enum rtx_code code;
     rtx x, y;
{
  enum machine_mode mode;

  mode = GET_MODE (x);
  /* For floating point compare insns, a call is generated so don't
     do anything here.  */

  if (GET_MODE_CLASS (mode) == MODE_FLOAT)
    return cc0_rtx;

  if (mode == QImode)
    {
      if (code == GTU || code == GEU
	  || code == LTU || code == LEU)
	{
	  emit_insn (gen_rtx_PARALLEL
		     (VOIDmode,
		      gen_rtvec (3,
				 gen_rtx_SET (VOIDmode, cc0_rtx,
					      gen_rtx_COMPARE (mode, x, y)),
				 gen_rtx_CLOBBER (VOIDmode,
						  gen_rtx_SCRATCH (QImode)),
				 gen_rtx_CLOBBER (VOIDmode,
						  gen_rtx_SCRATCH (QImode)))));
	}
      else
	{
	  emit_insn (gen_rtx_PARALLEL
		     (VOIDmode,
		      gen_rtvec (3, gen_rtx_SET (VOIDmode, cc0_rtx,
						 gen_rtx_COMPARE (mode, x, y)),
				 gen_rtx_CLOBBER (VOIDmode,
						  gen_rtx_SCRATCH (QImode)),
				 gen_rtx_CLOBBER (VOIDmode,
						  gen_rtx_SCRATCH (QImode)))));
	}
    }
  else if (mode == HImode)
    {
      if (code == GTU || code == GEU
	  || code == LTU || code == LEU)
	{
	  emit_insn (gen_rtx_PARALLEL 
		     (VOIDmode, 
		      gen_rtvec (5,
				 gen_rtx_SET (VOIDmode, cc0_rtx, 
					      gen_rtx_COMPARE (VOIDmode, x, y)),
				 gen_rtx_CLOBBER (VOIDmode, 
						  gen_rtx_SCRATCH (QImode)),
				 gen_rtx_CLOBBER (VOIDmode, 
						  gen_rtx_SCRATCH (QImode)),
				 gen_rtx_CLOBBER (VOIDmode, 
						  gen_rtx_SCRATCH (QImode)),
				 gen_rtx_CLOBBER (VOIDmode, 
						  gen_rtx_SCRATCH (QImode)))));
	}
      else
	emit_insn (gen_rtx_SET (VOIDmode, cc0_rtx,
				gen_rtx_COMPARE (VOIDmode,
						 force_reg (HImode, x), 
						 force_reg (HImode,y))));
    }
  else
    fatal_error ("invalid mode for integer comparison in gen_compare_reg");

  return cc0_rtx;
}

const char *
output_block_move (operands)
     rtx operands[];
{
  int loop_count = INTVAL(operands[2]);
  rtx xoperands[4];

  fprintf (asm_out_file, "\tdo %d {\n", loop_count);
  xoperands[0] = operands[4];
  xoperands[1] = operands[1];
  output_asm_insn ("%0=*%1++", xoperands);

  xoperands[0] = operands[0];
  xoperands[1] = operands[4];
  output_asm_insn ("*%0++=%1", xoperands);

  fprintf (asm_out_file, "\t}\n");
  return "";
}

int
uns_comparison_operator (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (mode == VOIDmode || GET_MODE (op) == mode)
    {
      enum rtx_code code;
      
      code = GET_CODE(op);

      if (code == LEU || code == LTU || code == GEU
	  || code == GTU)
	{
	  return 1;
	}
      else
	return 0;
    }

  return 0;
}

int
signed_comparison_operator (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (mode == VOIDmode || GET_MODE (op) == mode)
    {
      enum rtx_code code;
      
      code = GET_CODE(op);

      if (!(code == LEU || code == LTU || code == GEU
	  || code == GTU))
	{
	  return 1;
	}
      else
	return 0;
    }

  return 0;
}
