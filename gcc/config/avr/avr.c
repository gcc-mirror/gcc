/* Subroutines for insn-output.c for ATMEL AVR micro controllers
   Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.
   Contributed by Denis Chertykov (denisc@overta.ru)

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
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "reload.h"
#include "tree.h"
#include "expr.h"
#include "toplev.h"
#include "obstack.h"
#include "function.h"
#include "recog.h"
#include "tm_p.h"


static int    avr_naked_function_p PARAMS ((tree));
static int    interrupt_function_p PARAMS ((tree));
static int    signal_function_p    PARAMS ((tree));
static int    sequent_regs_live    PARAMS ((void));
static char * ptrreg_to_str        PARAMS ((int));
static char * cond_string          PARAMS ((enum rtx_code));


/* Allocate registers from r25 to r8 for parameters for function calls */
#define FIRST_CUM_REG 26

/* Temporary register RTX (gen_rtx (REG,QImode,TMP_REGNO)) */
rtx tmp_reg_rtx;

/* Zeroed register RTX (gen_rtx (REG,QImode,ZERO_REGNO)) */
rtx zero_reg_rtx;

/* AVR register names {"r0", "r1", ..., "r31"} */
char * avr_regnames[] = REGISTER_NAMES;

/* This holds the last insn address.  */
static int last_insn_address = 0;

/* Commands count in the compiled file */
static int commands_in_file;

/* Commands in the functions prologues in the compiled file */
static int commands_in_prologues;

/* Commands in the functions epilogues in the compiled file */
static int commands_in_epilogues;

/* Prologue/Epilogue size in words */
static int prologue_size;
static int epilogue_size;

/* Initial stack value specified by the `-minit-stack=' option */
const char *avr_ram_end = NULL;

/* Numeric representation */
static const char *initial_stack;

/* Default MCU name */
const char *avr_mcu_name = "at90s8515";

/* Default MCU */
struct mcu_type_s *avr_mcu_type;

/* MCU names, initial stack value, flag 'mega' */
static struct mcu_type_s mcu_types[] =
{{"at90s2313", 224-1, 0},
 {"at90s2323", 224-1, 0},
 {"at90s2333", 224-1, 0},
 {"attiny22",  224-1, 0},
 {"at90s2343", 224-1, 0},
 {"at90s4433", 224-1, 0},
 {"at90s4414", 0x15f, 0},
 {"at90s4434", 0x15f, 0},
 {"at90s8515", 0x25f, 0},
 {"at90s8535", 0x25f, 0},
 {"atmega603", 0x0fff,1},
 {"atmega103", 0x0fff,1},
 {NULL,0,0}};

/* Setup MCU */

void
avr_override_options (void)
{
  for (avr_mcu_type = mcu_types; avr_mcu_type->name; ++avr_mcu_type)
    if (strcmp (avr_mcu_type->name, avr_mcu_name) == 0)
      break;
  if (!avr_mcu_type->name)
    {
      int i;
      fprintf (stderr,
	       "Wrong mcu `%s' specified\n"
	       "Allowed mcu's:\n", avr_mcu_name);
      for (i = 0; mcu_types[i].name; ++i)
	fprintf (stderr,"   %s\n", mcu_types[i].name);
      fatal ("select right mcu name");
    }
}

/* Initialize TMP_REG_RTX and ZERO_REG_RTX */
void
avr_init_once (void)
{
  tmp_reg_rtx = xmalloc (sizeof (struct rtx_def) + 1 * sizeof (rtunion));
  memset (tmp_reg_rtx, 0, sizeof (struct rtx_def) + 1 * sizeof (rtunion));
  PUT_CODE (tmp_reg_rtx, REG);
  PUT_MODE (tmp_reg_rtx, QImode);
  XINT (tmp_reg_rtx, 0) = TMP_REGNO;

  zero_reg_rtx = xmalloc (sizeof (struct rtx_def) + 1 * sizeof (rtunion));
  memset (zero_reg_rtx, 0, sizeof (struct rtx_def) + 1 * sizeof (rtunion));
  PUT_CODE (zero_reg_rtx, REG);
  PUT_MODE (zero_reg_rtx, QImode);
  XINT (zero_reg_rtx, 0) = ZERO_REGNO;
}

/*  return register class from register number */

static int reg_class_tab[]={
  GENERAL_REGS,GENERAL_REGS,GENERAL_REGS,GENERAL_REGS,GENERAL_REGS,
  GENERAL_REGS,GENERAL_REGS,GENERAL_REGS,GENERAL_REGS,GENERAL_REGS,
  GENERAL_REGS,GENERAL_REGS,GENERAL_REGS,GENERAL_REGS,GENERAL_REGS,
  GENERAL_REGS, /* r0 - r15 */
  LD_REGS,LD_REGS,LD_REGS,LD_REGS,LD_REGS,LD_REGS,LD_REGS,
  LD_REGS,                      /* r16 - 23 */
  ADDW_REGS,ADDW_REGS,          /* r24,r25 */
  POINTER_X_REGS,POINTER_X_REGS, /* r26,27 */
  POINTER_Y_REGS,POINTER_Y_REGS, /* r28,r29 */
  POINTER_Z_REGS,POINTER_Z_REGS, /* r30,r31 */
  STACK_REG,STACK_REG           /* SPL,SPH */
};

/* Return register class for register R */

enum reg_class
avr_regno_reg_class (r)
     int r;
{
  if (r <= 33)
    return reg_class_tab[r];
  return ALL_REGS;
}


/* A C expression which defines the machine-dependent operand
   constraint letters for register classes.  If C is such a
   letter, the value should be the register class corresponding to
   it.  Otherwise, the value should be `NO_REGS'.  The register
   letter `r', corresponding to class `GENERAL_REGS', will not be
   passed to this macro; you do not need to handle it.  */

enum reg_class
avr_reg_class_from_letter  (c)
     int c;
{
  switch (c)
    {
    case 't' : return R0_REG;
    case 'b' : return BASE_POINTER_REGS;
    case 'e' : return POINTER_REGS;
    case 'w' : return ADDW_REGS;
    case 'd' : return LD_REGS;
    case 'l' : return NO_LD_REGS;
    case 'a' : return SIMPLE_LD_REGS;
    case 'x' : return POINTER_X_REGS;
    case 'y' : return POINTER_Y_REGS;
    case 'z' : return POINTER_Z_REGS;
    case 'q' : return STACK_REG;
    default: break;
    }
  return NO_REGS;
}

/* Return non-zero if FUNC is a naked function.  */

static int
avr_naked_function_p (func)
     tree func;
{
  tree a;

  if (TREE_CODE (func) != FUNCTION_DECL)
    abort ();
  
  a = lookup_attribute ("naked", DECL_MACHINE_ATTRIBUTES (func));
  return a != NULL_TREE;
}

/* Return nonzero if FUNC is an interrupt function as specified
   by the "interrupt" attribute.  */

static int
interrupt_function_p (func)
     tree func;
{
  tree a;

  if (TREE_CODE (func) != FUNCTION_DECL)
    return 0;

  a = lookup_attribute ("interrupt", DECL_MACHINE_ATTRIBUTES (func));
  return a != NULL_TREE;
}

/* Return nonzero if FUNC is an signal function as specified
   by the "signal" attribute.  */

static int
signal_function_p (func)
     tree func;
{
  tree a;

  if (TREE_CODE (func) != FUNCTION_DECL)
    return 0;

  a = lookup_attribute ("signal", DECL_MACHINE_ATTRIBUTES (func));
  return a != NULL_TREE;
}

/* Compute offset between arg_pointer and frame_pointer */

int
initial_elimination_offset (from,to)
     int from ATTRIBUTE_UNUSED;
     int to ATTRIBUTE_UNUSED;
{
  int reg;
  int interrupt_func_p = interrupt_function_p (current_function_decl);
  int signal_func_p = signal_function_p (current_function_decl);
  int leaf_func_p = leaf_function_p ();
  int offset= frame_pointer_needed ? 2 : 0;

  for (reg = 0; reg < 32; ++reg)
    {
      if ((!leaf_func_p && (call_used_regs[reg]
			    && (interrupt_func_p || signal_func_p)))
	  || (regs_ever_live[reg]
	      && (!call_used_regs[reg] || interrupt_func_p || signal_func_p)
	      && ! (frame_pointer_needed
		    && (reg == REG_Y || reg == (REG_Y+1)))))
	{
	  ++offset;
	}
    }
  return get_frame_size () + 2 + 1 + offset;
}

/* This function checks sequence of live registers */

static int
sequent_regs_live ()
{
  int reg;
  int live_seq=0;
  int cur_seq=0;

  for (reg = 0; reg < 18; ++reg)
    {
      if (!call_used_regs[reg])
	{
	  if (regs_ever_live[reg])
	    {
	      ++live_seq;
	      ++cur_seq;
	    }
	  else
	    cur_seq = 0;
	}
    }

  if (!frame_pointer_needed)
    {
      if (regs_ever_live[REG_Y])
	{
	  ++live_seq;
	  ++cur_seq;
	}
      else
	cur_seq = 0;

      if (regs_ever_live[REG_Y+1])
	{
	  ++live_seq;
	  ++cur_seq;
	}
      else
	cur_seq = 0;
    }
  else
    {
      cur_seq += 2;
      live_seq += 2;
    }
  return (cur_seq == live_seq) ? live_seq : 0;
}


/* Output function prologue */

void
function_prologue (FILE *file, int size)
{
  int reg;
  int interrupt_func_p;
  int signal_func_p;
  int leaf_func_p;
  int main_p;
  int live_seq;
  int minimize;
  
  if (avr_naked_function_p (current_function_decl))
    {
      fprintf (file, "/* prologue: naked */\n");
      return;
    }

  interrupt_func_p = interrupt_function_p (current_function_decl);
  signal_func_p = signal_function_p (current_function_decl);
  leaf_func_p = leaf_function_p ();
  main_p = ! strcmp ("main", current_function_name);
  live_seq = sequent_regs_live ();
  minimize = (TARGET_CALL_PROLOGUES
	      && !interrupt_func_p && !signal_func_p && live_seq);

  last_insn_address = 0;
  prologue_size = 0;
  fprintf (file, "/* prologue: frame size=%d */\n", size);
  
  if (interrupt_func_p)
    {
      fprintf (file,"\tsei\n");
      ++prologue_size;
    }
  if (interrupt_func_p | signal_func_p)
    {
      fprintf (file, "\t"
               AS1 (push,__zero_reg__)   CR_TAB
               AS1 (push,__tmp_reg__)    CR_TAB
	       AS2 (in,__tmp_reg__,__SREG__) CR_TAB
	       AS1 (push,__tmp_reg__)    CR_TAB
	       AS1 (clr,__zero_reg__)    "\n");
      prologue_size += 5;
    }
  if (main_p)
    {
      fprintf (file, ("\t" 
		      AS2 (ldi, r28, lo8(%s - %d)) CR_TAB
		      AS2 (ldi, r29, hi8(%s - %d)) CR_TAB
		      AS2 (out,__SP_L__,r28)       CR_TAB
		      AS2 (out,__SP_H__,r29) "\n"),
	       initial_stack, size, initial_stack, size);
      
      prologue_size += 4;
    }
  else if (minimize && (frame_pointer_needed || live_seq > 6)) 
    {
      fprintf (file, ("\t"
		      AS2 (ldi, r26, %d) CR_TAB
		      AS2 (ldi, r27, %d) CR_TAB), size & 0xff, size / 0x100);

      fprintf (file, (AS2 (ldi, r30, pm_lo8(.L_%s_body)) CR_TAB
		      AS2 (ldi, r31, pm_hi8(.L_%s_body)) CR_TAB)
	       ,current_function_name, current_function_name);
      
      prologue_size += 4;
      
      if (AVR_MEGA)
	{
	  fprintf (file, AS1 (jmp,__prologue_saves__+%d) "\n",
		   (18 - live_seq) * 2);
	  prologue_size += 2;
	}
      else
	{
	  fprintf (file, AS1 (rjmp,__prologue_saves__+%d) "\n",
		   (18 - live_seq) * 2);
	  ++prologue_size;
	}
      fprintf (file, ".L_%s_body:\n", current_function_name);
    }
  else
    {
      for (reg = 0; reg < 32; ++reg)
	{
	  if ((!leaf_func_p
	       && (call_used_regs[reg]
		   && (interrupt_func_p || signal_func_p)
		   && !(reg == TMP_REGNO || reg == ZERO_REGNO)))
	      || (regs_ever_live[reg]
		  && (!call_used_regs[reg]
		      || interrupt_func_p || signal_func_p)
		  && ! (frame_pointer_needed
			&& (reg == REG_Y || reg == (REG_Y+1)))))
	    {
	      fprintf (file, "\t" AS1 (push,%s) "\n", avr_regnames[reg]);
	      ++prologue_size;
	    }
	}
      if (frame_pointer_needed)
	{
	  {
	    fprintf (file, "\t"
		     AS1 (push,r28) CR_TAB
		     AS1 (push,r29) CR_TAB
		     AS2 (in,r28,__SP_L__) CR_TAB
		     AS2 (in,r29,__SP_H__) "\n");
	    prologue_size += 4;
	    if (size)
	      {
		if (size > 63)
		  {
		    fprintf (file, ("\t"
				    AS2 (subi,r28,%d) CR_TAB
				    AS2 (sbci,r29,%d) CR_TAB)
			     , size & 0xff, size / 0x100);
		    prologue_size += 2;
		  }
		else
		  {
		    fprintf (file, "\t" AS2 (sbiw,r28,%d) CR_TAB, size);
		    ++prologue_size;
		  }
		if (interrupt_func_p)
		  {
		    fprintf (file,
			     "cli" CR_TAB
			     AS2 (out,__SP_L__,r28) CR_TAB
			     "sei" CR_TAB
			     AS2 (out,__SP_H__,r29) "\n");
		    prologue_size += 4;
		  }
		else if (signal_func_p || TARGET_NO_INTERRUPTS)
		  {
		    fprintf (file,
			     AS2 (out,__SP_L__,r28) CR_TAB
			     AS2 (out,__SP_H__,r29) "\n");
		    prologue_size += 2;
		  }
		else
		  {
		    fprintf (file,
			     AS2 (in,__tmp_reg__,__SREG__) CR_TAB
			     "cli" CR_TAB
			     AS2 (out,__SP_L__,r28) CR_TAB
			     AS2 (out,__SREG__,__tmp_reg__) CR_TAB
			     AS2 (out,__SP_H__,r29) "\n");
		    prologue_size += 5;
		  }
	      }
	  }
	}
    }
  fprintf (file, "/* prologue end (size=%d) */\n", prologue_size);
}

/* Output function epilogue */

void
function_epilogue (FILE *file, int size)
{
  int reg;
  int interrupt_func_p;
  int signal_func_p;
  int leaf_func_p;
  int main_p;
  int function_size;
  int live_seq;
  int minimize;

  if (avr_naked_function_p (current_function_decl))
    {
      fprintf (file, "/* epilogue: naked */\n");
      return;
    }

  interrupt_func_p = interrupt_function_p (current_function_decl);
  signal_func_p = signal_function_p (current_function_decl);
  leaf_func_p = leaf_function_p ();
  main_p = ! strcmp ("main", current_function_name);
  function_size = (insn_addresses[INSN_UID (get_last_insn ())]
		   - insn_addresses[INSN_UID (get_insns ())]);
  live_seq = sequent_regs_live ();
  minimize = (TARGET_CALL_PROLOGUES
	      && !interrupt_func_p && !signal_func_p && live_seq);
  
  epilogue_size = 0;
  fprintf (file, "/* epilogue: frame size=%d */\n", size);
  if (main_p)
    {
      fprintf (file, "__stop_progIi__:\n\trjmp __stop_progIi__\n");
      ++epilogue_size;
    }
  else if (minimize && (frame_pointer_needed || live_seq > 4))
    {
      fprintf (file, ("\t" AS2 (ldi, r30, %d) CR_TAB), live_seq);
      ++epilogue_size;
      if (frame_pointer_needed)
	{
	  if (size)
	    {
	      if (size > 63)
		{
		  fprintf (file, AS2 (subi,r28,lo8(-%d)) CR_TAB, size);
		  fprintf (file, AS2 (sbci,r29,hi8(-%d)) CR_TAB, size);
		  epilogue_size += 2;
		}
	      else
		{
		  fprintf (file, AS2 (adiw,r28,%d) CR_TAB, size);
		  ++epilogue_size;
		}
	    }
	}
      else
	{
	  fprintf (file, (AS2 (in , r28, __SP_L__) CR_TAB
			  AS2 (in , r29, __SP_H__) CR_TAB));
	  epilogue_size += 2;
	}
      
      if (AVR_MEGA)
	{
	  fprintf (file, AS1 (jmp,__epilogue_restores__+%d) "\n",
		   (18 - live_seq) * 2);
	  epilogue_size += 2;
	}
      else
	{
	  fprintf (file, AS1 (rjmp,__epilogue_restores__+%d) "\n",
		   (18 - live_seq) * 2);
	  ++epilogue_size;
	}
    }
  else
    {
      if (frame_pointer_needed)
	{
	  if (size)
	    {
	      if (size > 63)
		{
		  fprintf (file, "\t" AS2 (subi,r28,lo8(-%d)) CR_TAB, size);
		  fprintf (file, AS2 (sbci,r29,hi8(-%d)) CR_TAB, size);
		  epilogue_size += 2;
		}
	      else
		{
		  fprintf (file, "\t" AS2 (adiw,r28,%d) CR_TAB, size);
		  ++epilogue_size;
		}
	      if (interrupt_func_p | signal_func_p)
		{
		  fprintf (file,
			   "cli" CR_TAB
			   AS2 (out,__SP_L__,r28) CR_TAB
			   AS2 (out,__SP_H__,r29) "\n");
		  epilogue_size += 3;
		}
	      else if (TARGET_NO_INTERRUPTS)
		{
		  fprintf (file,
			   AS2 (out,__SP_L__,r28) CR_TAB
			   AS2 (out,__SP_H__,r29) "\n");
		  epilogue_size += 2;
		}
	      else
		{
		  fprintf (file,
			   AS2 (in,__tmp_reg__,__SREG__) CR_TAB
			   "cli" CR_TAB
			   AS2 (out,__SP_L__,r28) CR_TAB
			   AS2 (out,__SREG__,__tmp_reg__) CR_TAB
			   AS2 (out,__SP_H__,r29) "\n");
		  epilogue_size += 5;
		}
	    }
	  fprintf (file, "\t"
		   AS1 (pop,r29) CR_TAB
		   AS1 (pop,r28) "\n");
	  epilogue_size += 2;
	}

      for (reg = 31; reg >= 0; --reg)
	{
	  if ((!leaf_func_p
	       && (call_used_regs[reg]
		   && (interrupt_func_p || signal_func_p)
		   && !(reg == TMP_REGNO || reg == ZERO_REGNO)))
	      || (regs_ever_live[reg]
		  && (!call_used_regs[reg]
		      || interrupt_func_p || signal_func_p)
		  && ! (frame_pointer_needed
			&& (reg == REG_Y || reg == (REG_Y+1)))))
	    {
	      fprintf (file, "\t" AS1 (pop,%s) "\n", avr_regnames[reg]);
	      ++epilogue_size;
	    }
	}
      
      if (interrupt_func_p | signal_func_p)
	{
	  fprintf (file, "\t"
		   AS1 (pop,__tmp_reg__)      CR_TAB
		   AS2 (out,__SREG__,__tmp_reg__) CR_TAB
		   AS1 (pop,__tmp_reg__)      CR_TAB
		   AS1 (pop,__zero_reg__)     "\n");
	  epilogue_size += 4;
	  fprintf (file, "\treti\n");
	}
      else
	fprintf (file, "\tret\n");
      ++epilogue_size;
    }
  
  fprintf (file, "/* epilogue end (size=%d) */\n", epilogue_size);
  fprintf (file, "/* function %s size %d (%d) */\n", current_function_name,
	   prologue_size + function_size + epilogue_size, function_size);
  commands_in_file += prologue_size + function_size + epilogue_size;
  commands_in_prologues += prologue_size;
  commands_in_epilogues += epilogue_size;
}


/* Return nonzero if X (an RTX) is a legitimate memory address on the target
   machine for a memory operand of mode MODE.  */

int
legitimate_address_p (mode, x, strict)
     enum machine_mode mode;
     rtx x;
     int strict;
{
  int r = 0;
  if (TARGET_ALL_DEBUG)
    {
      fprintf (stderr, "mode: (%s) %s %s %s %s:",
	       GET_MODE_NAME(mode),
	       strict ? "(strict)": "",
	       reload_completed ? "(reload_completed)": "",
	       reload_in_progress ? "(reload_in_progress)": "",
	       reg_renumber ? "(reg_renumber)" : "");
      if (GET_CODE (x) == PLUS
	  && REG_P (XEXP (x, 0))
	  && GET_CODE (XEXP (x, 1)) == CONST_INT
	  && INTVAL (XEXP (x, 1)) >= 0
	  && INTVAL (XEXP (x, 1)) <= (64 - GET_MODE_SIZE (mode))
	  && reg_renumber
	  )
	fprintf (stderr, "(r%d ---> r%d)", REGNO (XEXP (x, 0)),
		 true_regnum (XEXP (x, 0)));
      debug_rtx (x);
    }
  if (REG_P (x) && (strict ? REG_OK_FOR_BASE_STRICT_P (x)
                    : REG_OK_FOR_BASE_NOSTRICT_P (x)))
    r = 'R';
  else if (CONSTANT_ADDRESS_P (x))
    r = 'S';
  else if (GET_CODE (x) == PLUS
           && REG_P (XEXP (x, 0))
	   && GET_CODE (XEXP (x, 1)) == CONST_INT
	   && INTVAL (XEXP (x, 1)) >= 0)
    {
      int fit = INTVAL (XEXP (x, 1)) <= (64 - GET_MODE_SIZE (mode));
      if (fit)
	{
	  if (! strict
	      || REGNO (XEXP (x,0)) == REG_Y || REGNO (XEXP (x,0)) == REG_Z)
	      r = 'Q';
	  if (XEXP (x,0) == frame_pointer_rtx
	      || XEXP (x,0) == arg_pointer_rtx)
	    r = 'Q';
	}
      else if (frame_pointer_needed && XEXP (x,0) == frame_pointer_rtx)
	r = 'U';
    }
  else if ((GET_CODE (x) == PRE_DEC || GET_CODE (x) == POST_INC)
           && REG_P (XEXP (x, 0))
           && (strict ? REG_OK_FOR_BASE_STRICT_P (XEXP (x, 0))
               : REG_OK_FOR_BASE_NOSTRICT_P (XEXP (x, 0))))
    {
      r = 'T';
    }
  if (TARGET_ALL_DEBUG)
    {
      fprintf (stderr, "   ret = %c\n", r);
    }
  return r;
}

/* Attempts to replace X with a valid
   memory address for an operand of mode MODE  */

rtx
legitimize_address (x, oldx, mode)
     rtx x;
     rtx oldx;
     enum machine_mode mode;
{
  x = oldx;
  if (TARGET_ALL_DEBUG)
    {
      fprintf (stderr, "legitimize_address mode: %s", GET_MODE_NAME(mode));
      debug_rtx (oldx);
    }
  
  if (GET_CODE (oldx) == PLUS
      && REG_P (XEXP (oldx,0)))
    {
      if (REG_P (XEXP (oldx,1)))
	x = force_reg (GET_MODE (oldx), oldx);
      else if (GET_CODE (XEXP (oldx, 1)) == CONST_INT)
	{
	  int offs = INTVAL (XEXP (oldx,1));
	  if (frame_pointer_rtx != XEXP (oldx,0))
	    if (offs > 64 - GET_MODE_SIZE (mode))
	      {
		if (TARGET_ALL_DEBUG)
		  fprintf (stderr, "force_reg (big offset)\n");
		x = force_reg (GET_MODE (oldx), oldx);
	      }
	}
    }
  return x;
}


/* Return a pointer register name as a string */

static char *
ptrreg_to_str (regno)
     int regno;
{
  switch (regno)
    {
    case REG_X: return "X";
    case REG_Y: return "Y";
    case REG_Z: return "Z";
    default:
      fatal ("register r%d isn't a pointer\n", regno);
    }
  return NULL;
}

/* Return the condition name as a string.
   Used in conditional jump constructing  */

static char *
cond_string (code)
     enum rtx_code code;
{
  switch (code)
    {
    case NE:
      return "ne";
    case EQ:
      return "eq";
    case GE:
      if (cc_prev_status.flags & CC_OVERFLOW_UNUSABLE)
	return "pl";
      else
	return "ge";
    case GT:
      fatal ("Internal compiler bug: command `bgt'");
    case LE:
      fatal ("Internal compiler bug: command `ble'");
    case LT:
      if (cc_prev_status.flags & CC_OVERFLOW_UNUSABLE)
	return "mi";
      else
	return "lt";
    case GEU:
      return "sh";
    case GTU:
      fatal ("Internal compiler bug: command `bgtu'");
    case LEU:
      fatal ("Internal compiler bug: command `bleu'");
    case LTU:
      return "lo";
    default:
      abort ();
    }
}

/* Output ADDR to FILE as address */

void
print_operand_address (file, addr)
     FILE *file;
     rtx addr;
{
  switch (GET_CODE (addr))
    {
    case REG:
      fprintf (file, ptrreg_to_str (REGNO (addr)));
      break;

    case PRE_DEC:
      fprintf (file, "-%s", ptrreg_to_str (REGNO (XEXP (addr, 0))));
      break;

    case POST_INC:
      fprintf (file, "%s+", ptrreg_to_str (REGNO (XEXP (addr, 0))));
      break;

    default:
      if (CONSTANT_ADDRESS_P (addr)
	  && (SYMBOL_REF_FLAG (addr) || GET_CODE (addr) == LABEL_REF))
	{
	  fprintf (file, "pm(");
	  output_addr_const (file,addr);
	  fprintf (file ,")");
	}
      else
	output_addr_const (file, addr);
    }
}


/* Output X as assembler operand to file FILE */
     
void
print_operand (file, x, code)
     FILE *file;
     rtx x;
     int code;
{
  int abcd = 0;

  if (code >= 'A' && code <= 'D')
    abcd = code - 'A';

  if (REG_P (x))
    {
      if (x == zero_reg_rtx)
	fprintf (file,"__zero_reg__");
      else
	fprintf (file, reg_names[true_regnum (x) + abcd]);
    }
  else if (GET_CODE (x) == CONST_INT)
    fprintf (file, "%d", INTVAL (x) + abcd);
  else if (GET_CODE (x) == MEM)
    {
      rtx addr = XEXP (x,0);
      if (code == 'K')
	{
	  if (CONSTANT_P (addr))
	    putc ('s', file);
	  else if (GET_CODE (addr) == PLUS)
	    putc ('d', file);
	}
      else if (CONSTANT_P (addr) && abcd)
	{
	  fputc ('(', file);
	  output_address (addr);
	  fprintf (file, ")+%d", abcd);
	}
      else if (GET_CODE (addr) == PLUS)
	{
	  print_operand_address (file, XEXP (addr,0));
	  if (REGNO (XEXP (addr, 0)) == REG_X)
	    fatal_insn ("Internal compiler bug.\nBad address:"
			,addr);
	  fputc ('+', file);
	  print_operand (file, XEXP (addr,1), code);
	}
      else
	print_operand_address (file, addr);
    }
  else if (GET_CODE (x) == CONST_DOUBLE)
    {
      long val;
      REAL_VALUE_TYPE rv;
      if (GET_MODE (x) != SFmode)
	fatal_insn ("Internal compiler bug. Unknown mode:", x);
      REAL_VALUE_FROM_CONST_DOUBLE (rv, x);
      REAL_VALUE_TO_TARGET_SINGLE (rv, val);
      asm_fprintf (file, "0x%x", val);
    }
  else if (code == 'j')
    asm_fprintf (file, cond_string (GET_CODE (x)));
  else if (code == 'k')
    asm_fprintf (file, cond_string (reverse_condition (GET_CODE (x))));
  else
    print_operand_address (file, x);
}

/* Recognise operand OP of mode MODE used in call instructions */

int
call_insn_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) == MEM)
    {
      rtx inside = XEXP (op, 0);
      if (register_operand (inside, Pmode))
        return 1;
      if (CONSTANT_ADDRESS_P (inside))
        return 1;
    }
  return 0;
}

/* Update the condition code in the INSN.  */

void
notice_update_cc (body, insn)
     rtx body ATTRIBUTE_UNUSED;
     rtx insn;
{
  switch (get_attr_cc (insn))
    {
    case CC_NONE:
      /* Insn does not affect CC at all.  */
      break;

    case CC_SET_N:
      CC_STATUS_INIT;
      break;

    case CC_SET_ZN:
      {
	rtx set = single_set (insn);
	CC_STATUS_INIT;
	if (set)
	  {
	    cc_status.flags |= CC_NO_OVERFLOW;
	    cc_status.value1 = SET_DEST (set);
	  }
      }
      break;

    case CC_SET_CZN:
      /* Insn sets the Z,N,C flags of CC to recog_operand[0].
         The V flag may or may not be known but that's ok because
         alter_cond will change tests to use EQ/NE.  */
      {
	rtx set = single_set (insn);
	CC_STATUS_INIT;
	if (set)
	  {
	    cc_status.value1 = SET_DEST (set);
	    cc_status.flags |= CC_OVERFLOW_UNUSABLE;
	  }
      }
      break;

    case CC_COMPARE:
      {
	rtx set = single_set (insn);
	CC_STATUS_INIT;
	if (set)
	  cc_status.value1 = SET_SRC (set);
      }
      break;

    case CC_CLOBBER:
      /* Insn doesn't leave CC in a usable state.  */
      CC_STATUS_INIT;
      break;
    }
}

/* Return maximum number of consecutive registers of
   class CLASS needed to hold a value of mode MODE.  */

int
class_max_nregs (class, mode)
     enum reg_class class ATTRIBUTE_UNUSED;
     enum machine_mode mode;
{
  return ((GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD);
}

/* Choose mode for jump insn:
   1 - relative jump in range -63 <= x <= 62 ;
   2 - relative jump in range -2046 <= x <= 2045 ;
   3 - absolute jump (only for ATmega[16]03).  */

int
avr_jump_mode (x,insn)
     rtx x;                     /* jump operand */
     rtx insn;                  /* jump insn */
{
  int dest_addr = insn_addresses[INSN_UID (GET_MODE (x) == LABEL_REF
                                           ? XEXP (x, 0) : x)];
  int cur_addr = insn_addresses[INSN_UID (insn)];
  int jump_distance = cur_addr - dest_addr;
  
  if (-63 <= jump_distance && jump_distance <= 62)
    return 1;
  else if (-2046 <= jump_distance && jump_distance <= 2045)
    return 2;
  else if (AVR_MEGA)
    return 3;
  
  return 2;
}

/* return a AVR condition jump commands.
 LEN is a number returned by avr_jump_mode function.  */

char *
ret_cond_branch (cond,len)
     RTX_CODE cond;
     int len;
{
  switch (cond)
    {
    case GT:
      if (cc_prev_status.flags & CC_OVERFLOW_UNUSABLE)
	return (len == 1 ? (AS1 (breq,_PC_+2) CR_TAB
			    AS1 (brpl,%0)) :
		len == 2 ? (AS1 (breq,_PC_+4) CR_TAB
			    AS1 (brmi,_PC_+2) CR_TAB
			    AS1 (rjmp,%0)) :
		(AS1 (breq,_PC_+6) CR_TAB
		 AS1 (brmi,_PC_+4) CR_TAB
		 AS1 (jmp,%0)));
	  
      else
	return (len == 1 ? (AS1 (breq,_PC_+2) CR_TAB
			    AS1 (brge,%0)) :
		len == 2 ? (AS1 (breq,_PC_+4) CR_TAB
			    AS1 (brlt,_PC_+2) CR_TAB
			    AS1 (rjmp,%0)) :
		(AS1 (breq,_PC_+6) CR_TAB
		 AS1 (brlt,_PC_+4) CR_TAB
		 AS1 (jmp,%0)));
    case GTU:
      return (len == 1 ? (AS1 (breq,_PC_+2) CR_TAB
                          AS1 (brsh,%0)) :
              len == 2 ? (AS1 (breq,_PC_+4) CR_TAB
                          AS1 (brlo,_PC_+2) CR_TAB
                          AS1 (rjmp,%0)) :
              (AS1 (breq,_PC_+6) CR_TAB
               AS1 (brlo,_PC_+4) CR_TAB
               AS1 (jmp,%0)));
    case LE:
      if (cc_prev_status.flags & CC_OVERFLOW_UNUSABLE)
	return (len == 1 ? (AS1 (breq,%0) CR_TAB
			    AS1 (brmi,%0)) :
		len == 2 ? (AS1 (breq,_PC_+2) CR_TAB
			    AS1 (brpl,_PC_+2) CR_TAB
			    AS1 (rjmp,%0)) :
		(AS1 (breq,_PC_+2) CR_TAB
		 AS1 (brpl,_PC_+4) CR_TAB
		 AS1 (jmp,%0)));
      else
	return (len == 1 ? (AS1 (breq,%0) CR_TAB
			    AS1 (brlt,%0)) :
		len == 2 ? (AS1 (breq,_PC_+2) CR_TAB
			    AS1 (brge,_PC_+2) CR_TAB
			    AS1 (rjmp,%0)) :
		(AS1 (breq,_PC_+2) CR_TAB
		 AS1 (brge,_PC_+4) CR_TAB
		 AS1 (jmp,%0)));
    case LEU:
      return (len == 1 ? (AS1 (breq,%0) CR_TAB
                          AS1 (brlo,%0)) :
              len == 2 ? (AS1 (breq,_PC_+2) CR_TAB
                          AS1 (brsh,_PC_+2) CR_TAB
			  AS1 (rjmp,%0)) :
              (AS1 (breq,_PC_+2) CR_TAB
               AS1 (brsh,_PC_+4) CR_TAB
	       AS1 (jmp,%0)));
    default:
      switch (len)
        {
        case 1:
          return AS1 (br%j1,%0);
        case 2:
          return (AS1 (br%k1,_PC_+2) CR_TAB
                  AS1 (rjmp,%0));
        default:
          return (AS1 (br%k1,_PC_+4) CR_TAB
                  AS1 (jmp,%0));
        }
    }
  return "";
}

/* Predicate function for immediate operand which fits to byte (8bit) */

int
byte_immediate_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == CONST_INT
          && INTVAL (op) <= 0xff && INTVAL (op) >= 0);
}

/* Output all insn addresses and their sizes into the assembly language
   output file.  This is helpful for debugging whether the length attributes
   in the md file are correct.
   Output insn cost for next insn.  */

void
final_prescan_insn (insn, operand, num_operands)
     rtx insn, *operand ATTRIBUTE_UNUSED;
     int num_operands ATTRIBUTE_UNUSED;
{
  int uid = INSN_UID (insn);

  if (TARGET_INSN_SIZE_DUMP || TARGET_ALL_DEBUG)
    {
      fprintf (asm_out_file, "/*DEBUG: 0x%x\t\t%d\t%d */\n", insn_addresses[uid],
               insn_addresses[uid] - last_insn_address,
	       rtx_cost (PATTERN (insn),INSN));
    }
  last_insn_address = insn_addresses[uid];

  if (TARGET_RTL_DUMP)
    {
      fprintf (asm_out_file, "/*****************\n");
      print_rtl_single (asm_out_file, insn);
      fprintf (asm_out_file, "*****************/\n");
    }
}

/* return 1 if undefined,
   1 if always true or always false  */

int
avr_simplify_comparision_p (mode, operator, x)
     enum machine_mode mode;
     RTX_CODE operator;
     rtx x;
{
  unsigned int max = (mode == QImode ? 0xff :
                      mode == HImode ? 0xffff :
                      mode == SImode ? 0xffffffffU : 0);
  if (max && operator && GET_CODE (x) == CONST_INT)
    {
      if (unsigned_condition (operator) != operator)
	max >>= 1;

      if (max != (INTVAL (x) & max)
	  && INTVAL (x) != 0xff)
	return 1;
    }
  return 0;
}


/* Returns nonzero if REGNO is the number of a hard
   register in which function arguments are sometimes passed.  */

int
function_arg_regno_p(r)
     int r;
{
  return (r >= 8 && r <= 25);
}

/* Initializing the variable cum for the state at the beginning
   of the argument list.  */

void
init_cumulative_args (cum, fntype, libname, indirect)
     CUMULATIVE_ARGS *cum;
     tree fntype;
     rtx libname;
     int indirect ATTRIBUTE_UNUSED;
{
  cum->nregs = 18;
  cum->regno = FIRST_CUM_REG;
  if (!libname)
    {
      int stdarg = (TYPE_ARG_TYPES (fntype) != 0
                    && (TREE_VALUE (tree_last (TYPE_ARG_TYPES (fntype)))
                        != void_type_node));
      if (stdarg)
        cum->nregs = 0;
    }
}

/* Controls whether a function argument is passed
   in a register, and which register. */

rtx
function_arg (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named ATTRIBUTE_UNUSED;
{
  int bytes;

  bytes = (mode == BLKmode) ? int_size_in_bytes (type) : GET_MODE_SIZE (mode);

  if (cum->nregs && bytes <= cum->nregs)
    return gen_rtx (REG, mode, cum->regno - bytes);
  return NULL_RTX;
}

/* Update the summarizer variable CUM to advance past an argument
   in the argument list.  */
   
void
function_arg_advance (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;      /* current arg information */
     enum machine_mode mode;    /* current arg mode */
     tree type;                 /* type of the argument or 0 if lib support */
     int named ATTRIBUTE_UNUSED; /* whether or not the argument was named */
{
  int bytes;

  bytes = (mode == BLKmode ? int_size_in_bytes (type) : GET_MODE_SIZE (mode));
  cum->nregs -= bytes;
  cum->regno -= bytes;

  if (cum->nregs <= 0)
    {
      cum->nregs = 0;
      cum->regno = FIRST_CUM_REG;
    }

  return;
}

/***********************************************************************
  Functions for outputting various mov's for a various modes
************************************************************************/
char *
out_movqi_r_mr (insn, op, l)
     rtx insn;
     rtx op[];
     int *l; /* instruction length */
{
  /* We handle CONSTANT_ADDRESS_P case in adjust_insn_length */
  if (l) *l=1;
  if (GET_CODE (op[1]) == MEM)
    {
      rtx x = XEXP (op[1],0);
      if (GET_CODE (x) == PLUS
	  && REG_P (XEXP (x,0))
	  && GET_CODE (XEXP (x,1)) == CONST_INT)
	{
	  if((INTVAL (XEXP (x,1)) - GET_MODE_SIZE (GET_MODE (op[1]))) >= 63)
	    {
	      int disp = INTVAL (XEXP (x,1));
	      if (REGNO (XEXP (x,0)) != REG_Y)
		fatal_insn ("Incorrect insn:",insn);
	      if (disp <= 63 + 64 - GET_MODE_SIZE (GET_MODE (op[1])))
		{
		  if (l)
		    *l = 3;
		  else
		    {
		      op[4] = GEN_INT (disp - 63);
		      return (AS2 (adiw, r28, %4) CR_TAB
			      AS2 (ldd, %0,Y+63)  CR_TAB
			      AS2 (sbiw, r28, %4));
		    }
		}
	      else
		{
		  op[4] = XEXP (x,1);
		  if (l)
		    *l = 5;
		  else
		    return (AS2 (subi, r28, lo8(-%4))  CR_TAB
			    AS2 (sbci, r29, hi8(-%4)) CR_TAB
			    AS2 (ld, %0,Y)              CR_TAB
			    AS2 (subi, r28, lo8(%4))   CR_TAB
			    AS2 (sbci, r29, hi8(%4)));
		}
	    }
	  else if (REGNO (XEXP (x,0)) == REG_X)
	    {
	      /* This is a paranoid case LEGITIMIZE_RELOAD_ADDRESS must exclude
		 it but I have this situation with extremal optimizing options
	      */
	      if (l)
		*l=3;
	      else
		{
		  output_asm_insn (AS2 (adiw, r26, %0),&XEXP (x,1));
		  output_asm_insn (AS2 (ld ,%0,X),op);
		  if (!reg_overlap_mentioned_p (op[0],XEXP (x,0)))
		    output_asm_insn (AS2 (sbiw, r26, %0),&XEXP (x,1));
		}
	      return "";
	    }
	}
    }
  return AS2 (ld%K1,%0,%1);
}

char *
out_movhi_r_mr (insn, op, l)
     rtx insn;
     rtx op[];
     int *l; /* instruction length */
{
  int reg_dest = true_regnum (op[0]);
  int reg_base = true_regnum (XEXP (op[1],0));
  int len_p = 1,tmp;
  int *real_l=l;

  if (!l)
    l = &tmp, len_p = 0;

  if (reg_base > 0)
    {
      if (reg_dest == reg_base)         /* R = (R) */
        return *l=3, (AS2 (ld,__tmp_reg__,%1+) CR_TAB
                      AS2 (ld,%B0,%1) CR_TAB
                      AS2 (mov,%A0,__tmp_reg__));
      else if (reg_base == REG_X)        /* (R26) */
        {
          if (reg_unused_after (insn, XEXP (op[1],0)))
            return *l=2, (AS2 (ld,%A0,X+) CR_TAB
                          AS2 (ld,%B0,X));
          else
            return *l=3, (AS2 (ld,%A0,X+) CR_TAB
                          AS2 (ld,%B0,X) CR_TAB
                          AS2 (sbiw,r26,1));
        }
      else                      /* (R)  */
        return  *l=2, (AS2 (ld,%A0,%1)    CR_TAB
                       AS2 (ldd,%B0,%1+1));
    }
  else if (GET_CODE (XEXP (op[1],0)) == PLUS) /* (R + i) */
    {
      int disp = INTVAL(XEXP (XEXP (op[1],0), 1));
      int reg_base = true_regnum (XEXP (XEXP (op[1],0), 0));
      
      if (disp > 64 - GET_MODE_SIZE (GET_MODE (op[1])))
	{
	  rtx x = XEXP (op[1],0);
	  if (REGNO (XEXP (x,0)) != REG_Y)
	    fatal_insn ("Incorrect insn:",insn);
	  if (disp <= 63 + 64 - GET_MODE_SIZE (GET_MODE (op[1])))
	    {
	      op[4] = GEN_INT (disp - 62);
	      return *l=4, (AS2 (adiw, r28, %4) CR_TAB
			    AS2 (ldd, %A0,Y+62)       CR_TAB
			    AS2 (ldd, %B0,Y+63)     CR_TAB
			    AS2 (sbiw, r28, %4));
	    }
	  else
	    {
	      op[4] = XEXP (x,1);
	      return *l=6, (AS2 (subi, r28, lo8(-%4))  CR_TAB
			    AS2 (sbci, r29, hi8(-%4)) CR_TAB
			    AS2 (ld, %A0,Y)             CR_TAB
			    AS2 (ldd, %B0,Y+1)          CR_TAB
			    AS2 (subi, r28, lo8(%4))   CR_TAB
			    AS2 (sbci, r29, hi8(%4)));
	    }
	}
      if (reg_base == REG_X)
	{
	  /* This is a paranoid case. LEGITIMIZE_RELOAD_ADDRESS must exclude
	     it but I have this situation with extremal optimization options
	   */
	  rtx ops[1];
	  ops[0] = XEXP (XEXP (op[1],0), 1);
	  if (real_l)
	    *l = 4;
	  else if (reg_base == reg_dest)
	    {
	      output_asm_insn (AS2 (adiw, r26, %0), ops);
	      output_asm_insn (AS2 (ld , __tmp_reg__, X+), op);
	      output_asm_insn (AS2 (ld , %B0, X), op);
	      output_asm_insn (AS2 (mov, %A0, __tmp_reg__),op);
	    }
	  else
	    {
	      output_asm_insn (AS2 (adiw, r26, %0), ops);
	      output_asm_insn (AS2 (ld , %A0, X+), op);
	      output_asm_insn (AS2 (ld , %B0, X), op);
	      if (INTVAL (ops[0]) == 63)
		{
		  output_asm_insn (AS2 (subi, r26, %0+1), ops);
		  output_asm_insn (AS2 (sbci, r26, 0), ops);
		}
	      else
		output_asm_insn (AS2 (sbiw, r26, %0+1), ops);
	    }
	  return "";
	}
      
      if (reg_base == reg_dest)	
	return *l=3, (AS2 (ldd,__tmp_reg__,%A1)    CR_TAB
		      AS2 (ldd,%B0,%B1)   CR_TAB
		      AS2 (mov,%A0,__tmp_reg__));
      else
	return *l=2, (AS2 (ldd,%A0,%A1)    CR_TAB
		      AS2 (ldd,%B0,%B1));
    }
  else if (GET_CODE (XEXP (op[1],0)) == PRE_DEC) /* (--R) */
    {
      if (reg_overlap_mentioned_p (op[0], XEXP (XEXP (op[1],0),0)))
	{
	  debug_rtx (insn);
	  fatal ("Internal error. Incorrect insn.");
	}
      return *l=2, (AS2 (ld,%B0,%1) CR_TAB
		    AS2 (ld,%A0,%1));
    }
  else if (GET_CODE (XEXP (op[1],0)) == POST_INC) /* (R++) */
    {
      if (reg_overlap_mentioned_p (op[0], XEXP (XEXP (op[1],0),0)))
	{
	  debug_rtx (insn);
	  fatal ("Internal error. Incorrect insn.");
	}
      return *l=2, (AS2 (ld,%A0,%1)  CR_TAB
		    AS2 (ld,%B0,%1));
    }
  else if (CONSTANT_ADDRESS_P (XEXP (op[1],0)))
        return *l=4, (AS2 (lds,%A0,%A1) CR_TAB
		      AS2 (lds,%B0,%B1));
  fatal_insn ("Unknown move insn:",insn);
  return "";
}

char *
out_movsi_r_mr (insn,op,l)
     rtx insn;
     rtx op[];
     int *l; /* instruction length */
{
  int reg_dest=true_regnum (op[0]);
  int reg_base=true_regnum (XEXP (op[1],0));
  int tmp;
  if (!l)
    l=&tmp;
  if (reg_base > 0)
    {
      if (reg_base == REG_X)        /* (R26) */
        {
          if (reg_dest == REG_X)
            return *l=6, (AS2 (adiw,r26,3) CR_TAB
                          AS2 (ld,%D0,X)  CR_TAB
                          AS2 (ld,%C0,-X) CR_TAB
                          AS2 (ld,__tmp_reg__,-X)  CR_TAB
                          AS2 (ld,%A0,-X)  CR_TAB
                          AS2 (mov,%B0,__tmp_reg__));
          else if (reg_dest == REG_X - 2)
            return *l=5, (AS2 (ld,%A0,X+)  CR_TAB
                          AS2 (ld,%B0,X+) CR_TAB
                          AS2 (ld,__tmp_reg__,X+)  CR_TAB
                          AS2 (ld,%D0,X)  CR_TAB
                          AS2 (mov,%C0,__tmp_reg__));
          else if (reg_unused_after (insn,XEXP (op[1],0)))
            return  *l=4, (AS2 (ld,%A0,X+)  CR_TAB
                           AS2 (ld,%B0,X+) CR_TAB
                           AS2 (ld,%C0,X+) CR_TAB
                           AS2 (ld,%D0,X));
          else
            return  *l=5, (AS2 (ld,%A0,X+)  CR_TAB
                           AS2 (ld,%B0,X+) CR_TAB
                           AS2 (ld,%C0,X+) CR_TAB
                           AS2 (ld,%D0,X)  CR_TAB
                           AS2 (sbiw,r26,3));
        }
      else
        {
          if (reg_dest == reg_base)
            return *l=5, (AS2 (ldd,%D0,%1+3) CR_TAB
                          AS2 (ldd,%C0,%1+2) CR_TAB
                          AS2 (ldd,__tmp_reg__,%1+1)  CR_TAB
                          AS2 (ld,%A0,%1)  CR_TAB
                          AS2 (mov,%B0,__tmp_reg__));
          else if (reg_base == reg_dest + 2)
            return *l=5, (AS2 (ld ,%A0,%1)    CR_TAB
                          AS2 (ldd,%B0,%1+1) CR_TAB
                          AS2 (ldd,__tmp_reg__,%1+2)  CR_TAB
                          AS2 (ldd,%D0,%1+3) CR_TAB
                          AS2 (mov,%C0,__tmp_reg__));
          else
            return *l=4, (AS2 (ld ,%A0,%1)   CR_TAB
                          AS2 (ldd,%B0,%1+1) CR_TAB
                          AS2 (ldd,%C0,%1+2) CR_TAB
                          AS2 (ldd,%D0,%1+3));
        }
    }
  else if (GET_CODE (XEXP (op[1],0)) == PLUS) /* (R + i) */
    {
      int disp = INTVAL(XEXP (XEXP (op[1],0), 1));
      
      if (disp > 64 - GET_MODE_SIZE (GET_MODE (op[1])))
	{
	  rtx x = XEXP (op[1],0);
	  if (REGNO (XEXP (x,0)) != REG_Y)
	    fatal_insn ("Incorrect insn:",insn);
	  if (disp <= 63 + 64 - GET_MODE_SIZE (GET_MODE (op[1])))
	    {
	      op[4] = GEN_INT (disp - 60);
	      return *l=6,(AS2 (adiw, r28, %4) CR_TAB
			   AS2 (ldd, %A0,Y+60)       CR_TAB
			   AS2 (ldd, %B0,Y+61)     CR_TAB
			   AS2 (ldd, %C0,Y+62)     CR_TAB
			   AS2 (ldd, %D0,Y+63)     CR_TAB
			   AS2 (sbiw, r28, %4));
	    }
	  else
	    {
	      op[4] = XEXP (x,1);
	      return *l=8,(AS2 (subi, r28, lo8(-%4))  CR_TAB
			   AS2 (sbci, r29, hi8(-%4)) CR_TAB
			   AS2 (ld, %A0,Y)             CR_TAB
			   AS2 (ldd, %B0,Y+1)          CR_TAB
			   AS2 (ldd, %C0,Y+2)          CR_TAB
			   AS2 (ldd, %D0,Y+3)          CR_TAB
			   AS2 (subi, r28, lo8(%4))   CR_TAB
			   AS2 (sbci, r29, hi8(%4)));
	    }
	}

      reg_base = true_regnum (XEXP (XEXP (op[1],0), 0));
      if (reg_dest == reg_base)
        return *l=5, (AS2 (ldd,%D0,%D1) CR_TAB
                      AS2 (ldd,%C0,%C1) CR_TAB
                      AS2 (ldd,__tmp_reg__,%B1)  CR_TAB
                      AS2 (ldd,%A0,%A1) CR_TAB
                      AS2 (mov,%B0,__tmp_reg__));
      else if (reg_dest == reg_base - 2)
        return *l=5, (AS2 (ldd,%A0,%A1) CR_TAB
                      AS2 (ldd,%B0,%B1) CR_TAB
                      AS2 (ldd,__tmp_reg__,%C1)  CR_TAB
                      AS2 (ldd,%D0,%D1) CR_TAB
                      AS2 (mov,%C0,__tmp_reg__));
      return *l=4, (AS2 (ldd,%A0,%A1) CR_TAB
                    AS2 (ldd,%B0,%B1) CR_TAB
                    AS2 (ldd,%C0,%C1) CR_TAB
                    AS2 (ldd,%D0,%D1));
    }
  else if (GET_CODE (XEXP (op[1],0)) == PRE_DEC) /* (--R) */
    return *l=4, (AS2 (ld,%D0,%1) CR_TAB
		  AS2 (ld,%C0,%1) CR_TAB
		  AS2 (ld,%B0,%1) CR_TAB
		  AS2 (ld,%A0,%1));
  else if (GET_CODE (XEXP (op[1],0)) == POST_INC) /* (R++) */
    return *l=4, (AS2 (ld,%A0,%1) CR_TAB
		  AS2 (ld,%B0,%1) CR_TAB
		  AS2 (ld,%C0,%1) CR_TAB
		  AS2 (ld,%D0,%1));
  else if (CONSTANT_ADDRESS_P (XEXP (op[1],0)))
      return *l=8, (AS2 (lds,%A0,%A1) CR_TAB
		    AS2 (lds,%B0,%B1) CR_TAB
		    AS2 (lds,%C0,%C1) CR_TAB
		    AS2 (lds,%D0,%D1));
    
  fatal_insn ("Unknown move insn:",insn);
  return "";
}

char *
out_movsi_mr_r (insn,op,l)
     rtx insn;
     rtx op[];
     int *l;
{
  int reg_base = true_regnum (XEXP (op[0],0));
  int reg_dest = true_regnum (op[1]);
  int tmp;
  if (!l)
    l = &tmp;
  if (CONSTANT_ADDRESS_P (XEXP (op[0],0)))
    return *l=8,(AS2 (sts,%A0,%A1) CR_TAB
		 AS2 (sts,%B0,%B1) CR_TAB
		 AS2 (sts,%C0,%C1) CR_TAB
		 AS2 (sts,%D0,%D1));
  if (reg_base > 0)                 /* (r) */
    {
      if (reg_base == REG_X)                /* (R26) */
        {
          if (reg_dest == REG_X)
            {
              if (reg_unused_after (insn,XEXP (op[0],0)))
                return *l=5, (AS2 (mov,__tmp_reg__,%B1) CR_TAB
                              AS2 (st,%0+,%A1) CR_TAB
                              AS2 (st,%0+,__tmp_reg__)  CR_TAB
                              AS2 (st,%0+,%C1) CR_TAB
                              AS2 (st,%0,%D1));
              else
                return *l=6, (AS2 (mov,__tmp_reg__,%B1) CR_TAB
                              AS2 (st,%0+,%A1) CR_TAB
                              AS2 (st,%0+,__tmp_reg__)  CR_TAB
                              AS2 (st,%0+,%C1) CR_TAB
                              AS2 (st,%0,%D1)  CR_TAB
                              AS2 (sbiw,r26,3));
            }
          else if (reg_base == reg_dest+2)
            {
              if (reg_unused_after (insn,XEXP (op[0],0)))
                return *l=7, (AS2 (mov,__zero_reg__,%C1) CR_TAB
                              AS2 (mov,__tmp_reg__,%D1) CR_TAB
                              AS2 (st,%0+,%A1) CR_TAB
                              AS2 (st,%0+,%B1) CR_TAB
                              AS2 (st,%0+,__zero_reg__)  CR_TAB
                              AS2 (st,%0,__tmp_reg__)   CR_TAB
                              AS1 (clr,__zero_reg__));
              else
                return *l=8, (AS2 (mov,__zero_reg__,%C1) CR_TAB
                              AS2 (mov,__tmp_reg__,%D1) CR_TAB
                              AS2 (st,%0+,%A1) CR_TAB
                              AS2 (st,%0+,%B1) CR_TAB
                              AS2 (st,%0+,__zero_reg__)  CR_TAB
                              AS2 (st,%0,__tmp_reg__)   CR_TAB
                              AS1 (clr,__zero_reg__)     CR_TAB
                              AS2 (sbiw,r26,3));
            }
          return *l=5, (AS2 (st,%0+,%A1)  CR_TAB
                        AS2 (st,%0+,%B1) CR_TAB
                        AS2 (st,%0+,%C1) CR_TAB
                        AS2 (st,%0,%D1)  CR_TAB
                        AS2 (sbiw,r26,3));
        }
      else
        return *l=4, (AS2 (st,%0,%A1)    CR_TAB
		      AS2 (std,%0+1,%B1) CR_TAB
		      AS2 (std,%0+2,%C1) CR_TAB
		      AS2 (std,%0+3,%D1));
    }
  else if (GET_CODE (XEXP (op[0],0)) == PLUS) /* (R + i) */
    {
      int disp = INTVAL(XEXP (XEXP (op[0],0), 1));
      if (disp > 64 - GET_MODE_SIZE (GET_MODE (op[0])))
	{
	  rtx x = XEXP (op[0],0);
	  if (REGNO (XEXP (x,0)) != REG_Y)
	    fatal_insn ("Incorrect insn:",insn);
	  if (disp <= 63 + 64 - GET_MODE_SIZE (GET_MODE (op[0])))
	    {
	      op[4] = GEN_INT (disp - 60);
	      return *l=6,(AS2 (adiw, r28, %4) CR_TAB
			   AS2 (std, Y+60,%A1)       CR_TAB
			   AS2 (std, Y+61,%B1)     CR_TAB
			   AS2 (std, Y+62,%C1)     CR_TAB
			   AS2 (std, Y+63,%D1)     CR_TAB
			   AS2 (sbiw, r28, %4));
	    }
	  else
	    {
	      op[4] = XEXP (x,1);
	      return *l=8,(AS2 (subi, r28, lo8(-%4))  CR_TAB
			   AS2 (sbci, r29, hi8(-%4)) CR_TAB
			   AS2 (st, Y,%A1)             CR_TAB
			   AS2 (std, Y+1,%B1)          CR_TAB
			   AS2 (std, Y+2,%C1)          CR_TAB
			   AS2 (std, Y+3,%D1)          CR_TAB
			   AS2 (subi, r28, lo8(%4))   CR_TAB
			   AS2 (sbci, r29, hi8(%4)));
	    }
	}
      return *l=4, (AS2 (std,%A0,%A1)    CR_TAB
		    AS2 (std,%B0,%B1) CR_TAB
		    AS2 (std,%C0,%C1) CR_TAB
		    AS2 (std,%D0,%D1));
    }
  else if (GET_CODE (XEXP (op[0],0)) == PRE_DEC) /* (--R) */
    return *l=4, (AS2 (st,%0,%D1) CR_TAB
		  AS2 (st,%0,%C1) CR_TAB
		  AS2 (st,%0,%B1) CR_TAB
		  AS2 (st,%0,%A1));
  else if (GET_CODE (XEXP (op[0],0)) == POST_INC) /* (R++) */
    return *l=4, (AS2 (st,%0,%A1)  CR_TAB
		  AS2 (st,%0,%B1) CR_TAB
		  AS2 (st,%0,%C1) CR_TAB
		  AS2 (st,%0,%D1));
  fatal_insn ("Unknown move insn:",insn);
  return "";
}

char *
output_movsisf(insn, operands, which_alternative)
     rtx insn;
     rtx operands[];
     int which_alternative;
{
  rtx link;
  switch (which_alternative)
    {
    case 0: /* mov r,r */
      if (true_regnum (operands[0]) > true_regnum (operands[1]))
        return (AS2 (mov,%D0,%D1) CR_TAB
	        AS2 (mov,%C0,%C1) CR_TAB
		AS2 (mov,%B0,%B1) CR_TAB
		AS2 (mov,%A0,%A1));
      else
        return (AS2 (mov,%A0,%A1) CR_TAB
	        AS2 (mov,%B0,%B1) CR_TAB
		AS2 (mov,%C0,%C1) CR_TAB
		AS2 (mov,%D0,%D1));
    case 1:  /* mov r,L */
      return (AS1 (clr,%A0) CR_TAB
	      AS1 (clr,%B0) CR_TAB
	      AS1 (clr,%C0) CR_TAB
	      AS1 (clr,%D0));
    case 2: /* mov r,d */
      if (GET_MODE (operands[0]) == SImode
	  && operands[1] == const1_rtx
	  && (link = find_reg_note (insn, REG_WAS_0, 0))
	  /* Make sure the insn that stored the 0 is still present.  */
	  && ! INSN_DELETED_P (XEXP (link, 0))
	  && GET_CODE (XEXP (link, 0)) != NOTE
	  /* Make sure cross jumping didn't happen here.  */
	  && no_labels_between_p (XEXP (link, 0), insn)
	  /* Make sure the reg hasn't been clobbered.  */
	  && ! reg_set_between_p (operands[0], XEXP (link, 0), insn))
      /* Fastest way to change a 0 to a 1.  */
        return AS1 (inc,%A0 ; reg_was_0);
      return (AS2 (ldi,%A0,lo8(%1))  CR_TAB
	      AS2 (ldi,%B0,hi8(%1)) CR_TAB
	      AS2 (ldi,%C0,hlo8(%1)) CR_TAB
	      AS2 (ldi,%D0,hhi8(%1)));
    case 3: /* mov r,m*/
    case 5:
      return out_movsi_r_mr (insn, operands, NULL);
    case 4: /* mov m,r*/
    case 6:
      {
	rtx save1=NULL;
	if (operands[1] == const0_rtx)
	  {
	    save1 = operands[1];
	    operands[1] = zero_reg_rtx;
	  }
	output_asm_insn (out_movsi_mr_r (insn,operands,NULL), operands);
	if (save1)
	  operands[1] = save1;
      }
    }
  return "";
}

char *
out_movqi_mr_r (insn, op, l)
     rtx insn;
     rtx op[];
     int *l; /* instruction length */
{
  if (l) *l=1;

  if (GET_CODE (op[0]) == MEM)
    {
      rtx x = XEXP (op[0],0);
      if (GET_CODE (x) == PLUS
	  && REG_P (XEXP (x,0))
	  && GET_CODE (XEXP (x,1)) == CONST_INT)
	{
	  if ((INTVAL (XEXP (x,1)) - GET_MODE_SIZE (GET_MODE (op[0]))) >= 63)
	    {
	      int disp = INTVAL (XEXP (x,1));
	      if (REGNO (XEXP (x,0)) != REG_Y)
		fatal_insn ("Incorrect insn:",insn);
	      if (disp <= 63 + 64 - GET_MODE_SIZE (GET_MODE (op[0])))
		{
		  if (l)
		    *l = 3;
		  else
		    {
		      op[4] = GEN_INT (disp - 63);
		      return (AS2 (adiw, r28, %4) CR_TAB
			      AS2 (std, Y+63,%1)        CR_TAB
			      AS2 (sbiw, r28, %4));
		    }
		}
	      else
		{
		  op[4] = XEXP (x,1);
		  if (l)
		    *l = 5;
		  else
		    return (AS2 (subi, r28, lo8(-%4))  CR_TAB
			    AS2 (sbci, r29, hi8(-%4)) CR_TAB
			    AS2 (st, Y,%1)              CR_TAB
			    AS2 (subi, r28, lo8(%4))   CR_TAB
			    AS2 (sbci, r29, hi8(%4)));
		}
	    }
	  else if (REGNO (XEXP (x,0)) == REG_X)
	    {
	      if (l)
		*l=4;
	      else
		{
		  int overlap_p = reg_overlap_mentioned_p (op[1],XEXP (x,0));
		  if (!overlap_p)
		    output_asm_insn (AS2 (mov, __tmp_reg__, %1),op);
		  output_asm_insn (AS2 (adiw, r26,%0),&XEXP (x,1));
		  if (overlap_p)
		    output_asm_insn (AS2 (st ,X,__tmp_reg__),op);
		  else
		    output_asm_insn (AS2 (st ,X,%1),op);
		  output_asm_insn (AS2 (sbiw ,r26,%0),&XEXP (x,1));
		}
	      return "";
	    }
	}
    }
  return AS2 (st%K0, %0,%1);
}

char *
out_movhi_mr_r (insn,op,l)
     rtx insn;
     rtx op[];
     int *l;
{
  int reg_base = true_regnum (XEXP (op[0],0));
  int reg_dest = true_regnum (op[1]);
  int tmp;
  if (!l)
    l = &tmp;
  if (CONSTANT_ADDRESS_P (XEXP (op[0],0)))
    return *l=4,(AS2 (sts,%A0,%A1) CR_TAB
		 AS2 (sts,%B0,%B1));
  if (reg_base > 0)
    {
      if (reg_base == REG_X)
        {
          if (reg_dest == REG_X)
            {
              if (reg_unused_after (insn, op[1]))
                return *l=3, (AS2 (mov,__tmp_reg__,r27) CR_TAB
                              AS2 (st ,X+,r26) CR_TAB
                              AS2 (st ,X,__tmp_reg__));
              else
                return *l=4, (AS2 (mov,__tmp_reg__,r27) CR_TAB
                              AS2 (st ,X+,r26) CR_TAB
                              AS2 (st ,X,__tmp_reg__)   CR_TAB
                              AS2 (sbiw,r26,1));
            }
          else
            {
              if (reg_unused_after (insn, XEXP (op[0],0)))
                return *l=2, (AS2 (st,X+,%A1) CR_TAB
                              AS2 (st,X,%B1));
              else
                return *l=3, (AS2 (st  ,X+,%A1) CR_TAB
                              AS2 (st  ,X,%B1) CR_TAB
                              AS2 (sbiw,r26,1));
            }
        }
      else
        return  *l=2, (AS2 (st ,%0,%A1)    CR_TAB
                       AS2 (std,%0+1,%B1));
    }
  else if (GET_CODE (XEXP (op[0],0)) == PLUS)
    {
      int disp = INTVAL(XEXP (XEXP (op[0],0), 1));
      if (disp > 64 - GET_MODE_SIZE (GET_MODE (op[0])))
	{
	  rtx x = XEXP (op[0],0);
	  if (REGNO (XEXP (x,0)) != REG_Y)
	    fatal_insn ("Incorrect insn:",insn);
	  if (disp <= 63 + 64 - GET_MODE_SIZE (GET_MODE (op[0])))
	    {
	      op[4] = GEN_INT (disp - 62);
	      return *l=4,(AS2 (adiw, r28, %4) CR_TAB
			   AS2 (std, Y+62,%A1) CR_TAB
			   AS2 (std, Y+63,%B1) CR_TAB
			   AS2 (sbiw, r28, %4));
	    }
	  else
	    {
	      op[4] = XEXP (x,1);
	      return *l=6,(AS2 (subi, r28, lo8(-%4))  CR_TAB
			   AS2 (sbci, r29, hi8(-%4)) CR_TAB
			   AS2 (st, Y,%A1)           CR_TAB
			   AS2 (std, Y+1,%B1)        CR_TAB
			   AS2 (subi, r28, lo8(%4))  CR_TAB
			   AS2 (sbci, r29, hi8(%4)));
	    }
	}
      return *l=2, (AS2 (std,%A0,%A1)    CR_TAB
		    AS2 (std,%B0,%B1));
    }  
  else if (GET_CODE (XEXP (op[0],0)) == PRE_DEC) /* (--R) */
    return *l=2, (AS2 (st,%0,%B1) CR_TAB
		  AS2 (st,%0,%A1));
  else if (GET_CODE (XEXP (op[0],0)) == POST_INC) /* (R++) */
    return *l=2, (AS2 (st,%0,%A1)  CR_TAB
		  AS2 (st,%0,%B1));
  fatal_insn ("Unknown move insn:",insn);
  return "";
}

/* Return 1 if frame pointer for current function required */

int
frame_pointer_required_p(void)
{
  return (current_function_calls_alloca
	  || current_function_args_info.nregs == 0
	  || current_function_varargs
  	  || get_frame_size () > 0);
}

/* Return 1 if the next insn is a JUMP_INSN with condition (GT,LE,GTU,LTU)  */

int
compare_diff_p (insn)
     rtx insn;
{
  rtx next = next_real_insn (insn);
  RTX_CODE cond = UNKNOWN;
  if (GET_CODE (next) == JUMP_INSN)
    {
      rtx pat = PATTERN (next);
      rtx src = SET_SRC (pat);
      rtx t = XEXP (src,0);
      cond = GET_CODE (t);
    }
  return (cond == GT || cond == GTU || cond == LE || cond == LEU) ? cond : 0;
}

/* Returns nonzero if INSN is a compare insn with the EQ or NE condition */

int
compare_eq_p (insn)
     rtx insn;
{
  rtx next = next_real_insn (insn);
  RTX_CODE cond = UNKNOWN;
  if (GET_CODE (next) == JUMP_INSN)
    {
      rtx pat = PATTERN (next);
      rtx src = SET_SRC (pat);
      rtx t = XEXP (src,0);
      cond = GET_CODE (t);
    }
  return (cond == EQ || cond == NE);
}


/* Output test instruction for HImode */

char *
out_tsthi (insn,l)
     rtx insn;
     int *l;
{
  if (!compare_eq_p (insn))
    {
      if (l) *l = 1;
      return AS1 (tst,%B0);
    }
  if (TEST_HARD_REG_CLASS (ADDW_REGS, true_regnum (SET_SRC (PATTERN (insn)))))
    {
      if (l) *l = 1;
      return AS2 (sbiw,%0,0);
    }
  if (compare_eq_p (insn) && reg_unused_after (insn, SET_SRC (PATTERN (insn))))
    {
      if (l) *l = 1;
      return AS2 (or,%A0,%B0);
    }
  if (l) *l = 2;
  return (AS2 (cp,%A0,__zero_reg__) CR_TAB
          AS2 (cpc,%B0,__zero_reg__));
}


/* Output test instruction for SImode */

char *
out_tstsi (insn,l)
     rtx insn;
     int *l;
{
  if (!compare_eq_p (insn))
    {
      if (l) *l = 1;
      return AS1 (tst,%D0);
    }
  if (TEST_HARD_REG_CLASS (ADDW_REGS, true_regnum (SET_SRC (PATTERN (insn)))))
    {
      if (l) *l = 3;
      return (AS2 (sbiw,%A0,0) CR_TAB
              AS2 (cpc,%C0,__zero_reg__) CR_TAB
              AS2 (cpc,%D0,__zero_reg__));
    }
  if (l) *l = 4;
  return (AS2 (cp,%A0,__zero_reg__) CR_TAB
          AS2 (cpc,%B0,__zero_reg__) CR_TAB
          AS2 (cpc,%C0,__zero_reg__) CR_TAB
          AS2 (cpc,%D0,__zero_reg__));
}


/* Generate asm equivalent for various shift's.
   Shift count are CONST_INT or REG.  */

void
out_shift_with_cnt (template,insn,operands,len)
     char * template;
     rtx insn;
     rtx operands[];
     int *len;
{
  rtx op[10];
  int l_hi=0;
  char str[300];
  op[0] = operands[0];
  op[1] = operands[1];
  op[2] = operands[2];
  op[3] = operands[3];
  str[0] = 0;
    
  if (CONSTANT_P (operands[2]))
    {
      if (len)
	++*len;
      else
	strcat (str, "ldi %3,lo8(%2)");
    }
  else if (GET_CODE (operands[2]) == MEM)
    {
      int mov_len;
      rtx op_mov[10];
      l_hi = 1;
      if (len)
	*len = 2;
      op[3] = op_mov[0] = tmp_reg_rtx;
      op_mov[1] = op[2];
      
      if (!len)
	{
	  output_asm_insn (out_movqi_r_mr (insn, op_mov, NULL), op_mov);
	  strcat (str,(AS2 (or,%3,%3)    CR_TAB
		       AS1 (breq,L_hi%=)));
	}
      else
	{
	  out_movqi_r_mr (insn, op_mov, &mov_len);
	  *len += mov_len;
	}
    }
  else if (register_operand (operands[2],QImode))
    {
      l_hi = 1;
      if (len)
	*len += 2;
      else
	strcat (str, (AS2 (or,%2,%2) CR_TAB
		      AS1 (breq,L_hi%=)));
      
      if (reg_unused_after (insn, operands[2]))
	{
	  op[3] = op[2];
	}
      else
	{
	  op[3] = tmp_reg_rtx;
	  if (len)
	    ++*len;
	  else
	    strcat (str, CR_TAB "mov %3,%2");
	}
    }
  if (!len)
    {
      strcat (str,"\n\t");
      strcat (str, template);
      if (l_hi)
	strcat (str, "\nL_hi%=:");
      output_asm_insn (str, op);
    }
}


/* 8bit shift left ((char)x << i)   */

char *
ashlqi3_out (insn,operands,len)
     rtx insn;
     rtx operands[];
     int *len;			/* insn length (may be NULL) */
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      int k;
      int *t=len;
      if (!len)
	len = &k;
      switch (INTVAL (operands[2]))
	{
	default: len = t; break;
	case 1:
	  *len=1;
	  return AS1 (lsl,%0);
	case 2:
	  *len=2;
	  return (AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0));
	case 3:
	  *len=3;
	  return (AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0));
	case 4:
	  if (TEST_HARD_REG_CLASS (LD_REGS, true_regnum (operands[0])))
	    {
	      *len=2;
	      return (AS1 (swap,%0) CR_TAB
		      AS2 (andi,%0,0xf0));
	    }
	  *len=4;
	  return (AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0));
	case 5:
	  if (TEST_HARD_REG_CLASS (LD_REGS, true_regnum (operands[0])))
	    {
	      *len=3;
	      return (AS1 (swap,%0) CR_TAB
		      AS1 (lsl,%0)  CR_TAB
		      AS2 (andi,%0,0xe0));
	    }
	  *len=5;
	  return (AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0));
	case 6:
	  if (TEST_HARD_REG_CLASS (LD_REGS, true_regnum (operands[0])))
	    {
	      *len=4;
	      return (AS1 (swap,%0) CR_TAB
		      AS1 (lsl,%0)  CR_TAB
		      AS1 (lsl,%0)  CR_TAB
		      AS2 (andi,%0,0xc0));
	    }
	  *len=6;
	  return (AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0));
	case 7:
	  *len=3;
	  return (AS1 (ror,%0) CR_TAB
		  AS1 (clr,%0) CR_TAB
		  AS1 (ror,%0));
	}
    }
  if (len)
    *len = 3;
  out_shift_with_cnt (AS1 (lsl,%0)      CR_TAB
		      AS1 (dec,%3)      CR_TAB
		      AS1 (brne,_PC_-6),
		      insn, operands, len);
  return "";
}


/* 16bit shift left ((short)x << i)   */

char *
ashlhi3_out (insn,operands,len)
     rtx insn;
     rtx operands[];
     int *len;
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      int k;
      int *t=len;
      if (!len)
	len = &k;
      switch (INTVAL (operands[2]))
	{
	default: len = t; break;
	case 1:
	  *len=2;
	  return (AS1 (lsl,%A0) CR_TAB
		  AS1 (rol,%B0));
	case 2:
	  *len=4;
	  return (AS1 (lsl,%A0) CR_TAB
		  AS1 (rol,%B0) CR_TAB
		  AS1 (lsl,%0)  CR_TAB
		  AS1 (rol,%B0));
	case 8:
	  if (true_regnum (operands[0]) + 1 == true_regnum (operands[1]))
	    return *len = 1, AS1 (clr,%A0);
	  else
	    return *len = 2, (AS2 (mov,%B0,%A1) CR_TAB
			      AS1 (clr,%A0));
	}
    }
  if (len)
    *len = 4;
  out_shift_with_cnt (AS1 (lsl,%0)  CR_TAB
		      AS1 (rol,%B0) CR_TAB
		      AS1 (dec,%3)  CR_TAB
		      AS1 (brne,_PC_-8),
		      insn, operands, len);
  return "";
}


/* 32bit shift left ((long)x << i)   */

char *
ashlsi3_out (insn,operands,len)
     rtx insn;
     rtx operands[];
     int *len;
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      int k;
      int *t=len;
      if (!len)
	len = &k;
      switch (INTVAL (operands[2]))
	{
	default: len = t; break;
	case 1:
	  *len=4;
	  return (AS1 (lsl,%A0) CR_TAB
		  AS1 (rol,%B0) CR_TAB
		  AS1 (rol,%C0) CR_TAB
		  AS1 (rol,%D0));
	case 8:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);
	    *len=4;
	    if (reg0 >= reg1)
	      return (AS2 (mov,%D0,%C1)  CR_TAB
		      AS2 (mov,%C0,%B1)  CR_TAB
		      AS2 (mov,%B0,%A1)  CR_TAB
		      AS1 (clr,%A0));
	    else if (reg0 + 1 == reg1)
	      return *len = 1, AS1 (clr,%A0);
	    else
	      return (AS1 (clr,%A0)      CR_TAB
		      AS2 (mov,%B0,%A1)  CR_TAB
		      AS2 (mov,%C0,%B1)  CR_TAB
		      AS2 (mov,%D0,%C1));
	  }
	case 16:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);
	    *len=4;
	    if (reg0 + 1 >= reg1)
	      return (AS2 (mov,%D0,%B1)  CR_TAB
		      AS2 (mov,%C0,%A1)  CR_TAB
		      AS1 (clr,%B0)      CR_TAB
		      AS1 (clr,%A0));
	    if (reg0 + 2 == reg1)
	      return *len = 2, (AS1 (clr,%B0)      CR_TAB
				AS1 (clr,%A0));
	    else
	      return (AS2 (mov,%C0,%A1)  CR_TAB
		      AS2 (mov,%D0,%B1)  CR_TAB
		      AS1 (clr,%B0)      CR_TAB
		      AS1 (clr,%A0));
	  }
	case 24:
	  *len=4;
	  if (true_regnum (operands[0]) + 3 != true_regnum (operands[1]))
	    return (AS2 (mov,%D0,%A1)  CR_TAB
		    AS1 (clr,%C0)      CR_TAB
		    AS1 (clr,%B0)      CR_TAB
		    AS1 (clr,%A0));
	  else
	    return *len = 3, (AS1 (clr,%C0)      CR_TAB
			      AS1 (clr,%B0)      CR_TAB
			      AS1 (clr,%A0));
	}
    }
  if (len)
    *len = 6;
  out_shift_with_cnt (AS1 (lsl,%0)  CR_TAB
		      AS1 (rol,%B0) CR_TAB
		      AS1 (rol,%C0) CR_TAB
		      AS1 (rol,%D0) CR_TAB
		      AS1 (dec,%3)  CR_TAB
		      AS1 (brne,_PC_-12),
		      insn, operands, len);
  return "";
}

/* 8bit arithmetic shift right  ((signed char)x >> i) */

char *
ashrqi3_out (insn,operands,len)
     rtx insn;
     rtx operands[];
     int *len; /* insn length */
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      int *t=len;
      int k;
      if (!len)
	len = &k;
      switch (INTVAL (operands[2]))
	{
	default: len = t; break;
	case 1:
	  *len=1;
	  return AS1 (asr,%0);
	case 2:
	  *len=2;
	  return (AS1 (asr,%0) CR_TAB
		  AS1 (asr,%0));
	case 3:
	  *len=3;
	  return (AS1 (asr,%0) CR_TAB
		  AS1 (asr,%0) CR_TAB
		  AS1 (asr,%0));
	case 4:
	  *len=4;
	  return (AS1 (asr,%0) CR_TAB
		  AS1 (asr,%0) CR_TAB
		  AS1 (asr,%0) CR_TAB
		  AS1 (asr,%0));
	}
    }
  if (len)
    *len = 3;
  out_shift_with_cnt (AS1 (asr,%0) CR_TAB
		      AS1 (dec,%3) CR_TAB
		      AS1 (brne,_PC_-6),
		      insn, operands, len);
  return "";
}


/* 16bit arithmetic shift right  ((signed short)x >> i) */

char *
ashrhi3_out (insn,operands,len)
     rtx insn;
     rtx operands[];
     int *len;
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      int k;
      int *t=len;
      if (!len)
	len = &k;
      switch (INTVAL (operands[2]))
	{
	default: len = t; break;
	case 1:
	  *len=2;
	  return (AS1 (asr,%B0) CR_TAB
		  AS1 (ror,%A0));
	case 2:
	  *len=4;
	  return (AS1 (asr,%B0)  CR_TAB
		  AS1 (ror,%A0) CR_TAB
		  AS1 (asr,%B0)  CR_TAB
		  AS1 (ror,%A0));
	case 8:
	  if (true_regnum (operands[0]) != true_regnum (operands[1]) + 1)
	    return *len = 4, (AS2 (mov,%A0,%B1) CR_TAB
			      AS1 (clr,%B0)     CR_TAB
			      AS2 (sbrc,%A0,7)  CR_TAB
			      AS1 (dec,%B0));
	  else
	    return *len = 3, (AS1 (clr,%B0)     CR_TAB
			      AS2 (sbrc,%A0,7)  CR_TAB
			      AS1 (dec,%B0));
	}
    }
  if (len)
    *len = 4;
  out_shift_with_cnt (AS1 (asr,%B0) CR_TAB
		      AS1 (ror,%A0) CR_TAB
		      AS1 (dec,%3) CR_TAB
		      AS1 (brne,_PC_-8),
		      insn, operands, len);
  return "";
}


/* 32bit arithmetic shift right  ((signed long)x >> i) */

char *
ashrsi3_out (insn,operands,len)
     rtx insn;
     rtx operands[];
     int *len;
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      int k;
      int *t = len;
      if (!len)
	len = &k;
      switch (INTVAL (operands[2]))
	{
	default: len = t; break;
	case 1:
	  *len=4;
	  return (AS1 (asr,%D0)  CR_TAB
		  AS1 (ror,%C0) CR_TAB
		  AS1 (ror,%B0) CR_TAB
		  AS1 (ror,%A0));
	case 8:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);
	    *len=6;
	    if (reg0 <= reg1)
	      return (AS2 (mov,%A0,%B1) CR_TAB
		      AS2 (mov,%B0,%C1) CR_TAB
		      AS2 (mov,%C0,%D1) CR_TAB
		      AS1 (clr,%D0)     CR_TAB
		      AS2 (sbrc,%C0,7)  CR_TAB
		      AS1 (dec,%D0));
	    else if (reg0 == reg1 + 1)
	      return *len = 3, (AS1 (clr,%D0)     CR_TAB
				AS2 (sbrc,%C0,7)  CR_TAB
				AS1 (dec,%D0));
	    else
	      return (AS1 (clr,%D0)     CR_TAB
		      AS2 (sbrc,%C0,7)  CR_TAB
		      AS1 (dec,%D0)     CR_TAB
		      AS2 (mov,%C0,%D1) CR_TAB
		      AS2 (mov,%B0,%C1) CR_TAB
		      AS2 (mov,%A0,%B1));
	  }
	case 16:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);
	    *len=6;
	    if (reg0 <= reg1 + 1)
	      return (AS2 (mov,%A0,%C1) CR_TAB
		      AS2 (mov,%B0,%D1) CR_TAB
		      AS1 (clr,%D0)     CR_TAB
		      AS2 (sbrc,%B0,7)  CR_TAB
		      AS1 (com,%D0)     CR_TAB
		      AS2 (mov,%C0,%D0));
	    else if (reg0 == reg1 + 2)
	      return *len = 4, (AS1 (clr,%D0)     CR_TAB
				AS2 (sbrc,%B0,7)  CR_TAB
				AS1 (com,%D0)     CR_TAB
				AS2 (mov,%C0,%D0));
	    else
	      return (AS2 (mov,%B0,%D1) CR_TAB
		      AS2 (mov,%A0,%C1) CR_TAB
		      AS1 (clr,%D0)     CR_TAB
		      AS2 (sbrc,%B0,7)  CR_TAB
		      AS1 (com,%D0)     CR_TAB
		      AS2 (mov,%C0,%D0));
	  }
	case 24:
	  if (true_regnum (operands[0]) != true_regnum (operands[1]) + 3)
	    return *len = 6, (AS2 (mov,%A0,%D1) CR_TAB
			      AS1 (clr,%D0)     CR_TAB
			      AS2 (sbrc,%A0,7)  CR_TAB
			      AS1 (com,%D0)     CR_TAB
			      AS2 (mov,%B0,%D0) CR_TAB
			      AS2 (mov,%C0,%D0));
	  else
	    return *len = 5, (AS1 (clr,%D0)     CR_TAB
			      AS2 (sbrc,%A0,7)  CR_TAB
			      AS1 (com,%D0)     CR_TAB
			      AS2 (mov,%B0,%D0) CR_TAB
			      AS2 (mov,%C0,%D0));
	}
    }
  if (len)
    *len = 6;
  out_shift_with_cnt (AS1 (asr,%D0) CR_TAB
		      AS1 (ror,%C0) CR_TAB
		      AS1 (ror,%B0) CR_TAB
		      AS1 (ror,%A0) CR_TAB
		      AS1 (dec,%3) CR_TAB
		      AS1 (brne,_PC_-12),
		      insn, operands, len);
  return "";
}

/* 8bit logic shift right ((unsigned char)x >> i) */

char *
lshrqi3_out (insn,operands,len)
     rtx insn;
     rtx operands[];
     int *len;
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      int k;
      int *t=len;
      if (!len)
	len = &k;
      switch (INTVAL (operands[2]))
	{
	default: len = t; break;
	case 1:
	  *len=1;
	  return AS1 (lsr,%0);
	case 2:
	  *len=2;
	  return (AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0));
	case 3:
	  *len=3;
	  return (AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0));
	case 4:
	  if (TEST_HARD_REG_CLASS (LD_REGS, true_regnum (operands[0])))
	    {
	      *len=2;
	      return (AS1 (swap,%0) CR_TAB
		      AS2 (andi,%0,0x0f));
	    }
	  *len=4;
	  return (AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0));
	case 5:
	  if (TEST_HARD_REG_CLASS (LD_REGS, true_regnum (operands[0])))
	    {
	      *len=3;
	      return (AS1 (swap,%0) CR_TAB
		      AS1 (lsr,%0)  CR_TAB
		      AS2 (andi,%0,0x7));
	    }
	  *len=5;
	  return (AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0));
	case 6:
	  if (TEST_HARD_REG_CLASS (LD_REGS, true_regnum (operands[0])))
	    {
	      *len=4;
	      return (AS1 (swap,%0) CR_TAB
		      AS1 (lsr,%0)  CR_TAB
		      AS1 (lsr,%0)  CR_TAB
		      AS2 (andi,%0,0x3));
	    }
	  *len=6;
	  return (AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0));
	case 7:
	  *len=3;
	  return (AS1 (rol,%0) CR_TAB
		  AS1 (clr,%0) CR_TAB
		  AS1 (rol,%0));
	}
    }
  if (len)
    *len = 3;
  out_shift_with_cnt (AS1 (lsr,%0) CR_TAB
		      AS1 (dec,%3) CR_TAB
		      AS1 (brne,_PC_-6),
		      insn, operands, len);
  return "";
}

/* 16bit logic shift right ((unsigned short)x >> i) */

char *
lshrhi3_out (insn,operands,len)
     rtx insn;
     rtx operands[];
     int *len;
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      int k;
      int *t=len;
      if (!len)
	len = &k;
      switch (INTVAL (operands[2]))
	{
	default: len = t; break;
	case 1:
	  *len=2;
	  return (AS1 (lsr,%B0) CR_TAB
		  AS1 (ror,%A0));
	case 2:
	  *len=4;
	  return (AS1 (lsr,%B0)  CR_TAB
		  AS1 (ror,%A0)  CR_TAB
		  AS1 (lsr,%B0)  CR_TAB
		  AS1 (ror,%A0));
	case 8:
	  if (true_regnum (operands[0]) != true_regnum (operands[1]) + 1)
	    return *len = 2, (AS2 (mov,%A0,%B1) CR_TAB
			      AS1 (clr,%B0));
	  else
	    return *len = 1, AS1 (clr,%B0);
	    
	}
    }
  if (len)
    *len = 4;
  out_shift_with_cnt (AS1 (lsr,%B0) CR_TAB
		      AS1 (ror,%A0) CR_TAB
		      AS1 (dec,%3) CR_TAB
		      AS1 (brne,_PC_-8),
		      insn, operands, len);
  return "";
}

/* 32bit logic shift right ((unsigned int)x >> i) */

char *
lshrsi3_out (insn,operands,len)
     rtx insn;
     rtx operands[];
     int *len;
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      int k;
      int *t=len;
      if (!len)
	len = &k;
      switch (INTVAL (operands[2]))
	{
	default: len = t; break;
	case 1:
	  *len=4;
	  return (AS1 (lsr,%D0)  CR_TAB
		  AS1 (ror,%C0) CR_TAB
		  AS1 (ror,%B0) CR_TAB
		  AS1 (ror,%A0));
	case 8:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);
	    *len=4;
	    if (reg0 <= reg1)
	      return (AS2 (mov,%A0,%B1) CR_TAB
		      AS2 (mov,%B0,%C1) CR_TAB
		      AS2 (mov,%C0,%D1) CR_TAB
		      AS1 (clr,%D0));
	    else if (reg0 == reg1 + 1)
	      return *len = 1, AS1 (clr,%D0);
	    else
	      return (AS1 (clr,%D0)     CR_TAB
		      AS2 (mov,%C0,%D1) CR_TAB
		      AS2 (mov,%B0,%C1) CR_TAB
		      AS2 (mov,%A0,%B1)); 
	  }
	case 16:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);
	    *len=4;
	    if (reg0 <= reg1 + 1)
	      return (AS2 (mov,%A0,%C1) CR_TAB
		      AS2 (mov,%B0,%D1) CR_TAB
		      AS1 (clr,%C0)     CR_TAB
		      AS1 (clr,%D0));
	    else if (reg0 == reg1 + 2)
	      return *len = 2, (AS1 (clr,%C0)     CR_TAB
				AS1 (clr,%D0));
	    else
	      return (AS2 (mov,%B0,%D1) CR_TAB
		      AS2 (mov,%A0,%C1) CR_TAB
		      AS1 (clr,%C0)     CR_TAB
		      AS1 (clr,%D0));
	  }
	case 24:
	  if (true_regnum (operands[0]) != true_regnum (operands[1]) + 3)
	    return *len = 4, (AS2 (mov,%A0,%D1) CR_TAB
			      AS1 (clr,%B0)     CR_TAB
			      AS1 (clr,%C0)     CR_TAB
			      AS1 (clr,%D0));
	  else
	    return *len = 3, (AS1 (clr,%B0)     CR_TAB
			      AS1 (clr,%C0)     CR_TAB
			      AS1 (clr,%D0));
	}
    }
  if (len)
    *len = 6;
  out_shift_with_cnt (AS1 (lsr,%D0) CR_TAB
		      AS1 (ror,%C0) CR_TAB
		      AS1 (ror,%B0) CR_TAB
		      AS1 (ror,%A0) CR_TAB
		      AS1 (dec,%3) CR_TAB
		      AS1 (brne,_PC_-12),
		      insn, operands, len);
  return "";
}

/* Modifies the length assigned to instruction INSN
 LEN is the initially computed length of the insn.  */

int
adjust_insn_length (insn,len)
     rtx insn;
     int len;
{
  rtx patt = PATTERN (insn);
  rtx set;
  if (GET_CODE (patt) == SET)
    {
      rtx op[10];
      op[1] = SET_SRC (patt);
      op[0] = SET_DEST (patt);
      if (REG_P (op[0]) && GET_CODE (op[1]) == MEM)
        {
	  if (CONSTANT_ADDRESS_P (XEXP (op[1],0)))
	    switch (GET_MODE (op[0]))
	      {
	      case QImode: len = 2; break;
	      case HImode: len = 4; break;
	      case SImode:
	      case SFmode: len = 8; break;
	      default: break; 
	      }
	  else
	    switch (GET_MODE (op[0]))
	      {
	      case QImode: out_movqi_r_mr (insn,op,&len); break;
	      case HImode: out_movhi_r_mr (insn,op,&len); break;
	      case SImode:
	      case SFmode: out_movsi_r_mr (insn,op,&len); break;
	      default: break;
	      }
        }
      else if ((REG_P (op[1]) || const0_rtx == op[1])
	       && GET_CODE (op[0]) == MEM)
        {
	  if (CONSTANT_ADDRESS_P (XEXP (op[0],0)))
	    switch (GET_MODE (op[0]))
	      {
	      case QImode: len = 2; break;
	      case HImode: len = 4; break;
	      case SImode:
	      case SFmode: len = 8; break;
	      default: break;
	      }
	  else if (GET_CODE (XEXP (op[0],0)) != POST_DEC)
	    switch (GET_MODE (op[0]))
	      {
	      case QImode: out_movqi_mr_r (insn,op,&len); break;
	      case HImode: out_movhi_mr_r (insn,op,&len); break;
	      case SImode:
	      case SFmode: out_movsi_mr_r (insn,op,&len); break;
	      default: break;
	      }
        }
      else if (op[0] == cc0_rtx && REG_P (op[1]))
	{
	  switch (GET_MODE (op[1]))
	    {
	    case HImode: out_tsthi (insn,&len); break;
	    case SImode: out_tstsi (insn,&len); break;
	    default: break;
	    }
	}
      else if (GET_CODE (op[1]) == AND)
	{
	  if (GET_CODE (XEXP (op[1],1)) == CONST_INT)
	    {
	      HOST_WIDE_INT mask = INTVAL (XEXP (op[1],1));
	      if (GET_MODE (op[1]) == SImode)
		len = (((mask & 0xff) != 0xff)
		       + ((mask & 0xff00) != 0xff00)
		       + ((mask & 0xff0000UL) != 0xff0000UL)
		       + ((mask & 0xff000000UL) != 0xff000000UL));
	      else if (GET_MODE (op[1]) == HImode)
		len = (((mask & 0xff) != 0xff)
		       + ((mask & 0xff00) != 0xff00));
	    }
	}
      else if (GET_CODE (op[1]) == IOR)
	{
	  if (GET_CODE (XEXP (op[1],1)) == CONST_INT)
	    {
	      HOST_WIDE_INT mask = INTVAL (XEXP (op[1],1));
	      if (GET_MODE (op[1]) == SImode)
		len = (((mask & 0xff) == 0)
		       + ((mask & 0xff00) == 0)
		       + ((mask & 0xff0000UL) == 0)
		       + ((mask & 0xff000000UL) ==0));
	      else if (GET_MODE (op[1]) == HImode)
		len = (((mask & 0xff) == 0)
		       + ((mask & 0xff00) == 0));
	    }
	}
    }
  set = single_set (insn);
  if (set)
    {
      rtx op[10];
      op[1] = SET_SRC (set);
      op[0] = SET_DEST (set);
      if (GET_CODE (op[1]) == ASHIFT
	  || GET_CODE (op[1]) == ASHIFTRT
	  || GET_CODE (op[1]) == LSHIFTRT)
	{
	  rtx ops[10];
	  ops[0] = op[0];
	  ops[1] = XEXP (op[1],0);
	  ops[2] = XEXP (op[1],1);
	  switch (GET_CODE (op[1]))
	    {
	    case ASHIFT:
	      switch (GET_MODE (op[0]))
		{
		case QImode: ashlqi3_out (insn,ops,&len); break;
		case HImode: ashlhi3_out (insn,ops,&len); break;
		case SImode: ashlsi3_out (insn,ops,&len); break;
		default: break;
		}
	      break;
	    case ASHIFTRT:
	      switch (GET_MODE (op[0]))
		{
		case QImode: ashrqi3_out (insn,ops,&len); break;
		case HImode: ashrhi3_out (insn,ops,&len); break;
		case SImode: ashrsi3_out (insn,ops,&len); break;
		default: break;
		}
	      break;
	    case LSHIFTRT:
	      switch (GET_MODE (op[0]))
		{
		case QImode: lshrqi3_out (insn,ops,&len); break;
		case HImode: lshrhi3_out (insn,ops,&len); break;
		case SImode: lshrsi3_out (insn,ops,&len); break;
		default: break;
		}
	      break;
	    default:
	      break;
	    }
	}
    }
  return len;
}

/* Return non-zero if register REG dead after INSN */

int
reg_unused_after (insn, reg)
     rtx insn;
     rtx reg;
{
  return (0
	  /* In egcs 1.1.x dead_or_set_p give buggy result after reload 
#ifdef PRESERVE_DEATH_INFO_REGNO_P
	  || dead_or_set_p (insn,reg)
#endif
	  */
	  
	  || (REG_P(reg) && _reg_unused_after (insn, reg)));
}

/* Return non-zero if REG is not used after INSN.
   We assume REG is a reload reg, and therefore does
   not live past labels.  It may live past calls or jumps though.  */

int
_reg_unused_after (insn, reg)
     rtx insn;
     rtx reg;
{
  enum rtx_code code;
  rtx set;

  /* If the reg is set by this instruction, then it is safe for our
     case.  Disregard the case where this is a store to memory, since
     we are checking a register used in the store address.  */
  set = single_set (insn);
  if (set && GET_CODE (SET_DEST (set)) != MEM
      && reg_overlap_mentioned_p (reg, SET_DEST (set)))
    return 1;

  while ((insn = NEXT_INSN (insn)))
    {
      code = GET_CODE (insn);

#if 0
      /* If this is a label that existed before reload, then the register
	 if dead here.  However, if this is a label added by reorg, then
	 the register may still be live here.  We can't tell the difference,
	 so we just ignore labels completely.  */
      if (code == CODE_LABEL)
	return 1;
      /* else */
#endif

      if (code == JUMP_INSN)
	return 0;

      /* If this is a sequence, we must handle them all at once.
	 We could have for instance a call that sets the target register,
	 and a insn in a delay slot that uses the register.  In this case,
	 we must return 0.  */
      else if (code == INSN && GET_CODE (PATTERN (insn)) == SEQUENCE)
	{
	  int i;
	  int retval = 0;

	  for (i = 0; i < XVECLEN (PATTERN (insn), 0); i++)
	    {
	      rtx this_insn = XVECEXP (PATTERN (insn), 0, i);
	      rtx set = single_set (this_insn);

	      if (GET_CODE (this_insn) == CALL_INSN)
		code = CALL_INSN;
	      else if (GET_CODE (this_insn) == JUMP_INSN)
		{
		  if (INSN_ANNULLED_BRANCH_P (this_insn))
		    return 0;
		  code = JUMP_INSN;
		}

	      if (set && reg_overlap_mentioned_p (reg, SET_SRC (set)))
		return 0;
	      if (set && reg_overlap_mentioned_p (reg, SET_DEST (set)))
		{
		  if (GET_CODE (SET_DEST (set)) != MEM)
		    retval = 1;
		  else
		    return 0;
		}
	      if (set == 0
		  && reg_overlap_mentioned_p (reg, PATTERN (this_insn)))
		return 0;
	    }
	  if (retval == 1)
	    return 1;
	  else if (code == JUMP_INSN)
	    return 0;
	}

      if (code == CALL_INSN)
	{
	  rtx tem;
	  for (tem = CALL_INSN_FUNCTION_USAGE (insn); tem; tem = XEXP (tem, 1))
	    if (GET_CODE (XEXP (tem, 0)) == USE
		&& REG_P (XEXP (XEXP (tem, 0), 0))
		&& reg_overlap_mentioned_p (reg, XEXP (XEXP (tem, 0), 0)))
	      return 0;
	  if (call_used_regs[REGNO (reg)]) 
	    return 1;
	}

      if (GET_RTX_CLASS (code) == 'i')
	{
	  rtx set = single_set (insn);

	  if (set && reg_overlap_mentioned_p (reg, SET_SRC (set)))
	    return 0;
	  if (set && reg_overlap_mentioned_p (reg, SET_DEST (set)))
	    return GET_CODE (SET_DEST (set)) != MEM;
	  if (set == 0 && reg_overlap_mentioned_p (reg, PATTERN (insn)))
	    return 0;
	}
    }
  return 1;
}

/* Output rtx VALUE as .byte to file FILE */

void
asm_output_char(file,value)
     FILE *file;
     rtx value;
{
  fprintf (file, "\t.byte ");
  output_addr_const (file, value);
  fprintf (file, "\n");
}


/* Output VALUE as .byte to file FILE */

void
asm_output_byte (file,value)
     FILE *file;
     char value;
{
  fprintf (file, "\t.byte 0x%x\n",value & 0xff);
}


/* Output rtx VALUE as .word to file FILE */

void
asm_output_short (file, value)
     FILE *file;
     rtx value;
{
  if (SYMBOL_REF_FLAG (value) || GET_CODE (value) == LABEL_REF)
    {
      fprintf (file, "\t.word pm(");
      output_addr_const (file, (value));
      fprintf (file, ")\n");
    }
  else
    {
      fprintf (file, "\t.word ");
      output_addr_const (file, (value));
      fprintf (file, "\n");
    }
}


/* Output real N to file FILE */

void
asm_output_float (file, n)
     FILE *file;
     REAL_VALUE_TYPE n;
{
  long val;
  char dstr[100];
  
  REAL_VALUE_TO_TARGET_SINGLE (n, val);
  REAL_VALUE_TO_DECIMAL (n, "%g", dstr);
  fprintf (file, "\t.long 0x%08lx\t/* %s */\n",val, dstr);
}

/* Sets section name for declaration DECL */
  
void
unique_section (decl, reloc)
     tree decl;
     int reloc ATTRIBUTE_UNUSED;
{
  int len;
  char *name,*string;
  char *prefix;
  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
  /* Strip off any encoding in name.  */
  STRIP_NAME_ENCODING (name, name);

  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      if (flag_function_sections)
	prefix = ".text.";
      else
	prefix = ".text";
    }
  else 
    fatal ("Strange situation: unique section is not a FUNCTION_DECL");

  if (flag_function_sections)
    {
      len = strlen (name) + strlen (prefix);
      string = alloca (len + 1);
      sprintf (string, "%s%s", prefix, name);
      DECL_SECTION_NAME (decl) = build_string (len, string);
    }
}


/* Output section name to file FILE */

void
asm_output_section_name(file, decl, name, reloc)
     FILE *file;
     tree decl ATTRIBUTE_UNUSED;
     const char *name;
     int reloc ATTRIBUTE_UNUSED;
{
  fprintf (file, ".section %s\n", name);
}


/* The routine used to output NUL terminated strings.  We use a special
   version of this for most svr4 targets because doing so makes the
   generated assembly code more compact (and thus faster to assemble)
   as well as more readable, especially for targets like the i386
   (where the only alternative is to output character sequences as
   comma separated lists of numbers).   */

void
gas_output_limited_string(file, str)
     FILE * file ATTRIBUTE_UNUSED;
     char * str;
{
  unsigned char *_limited_str = (unsigned char *) str;
  unsigned ch;
  fprintf (file, "\t%s\t\"", STRING_ASM_OP);
  for (; (ch = *_limited_str); _limited_str++)
    {
      int escape;
      switch (escape = ESCAPES[ch])
	{
	case 0:
	  putc (ch, file);
	  break;
	case 1:
	  fprintf (file, "\\%03o", ch);
	  break;
	default:
	  putc ('\\', file);
	  putc (escape, file);
	  break;
	}
    }
  fprintf (file, "\"\n");
}

/* The routine used to output sequences of byte values.  We use a special
   version of this for most svr4 targets because doing so makes the
   generated assembly code more compact (and thus faster to assemble)
   as well as more readable.  Note that if we find subparts of the
   character sequence which end with NUL (and which are shorter than
   STRING_LIMIT) we output those using ASM_OUTPUT_LIMITED_STRING.  */

void
gas_output_ascii(file, str, length)
     FILE * file;
     char * str;
     size_t length;
{
  unsigned char *_ascii_bytes = (unsigned char *) str;
  unsigned char *limit = _ascii_bytes + length;
  unsigned bytes_in_chunk = 0;
  for (; _ascii_bytes < limit; _ascii_bytes++)
    {
      register unsigned char *p;
      if (bytes_in_chunk >= 60)
	{
	  fprintf (file, "\"\n");
	  bytes_in_chunk = 0;
	}
      for (p = _ascii_bytes; p < limit && *p != '\0'; p++)
	continue;
      if (p < limit && (p - _ascii_bytes) <= (signed)STRING_LIMIT)
	{
	  if (bytes_in_chunk > 0)
	    {
	      fprintf (file, "\"\n");
	      bytes_in_chunk = 0;
	    }
	  gas_output_limited_string (file, _ascii_bytes);
	  _ascii_bytes = p;
	}
      else
	{
	  int escape;
	  unsigned ch;
	  if (bytes_in_chunk == 0)
	    fprintf (file, "\t.ascii\t\"");
	  switch (escape = ESCAPES[ch = *_ascii_bytes])
	    {
	    case 0:
	      putc (ch, file);
	      bytes_in_chunk++;
	      break;
	    case 1:
	      fprintf (file, "\\%03o", ch);
	      bytes_in_chunk += 4;
	      break;
	    default:
	      putc ('\\', file);
	      putc (escape, file);
	      bytes_in_chunk += 2;
	      break;
	    }
	}
    }
  if (bytes_in_chunk > 0)
    fprintf (file, "\"\n");
}

/* Return value is nonzero if pseudos that have been
   assigned to registers of class CLASS would likely be spilled
   because registers of CLASS are needed for spill registers.  */

enum reg_class
class_likely_spilled_p(int c)
{
  return (c != ALL_REGS && c != ADDW_REGS);
}

/* Only `progmem' attribute valid for type.  */

int
valid_machine_type_attribute(type, attributes, identifier, args)
     tree type ATTRIBUTE_UNUSED;
     tree attributes ATTRIBUTE_UNUSED;
     tree identifier;
     tree args ATTRIBUTE_UNUSED;
{
  return is_attribute_p ("progmem", identifier);
}

/* If IDENTIFIER with arguments ARGS is a valid machine specific
   attribute for DECL return 1.
   Valid attributes:
   progmem - put data to program memory;
   signal - make a function to be hardware interrupt. After function
   epilogue interrupts are disabled;
   interrupt - make a function to be hardware interrupt. After function
   epilogue interrupts are enabled;
   naked     - don't generate function prologue/epilogue and `ret' command.  */

int
valid_machine_decl_attribute (decl, attributes, attr, args)
     tree decl;
     tree attributes ATTRIBUTE_UNUSED;
     tree attr;
     tree args ATTRIBUTE_UNUSED;
{
  if (is_attribute_p ("interrupt", attr)
      || is_attribute_p ("signal", attr)
      || is_attribute_p ("naked", attr))
    return TREE_CODE (decl) == FUNCTION_DECL;

  if (is_attribute_p ("progmem", attr)
      && (TREE_STATIC (decl) || DECL_EXTERNAL (decl)))
    {
      if (DECL_INITIAL (decl) == NULL_TREE)
	{
	  warning ("Only initialized variables can be placed into "
		   "program memory area.");
	  return 0;
	}
      return 1;
    }
  return 0;
}


/* Look for attribute `progmem' in DECL
   founded - 1 otherwise 0 */

int
avr_progmem_p (decl)
     tree decl;
{
  tree a;

  if (TREE_CODE (decl) != VAR_DECL)
    return 0;

  if (NULL_TREE
      != lookup_attribute ("progmem", DECL_MACHINE_ATTRIBUTES (decl)))
    return 1;

  a=decl;
  do
    a = TREE_TYPE(a);
  while (TREE_CODE (a) == ARRAY_TYPE);

  if (NULL_TREE != lookup_attribute ("progmem", TYPE_ATTRIBUTES (a)))
    return 1;
  
  return 0;
}

/* Encode section information about tree DECL */
  
void
encode_section_info (decl)
     tree decl;
{
  if (TREE_CODE (decl) == FUNCTION_DECL)
    SYMBOL_REF_FLAG (XEXP (DECL_RTL (decl), 0)) = 1;

  if ((TREE_STATIC (decl) || DECL_EXTERNAL (decl))
      && TREE_CODE (decl) == VAR_DECL
      && avr_progmem_p (decl))
    {
      char * dsec = ".progmem.data";
      DECL_SECTION_NAME (decl) = build_string (strlen (dsec), dsec);
    }
}   

/* Outputs to the stdio stream FILE some
   appropriate text to go at the start of an assembler file.  */

void
asm_file_start (file)
     FILE *file;
{
  output_file_directive (file, main_input_filename);
  fprintf (file, "\t.arch %s\n", avr_mcu_type->name);
  fputs ("__SREG__ = 0x3f\n"
	 "__SP_H__ = 0x3e\n"
	 "__SP_L__ = 0x3d\n", file);
  
  if (avr_ram_end)
    initial_stack = avr_ram_end;
  else
    {
      static char buf[30];
      initial_stack = buf;
      sprintf (buf, "0x%x", avr_mcu_type->stack);
    }
  
  fputs ("__tmp_reg__ = r0\n" 
	 "__zero_reg__ = r1\n"
	 "_PC_ = 2\n", file);
  
  commands_in_file = 0;
  commands_in_prologues = 0;
  commands_in_epilogues = 0;
}

/* Outputs to the stdio stream FILE some
   appropriate text to go at the end of an assembler file.  */

void
asm_file_end (file)
     FILE *file;
{
  fprintf (file,
	   "/* File %s: code %4d (%4d), prologues %3d, epilogues %3d */\n",
	   main_input_filename,
	   commands_in_file,
	   commands_in_file - commands_in_prologues - commands_in_epilogues,
	   commands_in_prologues, commands_in_epilogues);
}

/* Choose the order in which to allocate hard registers for
   pseudo-registers local to a basic block.

   Store the desired register order in the array `reg_alloc_order'.
   Element 0 should be the register to allocate first; element 1, the
   next register; and so on.  */

void
order_regs_for_local_alloc (void)
{
  unsigned int i;
  int order_0[] = {
    24,25,
    18,19,
    20,21,
    22,23,
    30,31,
    26,27,
    28,29,
    17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,
    0,1,
    32,33,34,35
  };
  int order_1[] = {
    18,19,
    20,21,
    22,23,
    24,25,
    30,31,
    26,27,
    28,29,
    17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,
    0,1,
    32,33,34,35
  };
  int order_2[] = {
    25,24,
    23,22,
    21,20,
    19,18,
    30,31,
    26,27,
    28,29,
    17,16,
    15,14,13,12,11,10,9,8,7,6,5,4,3,2,
    1,0,
    32,33,34,35
  };
  
  int *order = (TARGET_ORDER_1 ? order_1 :
		TARGET_ORDER_2 ? order_2 :
		order_0);
  for (i=0; i < sizeof (order_0) / sizeof (order_0[0]); ++i)
      reg_alloc_order[i] = order[i];
}

/* Calculate the cost of X code of the expression in which it is contained,
   found in OUTER_CODE */

int
default_rtx_costs (X, code, outer_code)
     rtx X;
     enum rtx_code code;
     enum rtx_code outer_code;
{
  int cost=0;
  switch (code)
    {
    case SYMBOL_REF:
    case LABEL_REF:
      cost = 2 * GET_MODE_SIZE (GET_MODE (X));
      break;
    case MEM:
      if (outer_code != SET)
	cost = 1;
      if (GET_CODE (XEXP (X,0)) == SYMBOL_REF)
	cost += 2 * GET_MODE_SIZE (GET_MODE (X));
      else
	cost += GET_MODE_SIZE (GET_MODE (X));
      break;
    case CONST_INT:
      cost = 0;
      break;
    case SIGN_EXTEND:
      if (outer_code == SET)
	cost = GET_MODE_SIZE (GET_MODE (X));
      else
	cost = -GET_MODE_SIZE (GET_MODE (X));
      break;
    case ZERO_EXTEND:
      if (outer_code == SET)
	cost = GET_MODE_SIZE (GET_MODE (X));
      else
	cost = -1;
      break;
    case PLUS:
    case MINUS:
      if (outer_code == SET)
	{
	  if (X == stack_pointer_rtx)
	    cost = -10;
	  else if (GET_CODE (XEXP (X,1)) == CONST_INT)
	    cost = (INTVAL (XEXP (X,1)) <= 63 ? 1 :
		     GET_MODE_SIZE (GET_MODE (X)));
	  else
	    cost = GET_MODE_SIZE (GET_MODE (X));
	}
      break;
    case COMPARE:
      if (GET_CODE (XEXP (X,1)) == CONST_INT)
	cost = GET_MODE_SIZE (GET_MODE (XEXP (X,0)));
      break;
    default:
      break;
    }
  return cost;
}

/* Calculate the cost of a memory address */

int
address_cost (rtx x)
{
  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x,1)) == CONST_INT
      && (REG_P (XEXP (x,0)) || GET_CODE (XEXP (x,0)) == SUBREG)
      && INTVAL (XEXP (x,1)) >= 61)
    return 18;
  if (CONSTANT_ADDRESS_P (x))
    return 4;
  return 4;
}

/*  EXTRA_CONSTRAINT helper */

int
extra_constraint (x,c)
     rtx x;
     char c;
{
  if (c == 'Q'
      && GET_CODE (x) == MEM
      && GET_CODE (XEXP (x,0)) == PLUS)
    {
	  if (TARGET_ALL_DEBUG)
	    {
	      fprintf (stderr, ("extra_constraint:\n"
				"reload_completed: %d\n"
				"reload_in_progress: %d\n"),
		       reload_completed, reload_in_progress);
	      debug_rtx (x);
	    }
      if (GET_CODE (x) == MEM
	  && GET_CODE (XEXP (x,0)) == PLUS
	  && REG_P (XEXP (XEXP (x,0), 0))
	  && GET_CODE (XEXP (XEXP (x,0), 1)) == CONST_INT
	  && (INTVAL (XEXP (XEXP (x,0), 1))
	      <= (64 - GET_MODE_SIZE (GET_MODE (x)))))
	{
	  rtx xx = XEXP (XEXP (x,0), 0);
	  int regno = REGNO (xx);
	  if (TARGET_ALL_DEBUG)
	    {
	      fprintf (stderr, ("extra_constraint:\n"
				"reload_completed: %d\n"
				"reload_in_progress: %d\n"),
		       reload_completed, reload_in_progress);
	      debug_rtx (x);
	    }
	  if (regno >= FIRST_PSEUDO_REGISTER)
	    return 1;		/* allocate pseudos */
	  else if (regno == REG_Z || regno == REG_Y)
	    return 1;		/* strictly check */
	  else if (xx == frame_pointer_rtx
		   || xx == arg_pointer_rtx)
	    return 1;		/* XXX frame & arg pointer checks */
	}
    }
  return 0;
}

/* Convert condition code CONDITION to the valid AVR condition code */

RTX_CODE
avr_normalize_condition (condition)
     RTX_CODE condition;
{
  switch (condition)
    {
    case GT:
      return GE;
    case GTU:
      return GEU;
    case LE:
      return LT;
    case LEU:
      return LTU;
    default:
      fatal ("Wrong condition: %s", GET_RTX_NAME (condition));
    }
}

/* This fnction optimizes conditional jumps */

void
machine_dependent_reorg (first_insn)
     rtx first_insn;
{
  rtx insn, pattern;
  CC_STATUS_INIT;
  
  for (insn = first_insn; insn; insn = NEXT_INSN (insn))
    {
      if (! (insn == 0 || GET_CODE (insn) == INSN
	     || GET_CODE (insn) == CALL_INSN || GET_CODE (insn) == JUMP_INSN)
	  || !single_set (insn))
	continue;

      pattern = PATTERN (insn);

      cc_prev_status = cc_status;
      NOTICE_UPDATE_CC (pattern, insn);
      
      if (GET_CODE (pattern) == PARALLEL)
	pattern = XVECEXP (pattern, 0, 0);
      if (GET_CODE (pattern) == SET
	  && SET_DEST (pattern) == cc0_rtx
	  && compare_diff_p (insn))
	{
	  if (GET_CODE (SET_SRC (pattern)) == COMPARE)
	    {
	      /* Now we work under compare insn */
	      
	      pattern = SET_SRC (pattern);
	      if (true_regnum (XEXP (pattern,0)) >= 0
		  && true_regnum (XEXP (pattern,1)) >= 0 )
		{
		  rtx x = XEXP (pattern,0);
		  rtx next = next_real_insn (insn);
		  rtx pat = PATTERN (next);
		  rtx src = SET_SRC (pat);
		  rtx t = XEXP (src,0);
		  PUT_CODE (t, swap_condition (GET_CODE (t)));
		  XEXP (pattern,0) = XEXP (pattern,1);
		  XEXP (pattern,1) = x;
		  INSN_CODE (next) = -1;
		}
	      else if (true_regnum (XEXP (pattern,0)) >= 0
		       && GET_CODE (XEXP (pattern,1)) == CONST_INT)
		{
		  rtx x = XEXP (pattern,1);
		  rtx next = next_real_insn (insn);
		  rtx pat = PATTERN (next);
		  rtx src = SET_SRC (pat);
		  rtx t = XEXP (src,0);

		  if (avr_simplify_comparision_p (GET_MODE (XEXP (pattern,0)),
						  GET_CODE (t), x))
		    {
		      XEXP (pattern,1) = GEN_INT (INTVAL (x)+1);
		      PUT_CODE (t, avr_normalize_condition (GET_CODE (t)));
		      INSN_CODE (next) = -1;
		      INSN_CODE (insn) = -1;
		    }
		}
	    }
	  else if (true_regnum (SET_SRC (pattern)) >= 0)
	    {
	      /* This is a tst insn */
	      rtx next = next_real_insn (insn);
	      rtx pat = PATTERN (next);
	      rtx src = SET_SRC (pat);
	      rtx t = XEXP (src,0);

	      if (!(cc_prev_status.value1 != 0 && cc_status.value1 != 0
		     && rtx_equal_p (cc_status.value1, cc_prev_status.value1)))
		  {
		    PUT_CODE (t, swap_condition (GET_CODE (t)));
		    SET_SRC (pattern) = gen_rtx (NEG,
						 GET_MODE (SET_SRC (pattern)),
						 SET_SRC (pattern));
		    INSN_CODE (next) = -1;
		    INSN_CODE (insn) = -1;
		  }
	    }
	}
    }
}

/* Returns register number for function return value.*/

int
avr_ret_register (void)
{
  return 24;
}

/* Ceate an RTX representing the place where a
   library function returns a value of mode MODE.  */

rtx
avr_libcall_value (mode)
     enum machine_mode mode;
{
  int offs = GET_MODE_SIZE (mode);
  if (offs < 2)
    offs = 2;
  return gen_rtx (REG, mode, RET_REGISTER + 2 - offs);
}

/* Create an RTX representing the place where a
   function returns a value of data type VALTYPE.  */

rtx
avr_function_value (type,func)
     tree type;
     tree func ATTRIBUTE_UNUSED;
{
  int offs;
  if (TYPE_MODE (type) != BLKmode)
    return avr_libcall_value (TYPE_MODE (type));
  
  offs = int_size_in_bytes (type);
  if (offs < 2)
    offs = 2;
  if (offs > 2 && offs < GET_MODE_SIZE (SImode))
    offs = GET_MODE_SIZE (SImode);
  else if (offs > GET_MODE_SIZE (SImode) && offs < GET_MODE_SIZE (DImode))
    offs = GET_MODE_SIZE (DImode);
  
  return gen_rtx (REG, BLKmode, RET_REGISTER + 2 - offs);
}

/* Returns non-zero if number MASK have only one setted bit */

int
mask_one_bit_p (mask)
     HOST_WIDE_INT mask;
{
  int i;
  unsigned HOST_WIDE_INT n=mask;
  for (i = 0; i < 32; ++i)
    {
      if (n & 0x80000000UL)
	{
	  if (n & 0x7fffffffUL)
	    return 0;
	  else
	    return 32-i;
	}
      n<<=1;
    }
  return 0; 
}


/* Places additional restrictions on the register class to
   use when it is necessary to copy value X into a register
   in class CLASS.  */

enum reg_class
preferred_reload_class(x,class)
     rtx x;
     enum reg_class class;
{
  if (GET_CODE (x) == CONST_INT && INTVAL (x) == 0)
    return class;
  if (CONSTANT_P (x) && (class == NO_LD_REGS
  			 || class == ALL_REGS
			 || class == GENERAL_REGS))
    {
      return LD_REGS;
    }
  return class;
}

void
debug_hard_reg_set (HARD_REG_SET set)
{
  int i;
  for (i=0; i < FIRST_PSEUDO_REGISTER; ++i)
    {
      if (TEST_HARD_REG_BIT (set, i))
	{
	  fprintf (stderr, "r%-2d ", i);
	}
    }
  fprintf (stderr, "\n");
}
	     
