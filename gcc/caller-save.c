/* Save and restore call-clobbered registers which are live across a call.
   Copyright (C) 1989, 1992 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "config.h"
#include "rtl.h"
#include "insn-config.h"
#include "flags.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "recog.h"
#include "basic-block.h"
#include "reload.h"
#include "expr.h"

/* A mode for each hard register that we can save.  This mode is wide enough
   to save the entire contents of the register and will be used whenever the
   register must be saved because it is live.  */

static enum machine_mode regno_save_mode[FIRST_PSEUDO_REGISTER];

/* For each hard register, a place on the stack where it can be saved,
   if needed.  */

static rtx regno_save_mem[FIRST_PSEUDO_REGISTER];

/* We will only make a register eligible for caller-save if it can be
   saved in its widest mode with a simple SET insn as long as the memory
   address is valid.  We record the INSN_CODE is those insns here since
   when we emit them, the addresses might not be valid, so they might not
   be recognized.  */

static enum insn_code reg_save_code[FIRST_PSEUDO_REGISTER];
static enum insn_code reg_restore_code[FIRST_PSEUDO_REGISTER];

/* Set of hard regs currently live (during scan of all insns).  */

static HARD_REG_SET hard_regs_live;

/* Set of hard regs currently residing in save area (during insn scan).  */

static HARD_REG_SET hard_regs_saved;

/* Number of registers currently in hard_regs_saved.  */

int n_regs_saved;

static void set_reg_live ();
static void clear_reg_live ();
static void restore_referenced_regs ();
static void insert_save_restore ();

/* Return a machine mode that is legitimate for hard reg REGNO and large
   enough to save the whole register.  If we can't find one, 
   return VOIDmode.  */

static enum machine_mode
choose_hard_reg_mode (regno)
     int regno;
{
  enum machine_mode found_mode = VOIDmode, mode;

  /* We first look for the largest integer mode that can be validly
     held in REGNO.  If none, we look for the largest floating-point mode.
     If we still didn't find a valid mode, try CCmode.  */

  for (mode = GET_CLASS_NARROWEST_MODE (MODE_INT); mode != VOIDmode;
       mode = GET_MODE_WIDER_MODE (mode))
    if (HARD_REGNO_NREGS (regno, mode) == 1
	&& HARD_REGNO_MODE_OK (regno, mode))
      found_mode = mode;

  if (found_mode != VOIDmode)
    return found_mode;

  for (mode = GET_CLASS_NARROWEST_MODE (MODE_FLOAT); mode != VOIDmode;
       mode = GET_MODE_WIDER_MODE (mode))
    if (HARD_REGNO_NREGS (regno, mode) == 1
	&& HARD_REGNO_MODE_OK (regno, mode))
      found_mode = mode;

  if (found_mode != VOIDmode)
    return found_mode;

  if (HARD_REGNO_NREGS (regno, CCmode) == 1
      && HARD_REGNO_MODE_OK (regno, CCmode))
    return CCmode;

  /* We can't find a mode valid for this register.  */
  return VOIDmode;
}

/* Initialize for caller-save.

   Look at all the hard registers that are used by a call and for which
   regclass.c has not already excluded from being used across a call.

   Ensure that we can find a mode to save the register and that there is a 
   simple insn to save and restore the register.  This latter check avoids
   problems that would occur if we tried to save the MQ register of some
   machines directly into memory.  */

void
init_caller_save ()
{
  char *first_obj = (char *) oballoc (0);
  rtx addr_reg;
  int offset;
  rtx address;
  int i;

  /* First find all the registers that we need to deal with and all
     the modes that they can have.  If we can't find a mode to use,
     we can't have the register live over calls.  */

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (call_used_regs[i] && ! call_fixed_regs[i])
	{
	  regno_save_mode[i] = choose_hard_reg_mode (i);
	  if (regno_save_mode[i] == VOIDmode)
	    {
	      call_fixed_regs[i] = 1;
	      SET_HARD_REG_BIT (call_fixed_reg_set, i);
	    }
	}
      else
	regno_save_mode[i] = VOIDmode;
    }

  /* The following code tries to approximate the conditions under which
     we can easily save and restore a register without scratch registers or
     other complexities.  It will usually work, except under conditions where
     the validity of an insn operand is dependent on the address offset.
     No such cases are currently known.

     We first find a typical offset from some BASE_REG_CLASS register.
     This address is chosen by finding the first register in the class
     and by finding the smallest power of two that is a valid offset from
     that register in every mode we will use to save registers.  */

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (TEST_HARD_REG_BIT (reg_class_contents[(int) BASE_REG_CLASS], i))
      break;

  if (i == FIRST_PSEUDO_REGISTER)
    abort ();

  addr_reg = gen_rtx (REG, Pmode, i);

  for (offset = 1 << (HOST_BITS_PER_INT / 2); offset; offset >>= 1)
    {
      address = gen_rtx (PLUS, Pmode, addr_reg,
			 gen_rtx (CONST_INT, VOIDmode, offset));

      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (regno_save_mode[i] != VOIDmode
	    && ! strict_memory_address_p (regno_save_mode[i], address))
	  break;

      if (i == FIRST_PSEUDO_REGISTER)
	break;
    }

  /* If we didn't find a valid address, we must use register indirect.  */
  if (offset == 0)
    address = addr_reg;

  /* Next we try to form an insn to save and restore the register.  We
     see if such an insn is recognized and meets its constraints.  */

  start_sequence ();

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (regno_save_mode[i] != VOIDmode)
      {
	rtx mem = gen_rtx (MEM, regno_save_mode[i], address);
	rtx reg = gen_rtx (REG, regno_save_mode[i], i);
	rtx savepat = gen_rtx (SET, VOIDmode, mem, reg);
	rtx restpat = gen_rtx (SET, VOIDmode, reg, mem);
	rtx saveinsn = emit_insn (savepat);
	rtx restinsn = emit_insn (restpat);
	int ok;

	reg_save_code[i] = recog_memoized (saveinsn);
	reg_restore_code[i] = recog_memoized (restinsn);

	/* Now extract both insns and see if we can meet their constraints.  */
	ok = (reg_save_code[i] != -1 && reg_restore_code[i] != -1);
	if (ok)
	  {
	    insn_extract (saveinsn);
	    ok = constrain_operands (reg_save_code[i], 1);
	    insn_extract (restinsn);
	    ok &= constrain_operands (reg_restore_code[i], 1);
	  }

	if (! ok)
	  {
	    call_fixed_regs[i] = 1;
	    SET_HARD_REG_BIT (call_fixed_reg_set, i);
	  }
      }

  end_sequence ();

  obfree (first_obj);
}

/* Initialize save areas by showing that we haven't allocated any yet.  */

void
init_save_areas ()
{
  int i;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    regno_save_mem[i] = 0;
}

/* Allocate save areas for any hard registers that might need saving.
   We take a conservative approach here and look for call-clobbered hard
   registers that are assigned to pseudos that cross calls.  This may
   overestimate slightly (especially if some of these registers are later
   used as spill registers), but it should not be significant.

   Then perform register elimination in the addresses of the save area
   locations; return 1 if all eliminated addresses are strictly valid.
   We assume that our caller has set up the elimination table to the
   worst (largest) possible offsets.

   Set *PCHANGED to 1 if we had to allocate some memory for the save area.  */

int
setup_save_areas (pchanged)
     int *pchanged;
{
  int ok = 1;
  int i;

  for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
    if (reg_renumber[i] >= 0 && reg_n_calls_crossed[i] > 0)
      {
	int regno = reg_renumber[i];
	int endregno
	  = regno + HARD_REGNO_NREGS (regno, GET_MODE (regno_reg_rtx[i]));
	int j;

	for (j = regno; j < endregno; j++)
	  if (call_used_regs[j] && regno_save_mem[j] == 0)
	    {
	      regno_save_mem[j]
		= assign_stack_local (regno_save_mode[j],
				      GET_MODE_SIZE (regno_save_mode[j]), 0);
	      *pchanged = 1;
	    }
      }

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (regno_save_mem[i] != 0)
      ok &= strict_memory_address_p (regno_save_mode[i],
				     XEXP (eliminate_regs (regno_save_mem[i],
							   0, 0),
					   0));

  return ok;
}

/* Find the places where hard regs are live across calls and save them.

   INSN_MODE is the mode to assign to any insns that we add.  This is used
   by reload to determine whether or not reloads or register eliminations
   need be done on these insns.  */

void
save_call_clobbered_regs (insn_mode)
     enum machine_mode insn_mode;
{
  rtx insn;
  int b;

  for (b = 0; b < n_basic_blocks; b++)
    {
      regset regs_live = basic_block_live_at_start[b];
      int offset, bit, i, j;
      int regno;

      /* Compute hard regs live at start of block -- this is the
	 real hard regs marked live, plus live pseudo regs that
	 have been renumbered to hard regs.  No registers have yet been
	 saved because we restore all of them before the end of the basic
	 block.  */

#ifdef HARD_REG_SET
      hard_regs_live = *regs_live;
#else
      COPY_HARD_REG_SET (hard_regs_live, regs_live);
#endif

      CLEAR_HARD_REG_SET (hard_regs_saved);
      n_regs_saved = 0;

      for (offset = 0, i = 0; offset < regset_size; offset++)
	{
	  if (regs_live[offset] == 0)
	    i += HOST_BITS_PER_INT;
	  else
	    for (bit = 1; bit && i < max_regno; bit <<= 1, i++)
	      if ((regs_live[offset] & bit)
		  && (regno = reg_renumber[i]) >= 0)
		for (j = regno;
		     j < regno + HARD_REGNO_NREGS (regno,
						   PSEUDO_REGNO_MODE (i));
		     j++)
		  SET_HARD_REG_BIT (hard_regs_live, j);
	}

      /* Now scan the insns in the block, keeping track of what hard
	 regs are live as we go.  When we see a call, save the live
	 call-clobbered hard regs.  */

      for (insn = basic_block_head[b]; ; insn = NEXT_INSN (insn))
	{
	  RTX_CODE code = GET_CODE (insn);

	  if (GET_RTX_CLASS (code) == 'i')
	    {
	      rtx link;

	      /* If some registers have been saved, see if INSN references
		 any of them.  We must restore them before the insn if so.  */

	      if (n_regs_saved)
		restore_referenced_regs (PATTERN (insn), insn, insn_mode);

	      /* NB: the normal procedure is to first enliven any
		 registers set by insn, then deaden any registers that
		 had their last use at insn.  This is incorrect now,
		 since multiple pseudos may have been mapped to the
		 same hard reg, and the death notes are ambiguous.  So
		 it must be done in the other, safe, order.  */

	      for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
		if (REG_NOTE_KIND (link) == REG_DEAD)
		  clear_reg_live (XEXP (link, 0));

	      /* When we reach a call, we need to save all registers that are
		 live, call-used, not fixed, and not already saved.  We must
		 test at this point because registers that die in a CALL_INSN
		 are not live across the call and likewise for registers that
		 are born in the CALL_INSN.  */

	      if (code == CALL_INSN)
		for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
		  if (call_used_regs[regno] && ! call_fixed_regs[regno]
		      && TEST_HARD_REG_BIT (hard_regs_live, regno)
		      && ! TEST_HARD_REG_BIT (hard_regs_saved, regno))
		    insert_save_restore (insn, 1, regno, insn_mode);
	      
	      note_stores (PATTERN (insn), set_reg_live);

	      for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
		if (REG_NOTE_KIND (link) == REG_UNUSED)
		  clear_reg_live (XEXP (link, 0));
	    }

	  if (insn == basic_block_end[b])
	    break;
	}

      /* At the end of the basic block, we must restore any registers that
	 remain saved.  If the last insn in the block is a JUMP_INSN, put
	 the restore before the insn, otherwise, put it after the insn.  */

      if (n_regs_saved)
	for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	  if (TEST_HARD_REG_BIT (hard_regs_saved, regno))
	    insert_save_restore ((GET_CODE (insn) == JUMP_INSN
				  ? insn : NEXT_INSN (insn)),
				 0, regno, insn_mode);
    }
}

/* Here from note_stores when an insn stores a value in a register.
   Set the proper bit or bits in hard_regs_live.  All pseudos that have
   been assigned hard regs have had their register number changed already,
   so we can ignore pseudos.  */

static void
set_reg_live (reg, setter)
     rtx reg, setter;
{
  register int regno, endregno, i;
  enum machine_mode mode = GET_MODE (reg);
  int word = 0;

  if (GET_CODE (reg) == SUBREG)
    {
      word = SUBREG_WORD (reg);
      reg = SUBREG_REG (reg);
    }

  if (GET_CODE (reg) != REG || REGNO (reg) >= FIRST_PSEUDO_REGISTER)
    return;

  regno = REGNO (reg) + word;
  endregno = regno + HARD_REGNO_NREGS (regno, mode);

  for (i = regno; i < endregno; i++)
    SET_HARD_REG_BIT (hard_regs_live, i);
}

/* Here when a REG_DEAD note records the last use of a reg.  Clear
   the appropriate bit or bits in hard_regs_live.  Again we can ignore
   pseudos.  */

static void
clear_reg_live (reg)
     rtx reg;
{
  register int regno, endregno, i;

  if (GET_CODE (reg) != REG || REGNO (reg) >= FIRST_PSEUDO_REGISTER)
    return;

  regno = REGNO (reg);
  endregno= regno + HARD_REGNO_NREGS (regno, GET_MODE (reg));

  for (i = regno; i < endregno; i++)
    CLEAR_HARD_REG_BIT (hard_regs_live, i);
}      

/* If any register currently residing in the save area is referenced in X,
   which is part of INSN, emit code to restore the register in front of INSN.
   INSN_MODE is the mode to assign to any insns that we add.  */

static void
restore_referenced_regs (x, insn, insn_mode)
     rtx x;
     rtx insn;
     enum machine_mode insn_mode;
{
  enum rtx_code code = GET_CODE (x);
  char *fmt;
  int i, j;

  if (code == REG)
    {
      int regno = REGNO (x);

      /* If this is a pseudo, scan its memory location, since it might
	 involve the use of another register, which might be saved.  */

      if (regno >= FIRST_PSEUDO_REGISTER
	  && reg_equiv_mem[regno] != 0)
	restore_referenced_regs (XEXP (reg_equiv_mem[regno], 0),
				 insn, insn_mode);
      else if (regno >= FIRST_PSEUDO_REGISTER
	       && reg_equiv_address[regno] != 0)
	restore_referenced_regs (reg_equiv_address[regno],
				 insn, insn_mode);

      /* Otherwise if this is a hard register, restore any piece of it that
	 is currently saved.  */

      else if (regno < FIRST_PSEUDO_REGISTER)
	{
	  int endregno = regno + HARD_REGNO_NREGS (regno, GET_MODE (x));

	  for (i = regno; i < endregno; i ++)
	    if (TEST_HARD_REG_BIT (hard_regs_saved, i))
	      insert_save_restore (insn, 0, i, insn_mode);
	}

      return;
    }
	  
  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	restore_referenced_regs (XEXP (x, i), insn, insn_mode);
      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  restore_referenced_regs (XVECEXP (x, i, j), insn, insn_mode);
    }
}

/* Insert a sequence of insns to save or restore, SAVE_P says which,
   REGNO.  Place these insns in front of INSN.  INSN_MODE is the mode
   to assign to these insns. 

   Note that we have verified in init_caller_save that we can do this
   with a simple SET, so use it.  Set INSN_CODE to what we save there
   since the address might not be valid so the insn might not be recognized.
   These insns will be reloaded and have register elimination done by
   find_reload, so we need not worry about that here.  */

static void
insert_save_restore (insn, save_p, regno, insn_mode)
     rtx insn;
     int save_p;
     int regno;
     enum machine_mode insn_mode;
{
  rtx pat;
  enum insn_code code;
  int i;

  /* A common failure mode if register status is not correct in the RTL
     is for this routine to be called with a REGNO we didn't expect to
     save.  That will cause us to write an insn with a (nil) SET_DEST
     or SET_SRC.  Instead of doing so and causing a crash later, check
     for this common case and abort here instead.  This will remove one
     step in debugging such problems.  */

  if (regno_save_mem[regno] == 0)
    abort ();

  /* If INSN is a CALL_INSN, we must insert our insns before any
     USE insns in front of the CALL_INSN.  */

  if (GET_CODE (insn) == CALL_INSN)
    while (GET_CODE (PREV_INSN (insn)) == INSN
	   && GET_CODE (PATTERN (PREV_INSN (insn))) == USE)
      insn = PREV_INSN (insn);

#ifdef HAVE_cc0
  /* If INSN references CC0, put our insns in front of the insn that sets
     CC0.  This is always safe, since the only way we could be passed an
     insn that references CC0 is for a restore, and doing a restore earlier
     isn't a problem.  We do, however, assume here that CALL_INSNs don't
     reference CC0.  Guard against non-INSN's like CODE_LABEL.  */

  if ((GET_CODE (insn) == INSN || GET_CODE (insn) == JUMP_INSN)
      && reg_referenced_p (cc0_rtx, PATTERN (insn)))
    insn = prev_nonnote_insn (insn);
#endif

  /* Get the pattern to emit and update our status.  */
  if (save_p)
    {
      pat = gen_rtx (SET, VOIDmode, regno_save_mem[regno],
		     gen_rtx (REG, regno_save_mode[regno], regno));
      code = reg_save_code[regno];
      SET_HARD_REG_BIT (hard_regs_saved, regno);
      n_regs_saved++;
    }
  else
    {
      pat = gen_rtx (SET, VOIDmode,
		     gen_rtx (REG, regno_save_mode[regno], regno),
		     regno_save_mem[regno]);
      code = reg_restore_code[regno];
      CLEAR_HARD_REG_BIT (hard_regs_saved, regno);
      n_regs_saved--;
    }

  /* Emit the insn and set the code and mode.  */

  insn = emit_insn_before (pat, insn);
  PUT_MODE (insn, insn_mode);
  INSN_CODE (insn) = code;
}
