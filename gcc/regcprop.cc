/* Copy propagation on hard registers for the GNU compiler.
   Copyright (C) 2000-2024 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

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
#include "backend.h"
#include "rtl.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "insn-config.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "addresses.h"
#include "tree-pass.h"
#include "rtl-iter.h"
#include "cfgrtl.h"
#include "target.h"
#include "function-abi.h"

/* The following code does forward propagation of hard register copies.
   The object is to eliminate as many dependencies as possible, so that
   we have the most scheduling freedom.  As a side effect, we also clean
   up some silly register allocation decisions made by reload.  This
   code may be obsoleted by a new register allocator.  */

/* DEBUG_INSNs aren't changed right away, as doing so might extend the
   lifetime of a register and get the DEBUG_INSN subsequently reset.
   So they are queued instead, and updated only when the register is
   used in some subsequent real insn before it is set.  */
struct queued_debug_insn_change
{
  struct queued_debug_insn_change *next;
  rtx_insn *insn;
  rtx *loc;
  rtx new_rtx;
};

/* For each register, we have a list of registers that contain the same
   value.  The OLDEST_REGNO field points to the head of the list, and
   the NEXT_REGNO field runs through the list.  The MODE field indicates
   what mode the data is known to be in; this field is VOIDmode when the
   register is not known to contain valid data.  */

struct value_data_entry
{
  machine_mode mode;
  unsigned int oldest_regno;
  unsigned int next_regno;
  struct queued_debug_insn_change *debug_insn_changes;
};

struct value_data
{
  struct value_data_entry e[FIRST_PSEUDO_REGISTER];
  unsigned int max_value_regs;
  unsigned int n_debug_insn_changes;
};

static object_allocator<queued_debug_insn_change> queued_debug_insn_change_pool
  ("debug insn changes pool");

static bool skip_debug_insn_p;

static void kill_value_one_regno (unsigned, struct value_data *);
static void kill_value_regno (unsigned, unsigned, struct value_data *);
static void kill_value (const_rtx, struct value_data *);
static void set_value_regno (unsigned, machine_mode, struct value_data *);
static void init_value_data (struct value_data *);
static void kill_clobbered_value (rtx, const_rtx, void *);
static void kill_set_value (rtx, const_rtx, void *);
static void copy_value (rtx, rtx, struct value_data *);
static bool mode_change_ok (machine_mode, machine_mode,
			    unsigned int);
static rtx maybe_mode_change (machine_mode, machine_mode,
			      machine_mode, unsigned int, unsigned int);
static rtx find_oldest_value_reg (enum reg_class, rtx, struct value_data *);
static bool replace_oldest_value_reg (rtx *, enum reg_class, rtx_insn *,
				      struct value_data *);
static bool replace_oldest_value_addr (rtx *, enum reg_class,
				       machine_mode, addr_space_t,
				       rtx_insn *, struct value_data *);
static bool replace_oldest_value_mem (rtx, rtx_insn *, struct value_data *);
static bool copyprop_hardreg_forward_1 (basic_block, struct value_data *);
extern void debug_value_data (struct value_data *);
static void validate_value_data (struct value_data *);

/* Free all queued updates for DEBUG_INSNs that change some reg to
   register REGNO.  */

static void
free_debug_insn_changes (struct value_data *vd, unsigned int regno)
{
  struct queued_debug_insn_change *cur, *next;
  for (cur = vd->e[regno].debug_insn_changes; cur; cur = next)
    {
      next = cur->next;
      --vd->n_debug_insn_changes;
      queued_debug_insn_change_pool.remove (cur);
    }
  vd->e[regno].debug_insn_changes = NULL;
}

/* Kill register REGNO.  This involves removing it from any value
   lists, and resetting the value mode to VOIDmode.  This is only a
   helper function; it does not handle any hard registers overlapping
   with REGNO.  */

static void
kill_value_one_regno (unsigned int regno, struct value_data *vd)
{
  unsigned int i, next;

  if (vd->e[regno].oldest_regno != regno)
    {
      for (i = vd->e[regno].oldest_regno;
	   vd->e[i].next_regno != regno;
	   i = vd->e[i].next_regno)
	continue;
      vd->e[i].next_regno = vd->e[regno].next_regno;
    }
  else if ((next = vd->e[regno].next_regno) != INVALID_REGNUM)
    {
      for (i = next; i != INVALID_REGNUM; i = vd->e[i].next_regno)
	vd->e[i].oldest_regno = next;
    }

  vd->e[regno].mode = VOIDmode;
  vd->e[regno].oldest_regno = regno;
  vd->e[regno].next_regno = INVALID_REGNUM;
  if (vd->e[regno].debug_insn_changes)
    free_debug_insn_changes (vd, regno);

  if (flag_checking)
    validate_value_data (vd);
}

/* Kill the value in register REGNO for NREGS, and any other registers
   whose values overlap.  */

static void
kill_value_regno (unsigned int regno, unsigned int nregs,
		  struct value_data *vd)
{
  unsigned int j;

  /* Kill the value we're told to kill.  */
  for (j = 0; j < nregs; ++j)
    kill_value_one_regno (regno + j, vd);

  /* Kill everything that overlapped what we're told to kill.  */
  if (regno < vd->max_value_regs)
    j = 0;
  else
    j = regno - vd->max_value_regs;
  for (; j < regno; ++j)
    {
      unsigned int i, n;
      if (vd->e[j].mode == VOIDmode)
	continue;
      n = hard_regno_nregs (j, vd->e[j].mode);
      if (j + n > regno)
	for (i = 0; i < n; ++i)
	  kill_value_one_regno (j + i, vd);
    }
}

/* Kill X.  This is a convenience function wrapping kill_value_regno
   so that we mind the mode the register is in.  */

static void
kill_value (const_rtx x, struct value_data *vd)
{
  if (GET_CODE (x) == SUBREG)
    {
      rtx tmp = simplify_subreg (GET_MODE (x), SUBREG_REG (x),
				 GET_MODE (SUBREG_REG (x)), SUBREG_BYTE (x));
      x = tmp ? tmp : SUBREG_REG (x);
    }
  if (REG_P (x))
    kill_value_regno (REGNO (x), REG_NREGS (x), vd);
}

/* Remember that REGNO is valid in MODE.  */

static void
set_value_regno (unsigned int regno, machine_mode mode,
		 struct value_data *vd)
{
  unsigned int nregs;

  vd->e[regno].mode = mode;

  nregs = hard_regno_nregs (regno, mode);
  if (nregs > vd->max_value_regs)
    vd->max_value_regs = nregs;
}

/* Initialize VD such that there are no known relationships between regs.  */

static void
init_value_data (struct value_data *vd)
{
  int i;
  for (i = 0; i < FIRST_PSEUDO_REGISTER; ++i)
    {
      vd->e[i].mode = VOIDmode;
      vd->e[i].oldest_regno = i;
      vd->e[i].next_regno = INVALID_REGNUM;
      vd->e[i].debug_insn_changes = NULL;
    }
  vd->max_value_regs = 0;
  vd->n_debug_insn_changes = 0;
}

/* Called through note_stores.  If X is clobbered, kill its value.  */

static void
kill_clobbered_value (rtx x, const_rtx set, void *data)
{
  struct value_data *const vd = (struct value_data *) data;

  if (GET_CODE (set) == CLOBBER)
    kill_value (x, vd);
}

/* A structure passed as data to kill_set_value through note_stores.  */
struct kill_set_value_data
{
  struct value_data *vd;
  rtx ignore_set_reg;
};
  
/* Called through note_stores.  If X is set, not clobbered, kill its
   current value and install it as the root of its own value list.  */

static void
kill_set_value (rtx x, const_rtx set, void *data)
{
  struct kill_set_value_data *ksvd = (struct kill_set_value_data *) data;
  if (rtx_equal_p (x, ksvd->ignore_set_reg))
    return;

  if (GET_CODE (set) != CLOBBER)
    {
      kill_value (x, ksvd->vd);
      if (REG_P (x))
	set_value_regno (REGNO (x), GET_MODE (x), ksvd->vd);
    }
}

/* Kill any register used in X as the base of an auto-increment expression,
   and install that register as the root of its own value list.  */

static void
kill_autoinc_value (rtx_insn *insn, struct value_data *vd)
{
  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, PATTERN (insn), NONCONST)
    {
      const_rtx x = *iter;
      if (GET_RTX_CLASS (GET_CODE (x)) == RTX_AUTOINC)
	{
	  x = XEXP (x, 0);
	  kill_value (x, vd);
	  set_value_regno (REGNO (x), GET_MODE (x), vd);
	  iter.skip_subrtxes ();
	}
    }
}

/* Assert that SRC has been copied to DEST.  Adjust the data structures
   to reflect that SRC contains an older copy of the shared value.  */

static void
copy_value (rtx dest, rtx src, struct value_data *vd)
{
  unsigned int dr = REGNO (dest);
  unsigned int sr = REGNO (src);
  unsigned int dn, sn;
  unsigned int i;

  /* ??? At present, it's possible to see noop sets.  It'd be nice if
     this were cleaned up beforehand...  */
  if (sr == dr)
    return;

  /* Do not propagate copies to the stack pointer, as that can leave
     memory accesses with no scheduling dependency on the stack update.  */
  if (dr == STACK_POINTER_REGNUM)
    return;

  /* Likewise with the frame pointer, if we're using one.  */
  if (frame_pointer_needed && dr == HARD_FRAME_POINTER_REGNUM)
    return;

  /* Do not propagate copies to fixed or global registers, patterns
     can be relying to see particular fixed register or users can
     expect the chosen global register in asm.  */
  if (fixed_regs[dr] || global_regs[dr])
    return;

  /* If SRC and DEST overlap, don't record anything.  */
  dn = REG_NREGS (dest);
  sn = REG_NREGS (src);
  if ((dr > sr && dr < sr + sn)
      || (sr > dr && sr < dr + dn))
    return;

  /* If SRC had no assigned mode (i.e. we didn't know it was live)
     assign it now and assume the value came from an input argument
     or somesuch.  */
  if (vd->e[sr].mode == VOIDmode)
    set_value_regno (sr, vd->e[dr].mode, vd);

  /* If we are narrowing the input to a smaller number of hard regs,
     and it is in big endian, we are really extracting a high part.
     Since we generally associate a low part of a value with the value itself,
     we must not do the same for the high part.
     Note we can still get low parts for the same mode combination through
     a two-step copy involving differently sized hard regs.
     Assume hard regs fr* are 32 bits each, while r* are 64 bits each:
     (set (reg:DI r0) (reg:DI fr0))
     (set (reg:SI fr2) (reg:SI r0))
     loads the low part of (reg:DI fr0) - i.e. fr1 - into fr2, while:
     (set (reg:SI fr2) (reg:SI fr0))
     loads the high part of (reg:DI fr0) into fr2.

     We can't properly represent the latter case in our tables, so don't
     record anything then.  */
  else if (sn < hard_regno_nregs (sr, vd->e[sr].mode)
	   && maybe_ne (subreg_lowpart_offset (GET_MODE (dest),
					       vd->e[sr].mode), 0U))
    return;

  /* If SRC had been assigned a mode narrower than the copy, we can't
     link DEST into the chain, because not all of the pieces of the
     copy came from oldest_regno.  */
  else if (sn > hard_regno_nregs (sr, vd->e[sr].mode))
    return;

  /* If a narrower value is copied using wider mode, the upper bits
     are undefined (could be e.g. a former paradoxical subreg).  Signal
     in that case we've only copied value using the narrower mode.
     Consider:
     (set (reg:DI r14) (mem:DI ...))
     (set (reg:QI si) (reg:QI r14))
     (set (reg:DI bp) (reg:DI r14))
     (set (reg:DI r14) (const_int ...))
     (set (reg:DI dx) (reg:DI si))
     (set (reg:DI si) (const_int ...))
     (set (reg:DI dx) (reg:DI bp))
     The last set is not redundant, while the low 8 bits of dx are already
     equal to low 8 bits of bp, the other bits are undefined.  */
  else if (partial_subreg_p (vd->e[sr].mode, GET_MODE (src)))
    {
      if (!REG_CAN_CHANGE_MODE_P (sr, GET_MODE (src), vd->e[sr].mode)
	  || !REG_CAN_CHANGE_MODE_P (dr, vd->e[sr].mode, GET_MODE (dest)))
	return;
      set_value_regno (dr, vd->e[sr].mode, vd);
    }

  /* Link DR at the end of the value chain used by SR.  */

  vd->e[dr].oldest_regno = vd->e[sr].oldest_regno;

  for (i = sr; vd->e[i].next_regno != INVALID_REGNUM; i = vd->e[i].next_regno)
    continue;
  vd->e[i].next_regno = dr;

  if (flag_checking)
    validate_value_data (vd);
}

/* Return true if a mode change from ORIG to NEW is allowed for REGNO.  */

static bool
mode_change_ok (machine_mode orig_mode, machine_mode new_mode,
		unsigned int regno ATTRIBUTE_UNUSED)
{
  if (partial_subreg_p (orig_mode, new_mode))
    return false;

  return REG_CAN_CHANGE_MODE_P (regno, orig_mode, new_mode);
}

/* Register REGNO was originally set in ORIG_MODE.  It - or a copy of it -
   was copied in COPY_MODE to COPY_REGNO, and then COPY_REGNO was accessed
   in NEW_MODE.
   Return a NEW_MODE rtx for REGNO if that's OK, otherwise return NULL_RTX.  */

static rtx
maybe_mode_change (machine_mode orig_mode, machine_mode copy_mode,
		   machine_mode new_mode, unsigned int regno,
		   unsigned int copy_regno ATTRIBUTE_UNUSED)
{
  if (partial_subreg_p (copy_mode, orig_mode)
      && partial_subreg_p (copy_mode, new_mode))
    return NULL_RTX;

  /* Avoid creating multiple copies of the stack pointer.  Some ports
     assume there is one and only one stack pointer.

     It's unclear if we need to do the same for other special registers.  */
  if (regno == STACK_POINTER_REGNUM)
    {
      if (orig_mode == new_mode && new_mode == GET_MODE (stack_pointer_rtx))
	return stack_pointer_rtx;
      else
	return NULL_RTX;
    }

  if (orig_mode == new_mode)
    return gen_raw_REG (new_mode, regno);
  else if (mode_change_ok (orig_mode, new_mode, regno)
	   && mode_change_ok (copy_mode, new_mode, copy_regno))
    {
      int copy_nregs = hard_regno_nregs (copy_regno, copy_mode);
      int use_nregs = hard_regno_nregs (copy_regno, new_mode);
      poly_uint64 bytes_per_reg;
      if (!can_div_trunc_p (GET_MODE_SIZE (copy_mode),
			    copy_nregs, &bytes_per_reg))
	return NULL_RTX;
      poly_uint64 copy_offset = bytes_per_reg * (copy_nregs - use_nregs);
      poly_uint64 offset
	= subreg_size_lowpart_offset (GET_MODE_SIZE (new_mode) + copy_offset,
				      GET_MODE_SIZE (orig_mode));
      regno += subreg_regno_offset (regno, orig_mode, offset, new_mode);
      if (targetm.hard_regno_mode_ok (regno, new_mode))
	return gen_raw_REG (new_mode, regno);
    }
  return NULL_RTX;
}

/* Find the oldest copy of the value contained in REGNO that is in
   register class CL and has mode MODE.  If found, return an rtx
   of that oldest register, otherwise return NULL.  */

static rtx
find_oldest_value_reg (enum reg_class cl, rtx reg, struct value_data *vd)
{
  unsigned int regno = REGNO (reg);
  machine_mode mode = GET_MODE (reg);
  unsigned int i;

  gcc_assert (regno < FIRST_PSEUDO_REGISTER);

  /* If we are accessing REG in some mode other that what we set it in,
     make sure that the replacement is valid.  In particular, consider
	(set (reg:DI r11) (...))
	(set (reg:SI r9) (reg:SI r11))
	(set (reg:SI r10) (...))
	(set (...) (reg:DI r9))
     Replacing r9 with r11 is invalid.  */
  if (mode != vd->e[regno].mode
      && (REG_NREGS (reg) > hard_regno_nregs (regno, vd->e[regno].mode)
	  || !REG_CAN_CHANGE_MODE_P (regno, mode, vd->e[regno].mode)))
    return NULL_RTX;

  for (i = vd->e[regno].oldest_regno; i != regno; i = vd->e[i].next_regno)
    {
      machine_mode oldmode = vd->e[i].mode;
      rtx new_rtx;

      if (!in_hard_reg_set_p (reg_class_contents[cl], mode, i))
	continue;

      new_rtx = maybe_mode_change (oldmode, vd->e[regno].mode, mode, i, regno);
      if (new_rtx)
	{
	  /* NEW_RTX may be the global stack pointer rtx, in which case we
	     must not modify it's attributes.  */
	  if (new_rtx != stack_pointer_rtx)
	    {
	      ORIGINAL_REGNO (new_rtx) = ORIGINAL_REGNO (reg);
	      REG_ATTRS (new_rtx) = REG_ATTRS (reg);
	      REG_POINTER (new_rtx) = REG_POINTER (reg);
	    }
	  return new_rtx;
	}
    }

  return NULL_RTX;
}

/* If possible, replace the register at *LOC with the oldest register
   in register class CL.  Return true if successfully replaced.  */

static bool
replace_oldest_value_reg (rtx *loc, enum reg_class cl, rtx_insn *insn,
			  struct value_data *vd)
{
  rtx new_rtx = find_oldest_value_reg (cl, *loc, vd);
  if (new_rtx && (!DEBUG_INSN_P (insn) || !skip_debug_insn_p))
    {
      if (DEBUG_INSN_P (insn))
	{
	  struct queued_debug_insn_change *change;

	  if (dump_file)
	    fprintf (dump_file, "debug_insn %u: queued replacing reg %u with %u\n",
		     INSN_UID (insn), REGNO (*loc), REGNO (new_rtx));

	  change = queued_debug_insn_change_pool.allocate ();
	  change->next = vd->e[REGNO (new_rtx)].debug_insn_changes;
	  change->insn = insn;
	  change->loc = loc;
	  change->new_rtx = new_rtx;
	  vd->e[REGNO (new_rtx)].debug_insn_changes = change;
	  ++vd->n_debug_insn_changes;
	  return true;
	}
      if (dump_file)
	fprintf (dump_file, "insn %u: replaced reg %u with %u\n",
		 INSN_UID (insn), REGNO (*loc), REGNO (new_rtx));

      validate_change (insn, loc, new_rtx, 1);
      return true;
    }
  return false;
}

/* Similar to replace_oldest_value_reg, but *LOC contains an address.
   Adapted from find_reloads_address_1.  CL is INDEX_REG_CLASS or
   BASE_REG_CLASS depending on how the register is being considered.  */

static bool
replace_oldest_value_addr (rtx *loc, enum reg_class cl,
			   machine_mode mode, addr_space_t as,
			   rtx_insn *insn, struct value_data *vd)
{
  rtx x = *loc;
  RTX_CODE code = GET_CODE (x);
  const char *fmt;
  int i, j;
  bool changed = false;

  switch (code)
    {
    case PLUS:
      if (DEBUG_INSN_P (insn))
	break;

      {
	rtx orig_op0 = XEXP (x, 0);
	rtx orig_op1 = XEXP (x, 1);
	RTX_CODE code0 = GET_CODE (orig_op0);
	RTX_CODE code1 = GET_CODE (orig_op1);
	rtx op0 = orig_op0;
	rtx op1 = orig_op1;
	rtx *locI = NULL;
	rtx *locB = NULL;
	enum rtx_code index_code = SCRATCH;

	if (GET_CODE (op0) == SUBREG)
	  {
	    op0 = SUBREG_REG (op0);
	    code0 = GET_CODE (op0);
	  }

	if (GET_CODE (op1) == SUBREG)
	  {
	    op1 = SUBREG_REG (op1);
	    code1 = GET_CODE (op1);
	  }

	if (code0 == MULT || code0 == SIGN_EXTEND || code0 == TRUNCATE
	    || code0 == ZERO_EXTEND || code1 == MEM)
	  {
	    locI = &XEXP (x, 0);
	    locB = &XEXP (x, 1);
	    index_code = GET_CODE (*locI);
	  }
	else if (code1 == MULT || code1 == SIGN_EXTEND || code1 == TRUNCATE
		 || code1 == ZERO_EXTEND || code0 == MEM)
	  {
	    locI = &XEXP (x, 1);
	    locB = &XEXP (x, 0);
	    index_code = GET_CODE (*locI);
	  }
	else if (code0 == CONST_INT || code0 == CONST
		 || code0 == SYMBOL_REF || code0 == LABEL_REF)
	  {
	    locB = &XEXP (x, 1);
	    index_code = GET_CODE (XEXP (x, 0));
	  }
	else if (code1 == CONST_INT || code1 == CONST
		 || code1 == SYMBOL_REF || code1 == LABEL_REF)
	  {
	    locB = &XEXP (x, 0);
	    index_code = GET_CODE (XEXP (x, 1));
	  }
	else if (code0 == REG && code1 == REG)
	  {
	    int index_op;
	    unsigned regno0 = REGNO (op0), regno1 = REGNO (op1);

	    if (REGNO_OK_FOR_INDEX_P (regno1)
		&& regno_ok_for_base_p (regno0, mode, as, PLUS, REG))
	      index_op = 1;
	    else if (REGNO_OK_FOR_INDEX_P (regno0)
		     && regno_ok_for_base_p (regno1, mode, as, PLUS, REG))
	      index_op = 0;
	    else if (regno_ok_for_base_p (regno0, mode, as, PLUS, REG)
		     || REGNO_OK_FOR_INDEX_P (regno1))
	      index_op = 1;
	    else if (regno_ok_for_base_p (regno1, mode, as, PLUS, REG))
	      index_op = 0;
	    else
	      index_op = 1;

	    locI = &XEXP (x, index_op);
	    locB = &XEXP (x, !index_op);
	    index_code = GET_CODE (*locI);
	  }
	else if (code0 == REG)
	  {
	    locI = &XEXP (x, 0);
	    locB = &XEXP (x, 1);
	    index_code = GET_CODE (*locI);
	  }
	else if (code1 == REG)
	  {
	    locI = &XEXP (x, 1);
	    locB = &XEXP (x, 0);
	    index_code = GET_CODE (*locI);
	  }

	if (locI)
	  changed |= replace_oldest_value_addr (locI, INDEX_REG_CLASS,
						mode, as, insn, vd);
	if (locB)
	  changed |= replace_oldest_value_addr (locB,
						base_reg_class (mode, as, PLUS,
								index_code),
						mode, as, insn, vd);
	return changed;
      }

    case POST_INC:
    case POST_DEC:
    case POST_MODIFY:
    case PRE_INC:
    case PRE_DEC:
    case PRE_MODIFY:
      return false;

    case MEM:
      return replace_oldest_value_mem (x, insn, vd);

    case REG:
      return replace_oldest_value_reg (loc, cl, insn, vd);

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	changed |= replace_oldest_value_addr (&XEXP (x, i), cl, mode, as,
					      insn, vd);
      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  changed |= replace_oldest_value_addr (&XVECEXP (x, i, j), cl,
						mode, as, insn, vd);
    }

  return changed;
}

/* Similar to replace_oldest_value_reg, but X contains a memory.  */

static bool
replace_oldest_value_mem (rtx x, rtx_insn *insn, struct value_data *vd)
{
  enum reg_class cl;

  if (DEBUG_INSN_P (insn))
    cl = ALL_REGS;
  else
    cl = base_reg_class (GET_MODE (x), MEM_ADDR_SPACE (x), MEM, SCRATCH);

  return replace_oldest_value_addr (&XEXP (x, 0), cl,
				    GET_MODE (x), MEM_ADDR_SPACE (x),
				    insn, vd);
}

/* Apply all queued updates for DEBUG_INSNs that change some reg to
   register REGNO.  */

static void
apply_debug_insn_changes (struct value_data *vd, unsigned int regno)
{
  struct queued_debug_insn_change *change;
  rtx_insn *last_insn = vd->e[regno].debug_insn_changes->insn;

  for (change = vd->e[regno].debug_insn_changes;
       change;
       change = change->next)
    {
      if (last_insn != change->insn)
	{
	  apply_change_group ();
	  last_insn = change->insn;
	}
      validate_change (change->insn, change->loc, change->new_rtx, 1);
    }
  apply_change_group ();
}

/* Called via note_uses, for all used registers in a real insn
   apply DEBUG_INSN changes that change registers to the used
   registers.  */

static void
cprop_find_used_regs (rtx *loc, void *data)
{
  struct value_data *const vd = (struct value_data *) data;
  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, *loc, NONCONST)
    {
      const_rtx x = *iter;
      if (REG_P (x))
	{
	  unsigned int regno = REGNO (x);
	  if (vd->e[regno].debug_insn_changes)
	    {
	      apply_debug_insn_changes (vd, regno);
	      free_debug_insn_changes (vd, regno);
	    }
	}
    }
}

/* Apply clobbers of INSN in PATTERN and C_I_F_U to value_data VD.  */

static void
kill_clobbered_values (rtx_insn *insn, struct value_data *vd)
{
  note_stores (insn, kill_clobbered_value, vd);
}

/* Perform the forward copy propagation on basic block BB.  */

static bool
copyprop_hardreg_forward_1 (basic_block bb, struct value_data *vd)
{
  bool anything_changed = false;
  rtx_insn *insn, *next;

  for (insn = BB_HEAD (bb); ; insn = next)
    {
      int n_ops, i, predicated;
      bool is_asm, any_replacements;
      rtx set;
      rtx link;
      bool changed = false;
      struct kill_set_value_data ksvd;

      next = NEXT_INSN (insn);
      if (!NONDEBUG_INSN_P (insn))
	{
	  if (DEBUG_BIND_INSN_P (insn))
	    {
	      rtx loc = INSN_VAR_LOCATION_LOC (insn);
	      if (!VAR_LOC_UNKNOWN_P (loc))
		replace_oldest_value_addr (&INSN_VAR_LOCATION_LOC (insn),
					   ALL_REGS, GET_MODE (loc),
					   ADDR_SPACE_GENERIC, insn, vd);
	    }

	  if (insn == BB_END (bb))
	    break;
	  else
	    continue;
	}

      set = single_set (insn);

      /* Detect noop sets and remove them before processing side effects.  */
      if (set && REG_P (SET_DEST (set)) && REG_P (SET_SRC (set)))
	{
	  unsigned int regno = REGNO (SET_SRC (set));
	  rtx r1 = find_oldest_value_reg (REGNO_REG_CLASS (regno),
					  SET_DEST (set), vd);
	  rtx r2 = find_oldest_value_reg (REGNO_REG_CLASS (regno),
					  SET_SRC (set), vd);
	  if (rtx_equal_p (r1 ? r1 : SET_DEST (set), r2 ? r2 : SET_SRC (set)))
	    {
	      bool last = insn == BB_END (bb);
	      delete_insn (insn);
	      if (last)
		break;
	      continue;
	    }
	}

      /* Detect obviously dead sets (via REG_UNUSED notes) and remove them.  */
      if (set
	  && !RTX_FRAME_RELATED_P (insn)
	  && NONJUMP_INSN_P (insn)
	  && !may_trap_p (set)
	  && find_reg_note (insn, REG_UNUSED, SET_DEST (set))
	  && !side_effects_p (SET_SRC (set))
	  && !side_effects_p (SET_DEST (set)))
	{
	  bool last = insn == BB_END (bb);
	  delete_insn (insn);
	  if (last)
	    break;
	  continue;
	}
	 

      extract_constrain_insn (insn);
      preprocess_constraints (insn);
      const operand_alternative *op_alt = which_op_alt ();
      n_ops = recog_data.n_operands;
      is_asm = asm_noperands (PATTERN (insn)) >= 0;

      /* Simplify the code below by promoting OP_OUT to OP_INOUT
	 in predicated instructions.  */

      predicated = GET_CODE (PATTERN (insn)) == COND_EXEC;
      for (i = 0; i < n_ops; ++i)
	{
	  int matches = op_alt[i].matches;
	  if (matches >= 0 || op_alt[i].matched >= 0
	      || (predicated && recog_data.operand_type[i] == OP_OUT))
	    recog_data.operand_type[i] = OP_INOUT;
	}

      /* Apply changes to earlier DEBUG_INSNs if possible.  */
      if (vd->n_debug_insn_changes)
	note_uses (&PATTERN (insn), cprop_find_used_regs, vd);

      /* For each earlyclobber operand, zap the value data.  */
      for (i = 0; i < n_ops; i++)
	if (op_alt[i].earlyclobber)
	  kill_value (recog_data.operand[i], vd);

      /* Within asms, a clobber cannot overlap inputs or outputs.
	 I wouldn't think this were true for regular insns, but
	 scan_rtx treats them like that...  */
      kill_clobbered_values (insn, vd);

      /* Kill all auto-incremented values.  */
      /* ??? REG_INC is useless, since stack pushes aren't done that way.  */
      kill_autoinc_value (insn, vd);

      /* Kill all early-clobbered operands.  */
      for (i = 0; i < n_ops; i++)
	if (op_alt[i].earlyclobber)
	  kill_value (recog_data.operand[i], vd);

      /* If we have dead sets in the insn, then we need to note these as we
	 would clobbers.  */
      for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
	{
	  if (REG_NOTE_KIND (link) == REG_UNUSED)
	    {
	      kill_value (XEXP (link, 0), vd);
	      /* Furthermore, if the insn looked like a single-set,
		 but the dead store kills the source value of that
		 set, then we can no-longer use the plain move
		 special case below.  */
	      if (set
		  && reg_overlap_mentioned_p (XEXP (link, 0), SET_SRC (set)))
		set = NULL;
	    }

	  /* We need to keep CFI info correct, and the same on all paths,
	     so we cannot normally replace the registers REG_CFA_REGISTER
	     refers to.  Bail.  */
	  if (REG_NOTE_KIND (link) == REG_CFA_REGISTER)
	    goto did_replacement;
	}

      /* Special-case plain move instructions, since we may well
	 be able to do the move from a different register class.  */
      if (set && REG_P (SET_SRC (set)))
	{
	  rtx src = SET_SRC (set);
	  rtx dest = SET_DEST (set);
	  unsigned int regno = REGNO (src);
	  machine_mode mode = GET_MODE (src);
	  unsigned int i;
	  rtx new_rtx;

	  /* If we are accessing SRC in some mode other that what we
	     set it in, make sure that the replacement is valid.  */
	  if (mode != vd->e[regno].mode)
	    {
	      if (REG_NREGS (src)
		  > hard_regno_nregs (regno, vd->e[regno].mode))
		goto no_move_special_case;

	      /* And likewise, if we are narrowing on big endian the transformation
		 is also invalid.  */
	      if (REG_NREGS (src) < hard_regno_nregs (regno, vd->e[regno].mode)
		  && maybe_ne (subreg_lowpart_offset (mode,
						      vd->e[regno].mode), 0U))
		goto no_move_special_case;
	    }

	  /* If the destination is also a register, try to find a source
	     register in the same class.  */
	  if (REG_P (dest))
	    {
	      new_rtx = find_oldest_value_reg (REGNO_REG_CLASS (regno),
					       src, vd);

	      if (new_rtx && validate_change (insn, &SET_SRC (set), new_rtx, 0))
		{
		  if (dump_file)
		    fprintf (dump_file,
			     "insn %u: replaced reg %u with %u\n",
			     INSN_UID (insn), regno, REGNO (new_rtx));
		  changed = true;
		  goto did_replacement;
		}
	      /* We need to re-extract as validate_change clobbers
		 recog_data.  */
	      extract_constrain_insn (insn);
	      preprocess_constraints (insn);
	    }

	  /* Otherwise, try all valid registers and see if its valid.  */
	  for (i = vd->e[regno].oldest_regno; i != regno;
	       i = vd->e[i].next_regno)
	    {
	      new_rtx = maybe_mode_change (vd->e[i].mode, vd->e[regno].mode,
				       mode, i, regno);
	      if (new_rtx != NULL_RTX)
		{
		  /* Don't propagate for a more expensive reg-reg move.  */
		  if (REG_P (dest))
		    {
		      enum reg_class from = REGNO_REG_CLASS (regno);
		      enum reg_class to = REGNO_REG_CLASS (REGNO (dest));
		      enum reg_class new_from = REGNO_REG_CLASS (i);
		      unsigned int original_cost
			= targetm.register_move_cost (mode, from, to);
		      unsigned int after_cost
			= targetm.register_move_cost (mode, new_from, to);
		      if (after_cost > original_cost)
			continue;
		    }

		  if (validate_change (insn, &SET_SRC (set), new_rtx, 0))
		    {
		      /* NEW_RTX may be the global stack pointer rtx, in which
			 case we must not modify it's attributes.  */
		      if (new_rtx != stack_pointer_rtx)
			{
			  ORIGINAL_REGNO (new_rtx) = ORIGINAL_REGNO (src);
			  REG_ATTRS (new_rtx) = REG_ATTRS (src);
			  REG_POINTER (new_rtx) = REG_POINTER (src);
			}
		      if (dump_file)
			fprintf (dump_file,
				 "insn %u: replaced reg %u with %u\n",
				 INSN_UID (insn), regno, REGNO (new_rtx));
		      changed = true;
		      goto did_replacement;
		    }
		  /* We need to re-extract as validate_change clobbers
		     recog_data.  */
		  extract_constrain_insn (insn);
		  preprocess_constraints (insn);
		}
	    }
	}
      no_move_special_case:

      any_replacements = false;

      /* For each input operand, replace a hard register with the
	 eldest live copy that's in an appropriate register class.  */
      for (i = 0; i < n_ops; i++)
	{
	  bool replaced = false;

	  /* Don't scan match_operand here, since we've no reg class
	     information to pass down.  Any operands that we could
	     substitute in will be represented elsewhere.  */
	  if (recog_data.constraints[i][0] == '\0')
	    continue;

	  /* Don't replace in asms intentionally referencing hard regs.  */
	  if (is_asm && REG_P (recog_data.operand[i])
	      && (REGNO (recog_data.operand[i])
		  == ORIGINAL_REGNO (recog_data.operand[i])))
	    continue;

	  if (recog_data.operand_type[i] == OP_IN)
	    {
	      if (op_alt[i].is_address)
		replaced
		  = replace_oldest_value_addr (recog_data.operand_loc[i],
					       alternative_class (op_alt, i),
					       VOIDmode, ADDR_SPACE_GENERIC,
					       insn, vd);
	      else if (REG_P (recog_data.operand[i]))
		replaced
		  = replace_oldest_value_reg (recog_data.operand_loc[i],
					      alternative_class (op_alt, i),
					      insn, vd);
	      else if (MEM_P (recog_data.operand[i]))
		replaced = replace_oldest_value_mem (recog_data.operand[i],
						     insn, vd);
	    }
	  else if (MEM_P (recog_data.operand[i]))
	    replaced = replace_oldest_value_mem (recog_data.operand[i],
						 insn, vd);

	  /* If we performed any replacement, update match_dups.  */
	  if (replaced)
	    {
	      int j;
	      rtx new_rtx;

	      new_rtx = *recog_data.operand_loc[i];
	      recog_data.operand[i] = new_rtx;
	      for (j = 0; j < recog_data.n_dups; j++)
		if (recog_data.dup_num[j] == i)
		  validate_unshare_change (insn, recog_data.dup_loc[j], new_rtx, 1);

	      any_replacements = true;
	    }
	}

      if (any_replacements)
	{
	  if (! apply_change_group ())
	    {
	      if (dump_file)
		fprintf (dump_file,
			 "insn %u: reg replacements not verified\n",
			 INSN_UID (insn));
	    }
	  else
	    changed = true;
	}

    did_replacement:
      if (changed)
	{
	  anything_changed = true;

	  /* If something changed, perhaps further changes to earlier
	     DEBUG_INSNs can be applied.  */
	  if (vd->n_debug_insn_changes)
	    note_uses (&PATTERN (insn), cprop_find_used_regs, vd);
	  df_insn_rescan (insn);
	}

      ksvd.vd = vd;
      ksvd.ignore_set_reg = NULL_RTX;

      /* Clobber call-clobbered registers.  */
      if (CALL_P (insn))
	{
	  unsigned int set_regno = INVALID_REGNUM;
	  unsigned int set_nregs = 0;
	  unsigned int regno;
	  rtx exp;

	  for (exp = CALL_INSN_FUNCTION_USAGE (insn); exp; exp = XEXP (exp, 1))
	    {
	      rtx x = XEXP (exp, 0);
	      if (GET_CODE (x) == SET)
		{
		  rtx dest = SET_DEST (x);
		  kill_value (dest, vd);
		  set_value_regno (REGNO (dest), GET_MODE (dest), vd);
		  copy_value (dest, SET_SRC (x), vd);
		  ksvd.ignore_set_reg = dest;
		  set_regno = REGNO (dest);
		  set_nregs = REG_NREGS (dest);
		  break;
		}
	    }

	  function_abi callee_abi = insn_callee_abi (insn);
	  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	    if (vd->e[regno].mode != VOIDmode
		&& callee_abi.clobbers_reg_p (vd->e[regno].mode, regno)
		&& (regno < set_regno || regno >= set_regno + set_nregs))
	      kill_value_regno (regno, 1, vd);

	  /* If SET was seen in CALL_INSN_FUNCTION_USAGE, and SET_SRC
	     of the SET isn't clobbered by CALLEE_ABI, but instead among
	     CLOBBERs on the CALL_INSN, we could wrongly assume the
	     value in it is still live.  */
	  if (ksvd.ignore_set_reg)
	    kill_clobbered_values (insn, vd);
	}

      bool copy_p = (set
		     && REG_P (SET_DEST (set))
		     && REG_P (SET_SRC (set)));
      bool noop_p = (copy_p
		     && rtx_equal_p (SET_DEST (set), SET_SRC (set)));

      /* If a noop move is using narrower mode than we have recorded,
	 we need to either remove the noop move, or kill_set_value.  */
      if (noop_p
	  && partial_subreg_p (GET_MODE (SET_DEST (set)),
			       vd->e[REGNO (SET_DEST (set))].mode))
	{
	  if (noop_move_p (insn))
	    {
	      bool last = insn == BB_END (bb);
	      delete_insn (insn);
	      if (last)
		break;
	    }
	  else
	    noop_p = false;
	}

      if (!noop_p)
	{
	  /* Notice stores.  */
	  note_stores (insn, kill_set_value, &ksvd);

	  /* Notice copies.  */
	  if (copy_p)
	    {
	      df_insn_rescan (insn);
	      copy_value (SET_DEST (set), SET_SRC (set), vd);
	    }
	}

      if (insn == BB_END (bb))
	break;
    }

  return anything_changed;
}

/* Dump the value chain data to stderr.  */

DEBUG_FUNCTION void
debug_value_data (struct value_data *vd)
{
  HARD_REG_SET set;
  unsigned int i, j;

  CLEAR_HARD_REG_SET (set);

  for (i = 0; i < FIRST_PSEUDO_REGISTER; ++i)
    if (vd->e[i].oldest_regno == i)
      {
	if (vd->e[i].mode == VOIDmode)
	  {
	    if (vd->e[i].next_regno != INVALID_REGNUM)
	      fprintf (stderr, "[%u] Bad next_regno for empty chain (%u)\n",
		       i, vd->e[i].next_regno);
	    continue;
	  }

	SET_HARD_REG_BIT (set, i);
	fprintf (stderr, "[%u %s] ", i, GET_MODE_NAME (vd->e[i].mode));

	for (j = vd->e[i].next_regno;
	     j != INVALID_REGNUM;
	     j = vd->e[j].next_regno)
	  {
	    if (TEST_HARD_REG_BIT (set, j))
	      {
		fprintf (stderr, "[%u] Loop in regno chain\n", j);
		return;
	      }

	    if (vd->e[j].oldest_regno != i)
	      {
		fprintf (stderr, "[%u] Bad oldest_regno (%u)\n",
			 j, vd->e[j].oldest_regno);
		return;
	      }
	    SET_HARD_REG_BIT (set, j);
	    fprintf (stderr, "[%u %s] ", j, GET_MODE_NAME (vd->e[j].mode));
	  }
	fputc ('\n', stderr);
      }

  for (i = 0; i < FIRST_PSEUDO_REGISTER; ++i)
    if (! TEST_HARD_REG_BIT (set, i)
	&& (vd->e[i].mode != VOIDmode
	    || vd->e[i].oldest_regno != i
	    || vd->e[i].next_regno != INVALID_REGNUM))
      fprintf (stderr, "[%u] Non-empty reg in chain (%s %u %i)\n",
	       i, GET_MODE_NAME (vd->e[i].mode), vd->e[i].oldest_regno,
	       vd->e[i].next_regno);
}

/* Do copyprop_hardreg_forward_1 for a single basic block BB.
   DEBUG_INSN is skipped since we do not want to involve DF related
   staff as how it is handled in function pass_cprop_hardreg::execute.

   NOTE: Currently it is only used for shrink-wrap.  Maybe extend it
   to handle DEBUG_INSN for other uses.  */

void
copyprop_hardreg_forward_bb_without_debug_insn (basic_block bb)
{
  struct value_data *vd;
  vd = XNEWVEC (struct value_data, 1);
  init_value_data (vd);

  skip_debug_insn_p = true;
  copyprop_hardreg_forward_1 (bb, vd);
  free (vd);
  skip_debug_insn_p = false;
}

static void
validate_value_data (struct value_data *vd)
{
  HARD_REG_SET set;
  unsigned int i, j;

  CLEAR_HARD_REG_SET (set);

  for (i = 0; i < FIRST_PSEUDO_REGISTER; ++i)
    if (vd->e[i].oldest_regno == i)
      {
	if (vd->e[i].mode == VOIDmode)
	  {
	    if (vd->e[i].next_regno != INVALID_REGNUM)
	      internal_error ("%qs: [%u] bad %<next_regno%> for empty chain (%u)",
			      __func__, i, vd->e[i].next_regno);
	    continue;
	  }

	SET_HARD_REG_BIT (set, i);

	for (j = vd->e[i].next_regno;
	     j != INVALID_REGNUM;
	     j = vd->e[j].next_regno)
	  {
	    if (TEST_HARD_REG_BIT (set, j))
	      internal_error ("%qs: loop in %<next_regno%> chain (%u)",
			      __func__, j);
	    if (vd->e[j].oldest_regno != i)
	      internal_error ("%qs: [%u] bad %<oldest_regno%> (%u)",
			      __func__, j, vd->e[j].oldest_regno);

	    SET_HARD_REG_BIT (set, j);
	  }
      }

  for (i = 0; i < FIRST_PSEUDO_REGISTER; ++i)
    if (! TEST_HARD_REG_BIT (set, i)
	&& (vd->e[i].mode != VOIDmode
	    || vd->e[i].oldest_regno != i
	    || vd->e[i].next_regno != INVALID_REGNUM))
      internal_error ("%qs: [%u] non-empty register in chain (%s %u %i)",
		      __func__, i,
		      GET_MODE_NAME (vd->e[i].mode), vd->e[i].oldest_regno,
		      vd->e[i].next_regno);
}


namespace {

const pass_data pass_data_cprop_hardreg =
{
  RTL_PASS, /* type */
  "cprop_hardreg", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_CPROP_REGISTERS, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_df_finish, /* todo_flags_finish */
};

class pass_cprop_hardreg : public rtl_opt_pass
{
public:
  pass_cprop_hardreg (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_cprop_hardreg, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override
    {
      return (optimize > 0 && (flag_cprop_registers));
    }

  unsigned int execute (function *) final override;

}; // class pass_cprop_hardreg

static bool
cprop_hardreg_bb (basic_block bb, struct value_data *all_vd, sbitmap visited)
{
  bitmap_set_bit (visited, bb->index);

  /* If a block has a single predecessor, that we've already
     processed, begin with the value data that was live at
     the end of the predecessor block.  */
  /* ??? Ought to use more intelligent queuing of blocks.  */
  if (single_pred_p (bb)
      && bitmap_bit_p (visited, single_pred (bb)->index)
      && ! (single_pred_edge (bb)->flags & (EDGE_ABNORMAL_CALL | EDGE_EH)))
    {
      all_vd[bb->index] = all_vd[single_pred (bb)->index];
      if (all_vd[bb->index].n_debug_insn_changes)
	{
	  unsigned int regno;

	  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	    {
	      if (all_vd[bb->index].e[regno].debug_insn_changes)
		{
		  struct queued_debug_insn_change *cur;
		  for (cur = all_vd[bb->index].e[regno].debug_insn_changes;
		       cur; cur = cur->next)
		    --all_vd[bb->index].n_debug_insn_changes;
		  all_vd[bb->index].e[regno].debug_insn_changes = NULL;
		  if (all_vd[bb->index].n_debug_insn_changes == 0)
		    break;
		}
	    }
	}
    }
  else
    init_value_data (all_vd + bb->index);

  return copyprop_hardreg_forward_1 (bb, all_vd + bb->index);
}

static void
cprop_hardreg_debug (function *fun, struct value_data *all_vd)
{
  basic_block bb;

  FOR_EACH_BB_FN (bb, fun)
    if (all_vd[bb->index].n_debug_insn_changes)
      {
	unsigned int regno;
	bitmap live;

	live = df_get_live_out (bb);
	for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	  if (all_vd[bb->index].e[regno].debug_insn_changes)
	    {
	      if (REGNO_REG_SET_P (live, regno))
		apply_debug_insn_changes (all_vd + bb->index, regno);

	      struct queued_debug_insn_change *cur;
	      for (cur = all_vd[bb->index].e[regno].debug_insn_changes;
		   cur; cur = cur->next)
		--all_vd[bb->index].n_debug_insn_changes;
	      all_vd[bb->index].e[regno].debug_insn_changes = NULL;
	      if (all_vd[bb->index].n_debug_insn_changes == 0)
		break;
	    }
      }

  queued_debug_insn_change_pool.release ();
}

unsigned int
pass_cprop_hardreg::execute (function *fun)
{
  struct value_data *all_vd;
  basic_block bb;

  all_vd = XNEWVEC (struct value_data, last_basic_block_for_fn (fun));

  auto_sbitmap visited (last_basic_block_for_fn (fun));
  bitmap_clear (visited);

  auto_vec<int> worklist1, worklist2;
  auto_vec<int> *curr = &worklist1;
  auto_vec<int> *next = &worklist2;
  bool any_debug_changes = false;

  /* We need accurate notes.  Earlier passes such as if-conversion may
     leave notes in an inconsistent state.  */
  df_note_add_problem ();
  df_analyze ();

  /* It is tempting to set DF_LR_RUN_DCE, but DCE may choose to delete
     an insn and this pass would not have visibility into the removal.
     This pass would then potentially use the source of that
     INSN for propagation purposes, generating invalid code.

     So we just ask for updated notes and handle trivial deletions
     within this pass where we can update this passes internal
     data structures appropriately.  */
  df_set_flags (DF_DEFER_INSN_RESCAN);

  FOR_EACH_BB_FN (bb, fun)
    {
      if (cprop_hardreg_bb (bb, all_vd, visited))
	curr->safe_push (bb->index);
      if (all_vd[bb->index].n_debug_insn_changes)
	any_debug_changes = true;
    }

  /* We must call df_analyze here unconditionally to ensure that the
     REG_UNUSED and REG_DEAD notes are consistent with and without -g.  */
  df_analyze ();

  if (MAY_HAVE_DEBUG_BIND_INSNS && any_debug_changes)
    cprop_hardreg_debug (fun, all_vd);

  /* Repeat pass up to PASSES times, but only processing basic blocks
     that have changed on the previous iteration.  CURR points to the
     current worklist, and each iteration populates the NEXT worklist,
     swapping pointers after each cycle.  */

  unsigned int passes = optimize > 1 ? 3 : 2;
  for (unsigned int pass = 2; pass <= passes && !curr->is_empty (); pass++)
    {
      any_debug_changes = false;
      bitmap_clear (visited);
      next->truncate (0);
      for (int index : *curr)
	{
	  bb = BASIC_BLOCK_FOR_FN (fun, index);
          if (cprop_hardreg_bb (bb, all_vd, visited))
	    next->safe_push (bb->index);
	  if (all_vd[bb->index].n_debug_insn_changes)
	    any_debug_changes = true;
	}

      df_analyze ();
      if (MAY_HAVE_DEBUG_BIND_INSNS && any_debug_changes)
	cprop_hardreg_debug (fun, all_vd);
      std::swap (curr, next);
    }

  free (all_vd);
  return 0;
}

} // anon namespace

rtl_opt_pass *
make_pass_cprop_hardreg (gcc::context *ctxt)
{
  return new pass_cprop_hardreg (ctxt);
}
