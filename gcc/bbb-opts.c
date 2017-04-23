/* Bebbo's Optimizations.
 Copyright (C) 2010-2017 Free Software Foundation, Inc.
 Copyright (C) 2017 Stefan "Bebbo" Franke.

 This file is part of GCC.

 GCC is free software; you can redistribute it and/or modify it under
 the terms of the GNU General Public License as published by the Free
 Software Foundation; either version 3, or (at your option) any later
 version.

 GCC is distributed in the hope that it will be useful, but WITHOUT ANY
 WARRANTY; without even the implied warranty of MERCHANTABILITY or
 FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 for more details.

 You should have received a copy of the GNU General Public License
 along with GCC; see the file COPYING3.  If not see
 <http://www.gnu.org/licenses/>.  */

/**
 * SBF (Stefan "Bebbo" Franke):
 *
 * This pass performs multiple optimizations.
 *
 * #1 propagate_moves
 * check if a->b->a can be moved out of a loop.
 *
 * #2 strcpy
 * check if a temp reg can be eliminated.
 *
 * #3 const_comp_sub
 * convert a compare with int constant into sub statement.
 *
 * #4 merge_add
 * merge adds
 *
 * #5 elim_dead_assign
 * eliminate some dead assignments.
 *
 * #6 shrink stack frame
 * remove push/pop for unused variables
 *
 * #7 rename register
 * rename registers without breaking register parameters, inline asm etc.
 *
 * Lessons learned:
 *
 * - do not trust existing code, better delete insns and inster a new one.
 * - do not modify insns, create new insns from pattern
 * - do not reuse registers, create new reg rtx instances
 *
 */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "df.h"
#include "tm_p.h"
#include "insn-config.h"
#include "recog.h"
#include "cfgrtl.h"
#include "emit-rtl.h"
#include "tree.h"
#include "tree-pass.h"
#include "conditions.h"
#include "cselib.h"
#include "langhooks.h"
#include <vector>
#include <set>
#include <map>

static bool be_verbose;

extern struct lang_hooks lang_hooks;

/* Lookup of the current function name. */
extern tree current_function_decl;
static tree last_function_decl;
static char const *
get_current_function_name ()
{
  static char fxname[512];
  if (current_function_decl == NULL)
    strcpy (fxname, "<toplevel>");
  else
    strcpy (fxname, lang_hooks.decl_printable_name (current_function_decl, 2));
  return fxname;
}

/* a simple log to stdout. */
static int
log (char const * fmt, ...)
{
  if (!be_verbose)
    return 0;

  va_list args;
  va_start(args, fmt);
  if (last_function_decl != current_function_decl)
    {
      last_function_decl = current_function_decl;
      printf (":bbb: in '%s'\n", get_current_function_name ());
    }
  printf (":bbb: ");
  int retval = vprintf (fmt, args);
  va_end(args);
  fflush (stdout);
  return retval;
}

/* Information for each insn to detect alive registers. Enough for m68k.
 * Why a class? Maybe extend it for general usage.
 *
 * Track use & def separate to determine starting points.
 */
struct insn_info
{
  unsigned _use;  // bit set if registers are used
  unsigned _def;  // bit set if registers are defined
  unsigned _hard; // bit set if registers can't be renamed

  insn_info () :
      _use (0), _def (0), _hard (0)
  {
  }

  inline void
  reset ()
  {
    _use = 0;
    _def = 0;
    _hard = 0;
  }

  inline void
  use (int regno)
  {
    _use |= 1 << regno;
  }

  inline void
  def (int regno)
  {
    _def |= 1 << regno;
  }

  inline void
  hard (int regno)
  {
    _hard |= 1 << regno;
  }

  inline void
  unset (int regno)
  {
    _use &= ~(1 << regno);
    _def &= ~(1 << regno);
    _hard &= ~(1 << regno);
  }

  inline bool
  is_use (int regno)
  {
    return (_use & (1 << regno)) != 0;
  }

  inline bool
  is_def (int regno)
  {
    return (_def & (1 << regno)) != 0;
  }

  inline bool
  is_hard (int regno)
  {
    return (_hard & (1 << regno)) != 0;
  }

  inline insn_info
  operator | (insn_info const & o) const
  {
    insn_info t;
    t._use = _use | o._use;
    t._def = _def | o._def;
    t._hard = _hard | o._hard;
    return t;
  }

  inline insn_info &
  operator |= (insn_info const & o)
  {
    _use |= o._use;
    _def |= o._def;
    _hard |= o._hard;
    return *this;
  }

  /*
   * update for previous insn.
   * - remove regs which are defined here
   * - add regs which are used here
   * - reset _def
   * - restrain _hard to used
   */
  inline void
  updateWith (insn_info const & o)
  {
    _use &= ~o._def;
    _use |= o._use;
    _def = 0;
    _hard &= ~_use;
  }

  inline bool
  operator == (insn_info const & o)
  {
    return _use == o._use;
  }

  inline insn_info
  operator ~ () const
  {
    insn_info t;
    t._use = ~_use;
    t._def = ~_def;
    t._hard = ~_hard;
    return t;
  }

  inline bool
  contains (insn_info const & o) const
  {
    if (o._def & ~_def)
      return false;
    if (o._use & ~_use)
      return false;
    if (o._hard & ~_hard)
      return false;
    return true;
  }

  void
  scan (rtx);

  /* return bits for alternate free registers. */
  unsigned
  get_free_mask () const
  {
    if (_def & _hard)
      return 0;

    if (!_def || _def > 0x1000)
      return 0;

    unsigned mask = _def - 1;
    /* more than one register -> don't touch. */
    if ((mask & ~_def) != mask)
      return 0;

    if (_def > 0xff)
      mask &= 0xff00;

    return mask & ~_use;
  }
};

/* scan rtx for registers and set the corresponding flags. */
void
insn_info::scan (rtx x)
{
  if (REG_P(x))
    {
      for (int n = REG_NREGS(x), r = REGNO(x); n > 0; --n, ++r)
	use (r);
      return;
    }

  if (x == cc0_rtx)
    {
      use (FIRST_PSEUDO_REGISTER);
      return;
    }

  RTX_CODE code = GET_CODE(x);

  /* handle SET and record use and def. */
  if (code == SET)
    {
      unsigned u = _use;
      scan (SET_DEST(x));
      if (REG_P(SET_DEST(x)))
	{
	  _def |= _use;
	  _use = u;
	}
      scan (SET_SRC(x));
      int code = GET_CODE(SET_SRC(x));
      if (code == ASM_OPERANDS)
	_use = _hard |= _def | _use;
      return;
    }

  const char *fmt = GET_RTX_FORMAT(code);
  for (int i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	scan (XEXP(x, i));
      else if (fmt[i] == 'E')
	for (int j = XVECLEN (x, i) - 1; j >= 0; j--)
	  scan (XVECEXP(x, i, j));
    }
}

/* create a copy for a reg. Optional specify a new register number. */
static rtx
copy_reg (rtx reg, int newregno)
{
  if (newregno < 0)
    newregno = REGNO(reg);
  rtx x = gen_raw_REG (GET_MODE(reg), newregno);
  x->jump = reg->jump;
  x->call = reg->call;
  x->unchanging = reg->unchanging;
  x->volatil = reg->volatil;
  x->in_struct = reg->in_struct;
  x->used = reg->used;
  x->frame_related = reg->frame_related;
  x->return_val = reg->return_val;

  x->u.reg.attrs = reg->u.reg.attrs;
  return x;
}

/* Rename the register plus track all locs to undo these changes. */
static void
temp_reg_rename (std::vector<std::pair<rtx *, rtx> > & loc, rtx x, unsigned oldregno, unsigned newregno)
{
  RTX_CODE code = GET_CODE(x);

  const char *fmt = GET_RTX_FORMAT(code);
  for (int i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  rtx y = XEXP(x, i);
	  if (REG_P(y))
	    {
	      if (REGNO(y) == oldregno)
		{
		  rtx z = copy_reg (y, newregno);
		  loc.push_back (std::make_pair (&XEXP(x, i), y));
		  XEXP(x, i) = z;
		}
	    }
	  else
	    temp_reg_rename (loc, y, oldregno, newregno);
	}
      else if (fmt[i] == 'E')
	for (int j = XVECLEN (x, i) - 1; j >= 0; j--)
	  temp_reg_rename (loc, XVECEXP(x, i, j), oldregno, newregno);
    }
}

/*
 * Collect some data.
 */
static std::vector<rtx_insn *> insns;
static std::vector<char> proepilogue;
static std::vector<rtx_insn *> temp;
static std::vector<rtx_insn *> jumps;
static std::map<rtx_insn *, unsigned> insn2index;
static std::vector<insn_info> infos;

/*
 * Reset collected data.
 */
static void
clear (void)
{
  insns.clear ();
  proepilogue.clear ();
  jumps.clear ();
  insn2index.clear ();
  infos.clear ();
}

/*
 *  return true if the register is DEAD.
 *  Do not check at jumps.
 */
static bool
is_reg_dead (unsigned regno, unsigned _pos)
{
  // skip labels.
  for (unsigned pos = _pos + 1; pos < infos.size (); ++pos)
    {
      insn_info & ii0 = infos[pos];
      // skip entries without info
      if (!ii0._def && !ii0._use && !ii0._hard)
	continue;

      // not dead if usage is reported in the next statement
      return !ii0.is_use (regno) && !ii0.is_hard (regno);
    }
  return true;
}

/*
 * Helper function to dump the code.
 * Sometimes used during debugging.
 */
static void
dump_insns (char const * name, bool all)
{
  fprintf (stderr, "====================================: %s\n", name);
  if (all)
    {
      for (rtx_insn * insn = get_insns (); insn && insn != insns[0]; insn = NEXT_INSN (insn))
	debug_rtx (insn);
    }
  for (unsigned i = 0; i < insns.size (); ++i)
    {
      fprintf (stderr, "%d: ", i);

      if (i < infos.size ())
	{
	  insn_info & ii = infos[i];

	  for (int j = 0; j < 8; ++j)
	    if (ii.is_use (j) || ii.is_def (j))
	      {
		if (ii.is_hard (j))
		  fprintf (stderr, "!");
		if (ii.is_def (j))
		  fprintf (stderr, "*");
		fprintf (stderr, "d%d ", j);
	      }

	  for (int j = 8; j < 16; ++j)
	    if (ii.is_use (j) || ii.is_def (j))
	      {
		if (ii.is_hard (j))
		  fprintf (stderr, "!");
		if (ii.is_def (j))
		  fprintf (stderr, "*");
		fprintf (stderr, "a%d ", j - 8);
	      }

	  if (ii.is_use (FIRST_PSEUDO_REGISTER))
	    fprintf (stderr, ii.is_def (FIRST_PSEUDO_REGISTER) ? "*cc " : "cc ");

	}

      fprintf (stderr, "\t");
      debug_rtx (insns[i]);

      if (all)
	{
	  rtx_insn * p = i + 1 < insns.size () ? insns[i + 1] : 0;
	  for (rtx_insn * q = NEXT_INSN (insns[i]); q && q != p; q = NEXT_INSN (q))
	    debug_rtx (q);
	}
    }
}

/*
 * Create a filtered view of insns - keep only those to work with.
 */
static void
update_insns ()
{
  rtx_insn *insn, *next;
  clear ();

  // df_insn_rescan_all ();

  char inproepilogue = 1;
  /* create a vector with relevant insn. */
  for (insn = get_insns (); insn; insn = next)
    {
      next = NEXT_INSN (insn);

      if (NONJUMP_INSN_P (insn) || LABEL_P(insn) || JUMP_P(insn) || CALL_P(insn))
	{
	  if (JUMP_P(insn))
	    jumps.push_back (insn);

	  insn2index.insert (std::make_pair (insn, insns.size ()));
	  insns.push_back (insn);
	  proepilogue.push_back (inproepilogue);

	  if (JUMP_P(insn))
	    inproepilogue = 0;
	}

      if (NOTE_P(insn))
	{
	  if (NOTE_KIND(insn) == NOTE_INSN_PROLOGUE_END)
	    inproepilogue = 0;
	  else if (NOTE_KIND(insn) == NOTE_INSN_EPILOGUE_BEG)
	    inproepilogue = 2;
	}
    }
}

/* This is the important function to track register usage plus hard/live state.
 *
 * Start at bottom and work upwards. On all labels trigger all jumps referring to this label.
 * A set destination into a register is a def. All other register references are an use.
 * Hard registers cann't be renamed and are mandatory for regparms and asm_operands.
 */
static void
update_insn_infos (void)
{
  /* prepare insn_info */
  infos.resize (insns.size ());

  /* own analyze reg life */
  std::vector<std::pair<unsigned, insn_info> > todo;
  todo.push_back (std::make_pair (insns.size () - 1, insn_info ()));

  int pass = 0;
  while (!todo.empty ())
    {
      std::pair<unsigned, insn_info> p = *todo.rbegin ();
      todo.pop_back ();

      insn_info ii = p.second;

      for (int pos = p.first; pos >= 0; --pos)
	{
	  rtx_insn * insn = insns[pos];
	  /* can be NULL as used in opt_shrink_stack_frame(). */
	  if (!insn)
	    continue;

	  /* no new information -> break. */
	  if (pass && infos[pos].contains (ii))
	    break;

	  ii |= infos[pos];

	  if (LABEL_P(insn))
	    {
	      /* work on all jumps referring to that label. */
	      for (std::vector<rtx_insn *>::iterator i = jumps.begin (); i != jumps.end (); ++i)
		{
		  if (JUMP_LABEL(*i) == insn)
		    {
		      std::map<rtx_insn *, unsigned>::iterator j = insn2index.find (*i);
		      if (j != insn2index.end ())
			todo.push_back (std::make_pair (j->second, ii));
		    }
		}
	      continue;
	    }

	  rtx pattern = PATTERN (insn);

	  if (CALL_P(insn))
	    {
	      insn_info use;

	      /* add mregparm registers. */
	      for (rtx link = CALL_INSN_FUNCTION_USAGE(insn); link; link = XEXP(link, 1))
		{
		  rtx op, reg;

		  if (GET_CODE (op = XEXP (link, 0)) == USE && REG_P(reg = XEXP (op, 0)))
		    for (unsigned r = REGNO(reg); r <= END_REGNO (reg); ++r)
		      use.use (r);
		}
	      /* also mark all registers as not renamable */
	      use._hard = use._use;

	      use.scan (pattern);

	      /* mark scratch registers. */
	      use.def (0);
	      use.def (1);
	      use.def (8);
	      use.def (9);

	      infos[pos] = use | ii;
	      ii.updateWith (use);

	      continue;
	    }

	  if (JUMP_P(insn))
	    {
	      insn_info use;
	      if (ANY_RETURN_P(pattern))
		{
		  tree type = TYPE_SIZE(TREE_TYPE (DECL_RESULT (current_function_decl)));
		  int sz = type ? TREE_INT_CST_LOW(type) : 0;
		  // log ("return size %d\n", sz);
		  if (sz <= 64)
		    {
		      use.hard (0);
		      use.use (0);
		      if (sz > 32)
			{
			  use.hard (1);
			  use.use (1);
			}
		    }
		  ii.reset ();
		}

	      use.scan (pattern);
	      infos[pos] = use | ii;
	      ii.updateWith (use);

	      continue;
	    }

	  if (GET_CODE (pattern) == USE)
	    {
	      rtx x = XEXP(pattern, 0);
	      if (REG_P(x))
		{
		  ii.use (REGNO(x));
		  ii.hard (REGNO(x));
		}
	      infos[pos] = ii;
	      continue;
	    }

	  if (GET_CODE (pattern) == CLOBBER)
	    {
	      /* mark regs as use and def */
	      insn_info use;
	      use.scan (pattern);
	      use._hard = use._use = use._def = use._use | use._def;
	      infos[pos] = use | ii;
	      ii.updateWith (use);
	      continue;
	    }

	  insn_info use;
	  use.scan (pattern);
	  if (single_set (insn) == 0)
	    use._hard = use._use | use._def;

	  /* if not cc0 defined check for mod. */
	  if (!use.is_def (FIRST_PSEUDO_REGISTER))
	    {
	      CC_STATUS_INIT;
	      NOTICE_UPDATE_CC(PATTERN (insn), insn);
	      if (cc_status.value1 || cc_status.value2)
		use.def (FIRST_PSEUDO_REGISTER);
	    }

	  /* mark not renameable in prologue/epilogue. */
	  if (proepilogue[pos])
	    use._hard = use._use | use._def;

	  ii._use &= ~use._def;
	  infos[pos] = use | ii;
	  ii.updateWith (use);
	}
      ++pass;
    }
}

/* convert the lowest set bit into a register number. */
static int
bit2regno (unsigned bit)
{
  if (!bit)
    return -1;

  unsigned regno = 0;
  while (!(bit & 1))
    {
      ++regno;
      bit >>= 1;
    }
  return regno;
}

/*
 * Always prefer lower register numbers within the class.
 */
static unsigned
opt_reg_rename (void)
{
//  dump_insns ("rename", 1);
  for (unsigned index = 0; index < insns.size (); ++index)
    {
      insn_info & ii = infos[index];

      /* do not rename if register is hard or used in same statement. */
      const unsigned toRename = ii._def & ~ii._hard & ~ii._use;
      if (!toRename)
	continue;

      /* get the mask for free registers. */
      unsigned mask = ii.get_free_mask ();
      if (!mask)
	continue;

      std::set<unsigned> found;
      std::vector<unsigned> todo;
      if (index + 1 < insns.size ())
	todo.push_back (index + 1);

      found.insert (index);
      /* a register was defined, follow all branches. */
      while (todo.size ())
	{
	  unsigned pos = todo[todo.size () - 1];
	  todo.pop_back ();

	  if (found.find (pos) != found.end ())
	    continue;

	  if (LABEL_P(insns[pos]))
	    {
	      if (pos + 1 < insns.size ())
		todo.push_back (pos + 1);
	      continue;
	    }

	  insn_info & jj = infos[pos];

	  /* marked as hard reg -> invalid rename */
	  if (jj._hard & toRename)
	    mask = 0;

	  /* defined again -> invalid rename */
	  if ((jj._def & toRename) && !(jj._use & toRename))
	    mask = 0;

	  if (!mask)
	    break;

	  /* not used. */
	  if (!(jj._use & toRename))
	    continue;

	  /* update free regs. */
	  mask &= ~jj._use;
	  mask &= ~jj._def;
	  if (!mask)
	    break;

	  found.insert (pos);

	  /* follow jump and/or next insn. */
	  rtx_insn * insn = insns[pos];
	  if (JUMP_P(insn))
	    {
	      std::map<rtx_insn *, unsigned>::iterator j = insn2index.find ((rtx_insn *) JUMP_LABEL(insn));
	      if (j != insn2index.end ())
		todo.push_back (j->second);

	      rtx jmppattern = PATTERN (insn);
	      if (GET_CODE(jmppattern) == PARALLEL)
		{
		  return 0; /* can't handle yet. Abort renaming. */
		}

	      rtx jmpsrc = XEXP(jmppattern, 1);
	      if (jmpsrc && GET_CODE(jmpsrc) == IF_THEN_ELSE)
		if (pos + 1 < insns.size ())
		  todo.push_back (pos + 1);
	    }
	  else if (pos + 1 < insns.size ())
	    todo.push_back (pos + 1);
	}

      if (mask)
	{
	  int oldregno = bit2regno (toRename);
	  int newregno = bit2regno (mask);

	  /* check the renamed insns. */
	  std::vector<std::pair<rtx *, rtx> > locs;
	  std::vector<std::pair<rtx *, rtx> > patch;
	  bool ok = true;

	  for (std::set<unsigned>::iterator i = found.begin (); ok && i != found.end (); ++i)
	    {
	      rtx_insn * insn = insns[*i];

	      /* temp rename. */
	      temp_reg_rename (locs, PATTERN (insn), oldregno, newregno);
	      if (!locs.empty ())
		{
		  int num_clobbers_to_add = 0;
		  int insn_code_number = recog (PATTERN (insn), insn, &num_clobbers_to_add);
		  if (insn_code_number < 0 || !check_asm_operands (PATTERN (insn)))
		    ok = false;

		  /* undo temp change but keep loc and new register. */
		  for (std::vector<std::pair<rtx *, rtx> >::iterator j = locs.begin (); j != locs.end (); ++j)
		    {
		      patch.push_back (std::make_pair (j->first, *j->first));
		      *j->first = j->second;
		    }

		  locs.clear ();
		}
	    }

	  if (!ok)
	    continue;

	  log ("opt_reg_rename %s -> %s (%d locs)\n", reg_names[oldregno], reg_names[newregno], patch.size ());

	  /* apply all changes. */
	  for (std::vector<std::pair<rtx *, rtx> >::iterator j = patch.begin (); j != patch.end (); ++j)
	    *j->first = j->second;

	  return 1;
	}
    }
  return 0;
}

/*
 *  #1 propagate a->b->a moves out of a loop.
 *
 * consider a loop:
 *
 * .L1
 *   ...
 *   move d0, a0 ; (1)
 *   ...
 *   move xy, (a0)+
 *   ...
 *   move a0, d0 ; (2)
 *   ...
 *   jxx .L1
 *
 *  Then the statements (1) and (2) can be moved out of the loop:
 *
 *   move d0, a0 ; (3)
 * .L1
 *   ...
 *   move *, (a0)+ ; a0 is modified somehow
 *   ...
 *   jxx .L1
 *   move a0, d0 ; (4)
 *
 *  if all criteria are met:
 *
 *  a) no other jump to .L1 -> (LABEL_NUSES(insn) == 1)
 *  b) no other use of d0 inside the loop
 *  c) no other use of a0 before (1)
 *  d) no other use of a1 after (2)
 *
 *  Optional:
 *  - omit (4) if d0 is dead
 *
 *  this will e.g. convert
 .L6:
 move.l d0,a1
 move.b (a1)+,d1
 move.l a1,d0
 move.b d1,(a0)+
 cmp.b #0, d1
 jne .L6
 *  to
 move.l d0,a1
 .L6:
 move.b (a1)+,d1
 move.b d1,(a0)+
 cmp.b #0, d1
 jne .L6

 *
 * Also allow exit jumps, if the modification of the reg is const
 * and insert a correction after the exit label.
 * The label must only be reachable by the exit jump.
 */
static unsigned
opt_propagate_moves ()
{
  unsigned change_count = 0;
  rtx_insn * current_label = 0;
  unsigned current_label_index;
  std::vector<unsigned> reg_reg;
  std::vector<rtx_insn *> jump_out;

  /* start at 1 since there must be an insn before the label. */
  for (unsigned index = 1; index < insns.size (); ++index)
    {
      rtx_insn * insn = insns[index];

      if (LABEL_P(insn))
	{
	  if (LABEL_NUSES(insn) == 1)
	    {
	      current_label = insn;
	      current_label_index = index;
	      reg_reg.clear ();
	      jump_out.clear ();
	    }
	  else
	    current_label = 0;
	}

      if (current_label == 0)
	continue;

      if (NONJUMP_INSN_P(insn))
	{
	  // check for set reg, reg
	  rtx set = single_set (insn);
	  if (set)
	    {
	      rtx src = SET_SRC(set);
	      rtx dst = SET_DEST(set);
	      if (REG_P(src) && REG_P(dst))
		reg_reg.push_back (index);
	    }
	  else
	    current_label = 0;

	  continue;
	}

      if (JUMP_P(insn))
	{
	  rtx_insn * label = (rtx_insn *) JUMP_LABEL(insn);
	  if (label != current_label)
	    {
	      /* collect the labels for a later check if a fixup is possible. */
	      if (LABEL_NUSES(label) == 1 && BARRIER_P(PREV_INSN (label)))
		jump_out.push_back (label);
	      else
		current_label = 0;
	      continue;
	    }

	  if (reg_reg.size () > 1)
	    {
	      /* Search for reg/reg pairs. */
	      for (std::vector<unsigned>::iterator i = reg_reg.begin (); i != reg_reg.end () && i + 1 != reg_reg.end ();
		  )
		{
		  bool inc = true;
		  for (std::vector<unsigned>::iterator j = i + 1; j != reg_reg.end ();)
		    {
		      rtx_insn * ii = insns[*i];
		      rtx seti = single_set (ii);
		      rtx srci = SET_SRC(seti);
		      rtx dsti = SET_DEST(seti);
		      rtx_insn * jj = insns[*j];
		      rtx setj = single_set (jj);
		      rtx srcj = SET_SRC(setj);
		      rtx dstj = SET_DEST(setj);

		      if (rtx_equal_p (srci, dstj) && rtx_equal_p (srcj, dsti))
			{
			  /* Ensure correct usage. */
			  if (!reg_used_between_p (srci, current_label, ii) && !reg_used_between_p (srci, ii, jj)
			      && !reg_used_between_p (srci, jj, insn) && !reg_used_between_p (dsti, current_label, ii)
			      && !reg_used_between_p (dsti, jj, insn))
			    {
			      std::vector<int> fixups;

			      /* if there are jumps out of the loop,
			       * check if the modification occurs before the jump,
			       * and if, that it's a plus const.
			       */
			      if (jump_out.size ())
				{
				  std::vector<rtx_insn *>::iterator label_iter = jump_out.begin ();
				  int fixup = 0;

				  for (unsigned k = *i + 1; k != *j; ++k)
				    {
				      rtx_insn * check = insns[k];
				      if (JUMP_P(check))
					{
					  fixups.push_back (fixup);
					  if (++label_iter == jump_out.end ())
					    break;
					  continue;
					}

				      if (reg_overlap_mentioned_p (dsti, PATTERN (check)))
					{
					  /* right now only support auto_incs. */
					  rtx set = single_set (check);
					  rtx src = SET_SRC(set);
					  rtx dst = SET_DEST(set);

					  if (reg_overlap_mentioned_p (dsti, dst))
					    {
					      if (REG_P(dst))
						break;
					      if (!MEM_P(dst))
						break;

					      rtx x = XEXP(dst, 0);
					      if (GET_CODE(x) == REG)
						fixup += 0; // direct use
					      else if (GET_CODE(x) == PRE_INC ||
					      GET_CODE(x) == POST_INC)
						fixup -= GET_MODE_SIZE(GET_MODE(dst));
					      else if (GET_CODE(dst) == PRE_DEC ||
					      GET_CODE(dst) == POST_DEC)
						fixup += GET_MODE_SIZE(GET_MODE(dst));
					      else
						break;
					    }

					  if (reg_overlap_mentioned_p (dsti, src))
					    {
					      if (REG_P(src))
						fixup += 0;
					      else
						{
						  if (!MEM_P(src))
						    break;

						  rtx x = XEXP(src, 0);
						  if (GET_CODE(x) == REG)
						    fixup += 0; // direct use
						  else if (GET_CODE(x) == PRE_INC ||
						  GET_CODE(x) == POST_INC)
						    fixup -= GET_MODE_SIZE(GET_MODE(dst));
						  else if (GET_CODE(dst) == PRE_DEC ||
						  GET_CODE(dst) == POST_DEC)
						    fixup += GET_MODE_SIZE(GET_MODE(dst));
						  else
						    break;
						}
					    }
					}
				    }
				}

			      /* got a fixup for all jump_outs? */
			      if (fixups.size () == jump_out.size ())
				{
				  rtx_insn * before = insns[current_label_index - 1];
				  rtx_insn * after = insns[index + 1];
				  rtx bset = single_set (before);

				  log ("propagate_moves condition met, moving regs %s, %s\n",
				  reg_names[REGNO(srci)],
				       reg_names[REGNO(dsti)]);

				  /* Move in front of loop and mark as dead. */
				  rtx_insn * newii = make_insn_raw (PATTERN (ii));
				  SET_INSN_DELETED(ii);

				  /* Plus check if the reg was just loaded. */
				  if (bset)
				    {
				      rtx bdst = SET_DEST(bset);
				      if (REG_P(bdst) && REGNO(bdst) == REGNO(srci))
					{
					  SET_SRC(PATTERN(newii)) = SET_SRC(bset);
//					  SET_INSN_DELETED(ii);
					}
				    }
				  else
				    add_reg_note (newii, REG_DEAD, srci);

				  add_insn_after (newii, before, 0);

				  /* Move behind loop - into next BB. */
				  rtx_insn * newjj = make_insn_raw (PATTERN (jj));
				  add_insn_before (newjj, after, 0);
				  SET_INSN_DELETED(jj);

				  reg_reg.erase (j);
				  reg_reg.erase (i);
				  j = reg_reg.end ();
				  inc = false;

				  // df_insn_rescan (newii);
				  // df_insn_rescan (newjj);

				  /* add fixes if there were jumps out of the loop. */
				  if (jump_out.size ())
				    {
				      log ("propagate_moves fixing %d jump outs\n", jump_out.size ());

				      for (unsigned k = 0; k < jump_out.size (); ++k)
					{
					  rtx neu = gen_rtx_SET(
					      dstj, gen_rtx_PLUS(Pmode, dsti, gen_rtx_CONST_INT(Pmode, fixups[k])));
					  rtx_insn * neui = emit_insn_after (neu, jump_out[k]);
					  // df_insn_rescan (neui);
					}
				    }
				  ++change_count;
				}
			    }
			}
		      if (inc)
			++j;
		    }
		  if (inc)
		    ++i;
		}
	    }
	  current_label = 0;
	}
    }
  return change_count;
}

/**
 * Search for
 *
 *   mov x,reg
 *   mov reg,x
 *   cmp #0, reg
 *   jxx
 *
 * patterns.
 *
 * Use a simple state machine to find the patterns.
 */
static unsigned
opt_strcpy ()
{
  unsigned change_count = 0;
#if HAVE_cc0
  rtx_insn * x2reg = 0;
  rtx_insn * reg2x;
  unsigned int regno;

  for (unsigned index = 0; index < insns.size (); ++index)
    {
      rtx_insn * insn = insns[index];

      if (!NONJUMP_INSN_P(insn))
	{
	  x2reg = 0;
	  continue;
	}

      rtx set = single_set (insn);
      if (!set)
	{
	  x2reg = 0;
	  continue;
	}

      if (x2reg && reg2x)
	{
	  rtx src = SET_SRC(set);
	  if (GET_CODE(src) == COMPARE)
	    {
	      rtx dst = XEXP(src, 0);
	      src = XEXP(src, 1);

//	      if (CONST_INT_P(src) && INTVAL(src) == 0 && find_reg_note (insn, REG_DEAD, dst))
	      if (REG_P(dst) && CONST_INT_P(src) && INTVAL(src) == 0 && is_reg_dead (REGNO(dst), index))
		{
		  /* now check via NOTICE_UPDATE_CC*/
		  NOTICE_UPDATE_CC(PATTERN (reg2x), reg2x);
		  if (cc_status.flags == 0 && rtx_equal_p (dst, cc_status.value2))
		    {
		      int num_clobbers_to_add = 0;
		      int insn_code_number;

		      rtx_insn * newinsn = make_insn_raw (
			  gen_rtx_SET(SET_DEST(single_set(reg2x)), SET_SRC(single_set (x2reg))));
		      insn_code_number = recog (PATTERN (newinsn), newinsn, &num_clobbers_to_add);
		      if (insn_code_number >= 0 && check_asm_operands (PATTERN (newinsn)))
			{
			  rtx link;

			  log ("opt_strcpy condition met, removing compare and joining insns - omit reg %s\n",
			  reg_names[REGNO(dst)]);

			  SET_SRC(single_set(reg2x)) = SET_SRC(single_set (x2reg));

			  for (link = REG_NOTES(x2reg); link; link = XEXP(link, 1))
			    if (REG_NOTE_KIND (link) != REG_LABEL_OPERAND)
			      {
				if (GET_CODE (link) == EXPR_LIST)
				  add_reg_note (reg2x, REG_NOTE_KIND(link), copy_insn_1 (XEXP(link, 0)));
				else
				  add_shallow_copy_of_reg_note (reg2x, link);
			      }

			  SET_INSN_DELETED(x2reg);
			  SET_INSN_DELETED(insn);

			  // df_insn_rescan (reg2x);

			  ++change_count;
			}
		    }
		}
	      x2reg = 0;
	      continue;
	    }
	  reg2x = 0;
	}

      /* check for reg2x first, maybe fallback to x2reg. */
      if (x2reg && reg2x == 0)
	{
	  if (REG_P(SET_SRC(set)) && REGNO(SET_SRC(set)) == regno)
	    {
	      reg2x = insn;
	      continue;
	    }
	  x2reg = 0;
	}

      /* check for a match for x2reg. */
      if (x2reg == 0)
	{
	  if (REG_P(SET_DEST(set)))
	    {
	      x2reg = insn;
	      reg2x = 0;
	      regno = REGNO(SET_DEST(set));
	    }
	}
    }
#endif
  return change_count;
}

/*
 * convert
 *
 * set reg1, plus (reg2, const)
 * set mem(reg2), y
 *
 * ->
 * set reg1, reg2
 * set mem(reg1+), y
 *
 * if size of postinc == const
 *
 (insn 33 32 35 4 (set (reg/v/f:SI 8 a0 [orig:47 s ] [47])
 (plus:SI (reg/v/f:SI 9 a1 [orig:46 s ] [46])
 (const_int 1 [0x1]))) sn.c:5 141 {*addsi3_internal}
 (nil))
 (insn 36 35 37 4 (set (mem:QI (reg/v/f:SI 9 a1 [orig:46 s ] [46]) [0 MEM[base: s_17, offset: 4294967295B]+0 S1 A8])
 (mem:QI (post_inc:SI (reg/v/f:SI 10 a2 [orig:53 s2 ] [53])) [0 MEM[base: s2_19, offset: 4294967295B]+0 S1 A8])) sn.c:5 46 {*m68k.md:1083}
 (expr_list:REG_INC (reg/v/f:SI 10 a2 [orig:53 s2 ] [53])
 (nil)))
 */
static unsigned
opt_commute_add_move (void)
{
  unsigned change_count = 0;

  for (unsigned index = 0; index + 1 < insns.size (); ++index)
    {
      rtx_insn * insn = insns[index];
      rtx set = single_set (insn);
      if (!set)
	continue;

      rtx reg1dst = SET_DEST(set);
      if (!REG_P(reg1dst))
	continue;

      rtx plus = SET_SRC(set);
      if (GET_CODE(plus) != PLUS)
	continue;

      rtx reg1src = XEXP(plus, 0);
      if (!REG_P(reg1src) || reg1src == reg1dst)
	continue;

      rtx cnst = XEXP(plus, 1);
      if (!CONST_INT_P(cnst))
	continue;

      rtx_insn * next = insns[index + 1];
      rtx set2 = single_set (next);
      if (!set2)
	continue;

      rtx dst = SET_DEST(set2);
      if (!MEM_P(dst) || GET_MODE_SIZE(GET_MODE(dst)) != INTVAL(cnst))
	continue;

      rtx memreg = XEXP(dst, 0);
      if (!REG_P(memreg) || REGNO(memreg) != REGNO(reg1src))
	continue;

      rtx pinc = gen_rtx_POST_INC(GET_MODE(dst), reg1dst);
      rtx newmem = replace_equiv_address_nv (dst, pinc);

      rtx_insn * newinsn = make_insn_raw (gen_rtx_SET(reg1dst, reg1src));
      if (recog (PATTERN (newinsn), newinsn, 0) < 0 || !check_asm_operands (PATTERN (newinsn)))
	continue;

      if (validate_change (next, &SET_DEST(set2), newmem, 0))
	{
	  log ("commute_add_move found\n");

	  SET_INSN_DELETED(insn);

	  insn = emit_insn_before (PATTERN (newinsn), next);

	  add_reg_note (next, REG_INC, reg1dst);

	  // df_insn_rescan (insn);
	  // df_insn_rescan (next);

	  ++change_count;
	}
    }
  return change_count;
}

/*
 * Replace
 *
 * move x,dx
 * cmp  dx,dy
 *
 * if dx and dy are both dead after compare.
 *
 * with
 *
 * sub #n,dx
 *
 d0 d1 d2 a0 a1 a7       (insn 99 59 41 7 (set (reg:SI 2 d2)
 (const_int 1 [0x1])) sn.c:8 38 {*movsi_m68k}
 (nil))
 d0 d1 d2 a0 a1 a7       (insn 41 99 42 7 (set (cc0)
 (compare (reg/v:SI 1 d1 [orig:54 n ] [54])
 (reg:SI 2 d2))) sn.c:8 16 {*m68k.md:499}
 (expr_list:REG_DEAD (reg:SI 2 d2)
 (expr_list:REG_DEAD (reg/v:SI 1 d1 [orig:54 n ] [54])
 (nil))))
 *
 */
static unsigned
opt_const_cmp_to_sub (void)
{
  unsigned change_count = 0;
#if HAVE_cc0
  for (int index = insns.size () - 2; index > 0; --index)
    {
      rtx_insn * insn = insns[index];
      rtx seti = single_set (insn);
      if (!seti)
	continue;

      rtx dsti = SET_DEST(seti);
      if (dsti != cc0_rtx)
	continue;

      rtx srci = SET_SRC(seti);
      if (GET_CODE(srci) != COMPARE)
	continue;

      rtx left = XEXP(srci, 0);
      rtx right = XEXP(srci, 1);
      if (!REG_P(left) || !REG_P(right) || REG_NREGS(left) > 1 || REG_NREGS(right) > 1)
	continue;

      // TODO
      // FEATURE: check if the next uses are also a add/sub
      // then maybe that add/sub can be adjusted too

//      if (!find_reg_note (insn, REG_DEAD, left) || !find_reg_note (insn, REG_DEAD, right))
//      	continue;
      /* use own reg_dead - reg_notes seem to be inaccurate!? */
      if (!is_reg_dead (REGNO(left), index) || !is_reg_dead (REGNO(right), index))
	continue;

      // maybe add a search?
      rtx_insn * prev = insns[index - 1];
      rtx setp = single_set (prev);
      if (!setp)
	continue;

      rtx dstp = SET_DEST(setp);
      if (!REG_P(dstp))
	continue;

      rtx srcp = SET_SRC(setp);
      if (!CONST_INT_P(srcp))
	continue;

      int intval = -INTVAL(srcp);
      if (intval < -8 || intval > 7 || intval == 0)
	continue;

      enum machine_mode mode = GET_MODE(dstp);
      if (GET_MODE_SIZE(mode) > 4)
	continue;

      //     printf("mode size: %d\n", GET_MODE_SIZE(mode));

      rtx reg = dstp == left ? right : left;
      rtx plus = gen_rtx_PLUS(mode, copy_reg (reg, -1), gen_rtx_CONST_INT (mode, intval));

      rtx_insn * neuprev = make_insn_raw (gen_rtx_SET(copy_reg (reg, -1), plus));

      int num_clobbers_to_add = 0;
      int insn_code_number = recog (PATTERN (neuprev), neuprev, &num_clobbers_to_add);
      if (insn_code_number < 0 || !check_asm_operands (PATTERN (neuprev)))
	continue;

      // also convert current statement to cmp #0, reg
      SET_INSN_DELETED(insn);
      rtx neu = gen_rtx_SET(cc0_rtx, gen_rtx_COMPARE(mode, copy_reg(reg, -1), gen_rtx_CONST_INT(mode, 0)));
      insn = emit_insn_after (neu, prev);
      add_reg_note (insn, REG_DEAD, reg);

      SET_INSN_DELETED(prev);
      prev = emit_insn_before (PATTERN (neuprev), insn);

      log ("const_cmp_to_sub replaced reg-reg compare with sub\n");

      if (dstp != left)
	{
	  // invert all conditions using this statement.
	  std::vector<unsigned> todo;
	  std::vector<unsigned> done;
	  done.resize (insns.size ());
	  todo.push_back (index + 1);

	  while (todo.size ())
	    {
	      unsigned pos = todo[todo.size () - 1];
	      todo.pop_back ();

	      if (done[pos])
		continue;

	      done[pos] = 1;

	      if (infos[pos].is_def (FIRST_PSEUDO_REGISTER))
		continue;

	      if (pos + 1 < infos.size ())
		todo.push_back (pos + 1);

	      rtx_insn * patchme = insns[pos];
	      if (!JUMP_P(insn))
		continue;

	      std::map<rtx_insn *, unsigned>::iterator j = insn2index.find ((rtx_insn *) JUMP_LABEL(patchme));
	      if (j != insn2index.end ())
		todo.push_back (j->second);

	      rtx jmppattern = PATTERN (patchme);

	      rtx jmpsrc = XEXP(jmppattern, 1);
	      if (GET_CODE(jmpsrc) == IF_THEN_ELSE)
		{
		  rtx condition = XEXP(jmpsrc, 0);
		  RTX_CODE code = GET_CODE(condition);
		  RTX_CODE newcode = code;
		  if (code == GE)
		    newcode = LE;
		  else if (code == GT)
		    newcode = LT;
		  else if (code == LT)
		    newcode = GT;
		  else if (code == LE)
		    newcode = GE;
		  else if (code == GEU)
		    newcode = LEU;
		  else if (code == GTU)
		    newcode = LTU;
		  else if (code == LTU)
		    newcode = GTU;
		  else if (code == LEU)
		    newcode = GEU;

		  if (code != newcode)
		    {
		      log ("patch jcc %d -> %d\n", code, newcode);
		      XEXP(jmpsrc, 0) = gen_rtx_fmt_ee(newcode, VOIDmode, XEXP(condition, 0), XEXP(condition, 1));
		    }
		}
	    }
	}

      ++change_count;
    }
#endif
  return change_count;
}

/*
 * Some optimizations (e.g. propagate_moves) might result into an unuses assignment behind the loop.
 * delete those insns.
 */
static unsigned
opt_elim_dead_assign (void)
{
  unsigned change_count = 0;
  for (unsigned index = 0; index + 1 < insns.size (); ++index)
    {
      rtx_insn * insn = insns[index];
      if (!NONJUMP_INSN_P(insn))
	continue;

      rtx set = single_set (insn);
      if (!set)
	continue;

      rtx src = SET_SRC(set);
      rtx dst = SET_DEST(set);
      if (!REG_P(dst) || !REG_P(src))
	continue;

      if (is_reg_dead (REGNO(dst), index))
	{
	  log ("elim_dead_assign to %s\n", reg_names[REGNO(dst)]);
	  SET_INSN_DELETED(insn);
	  ++change_count;
	}
    }
  return change_count;
}

/*
 * rare and only little gain - but :-)
 lea (-1,a0),a1
 add.l d1,a1
 subq.l #1,d1
 ->
 move.l a0,a1
 subq.l #1,d1
 add.l d1,a1
 */
static unsigned
opt_merge_add (void)
{
  unsigned change_count = 0;
  for (unsigned index = 0; index + 2 < insns.size (); ++index)
    {
      rtx_insn * ins1 = insns[index];
      rtx_insn * ins2 = insns[index + 1];
      rtx_insn * ins3 = insns[index + 2];
      if (!NONJUMP_INSN_P(ins1) && !NONJUMP_INSN_P(ins2) && !NONJUMP_INSN_P(ins3))
	continue;

      rtx set1 = single_set (ins1);
      rtx set2 = single_set (ins2);
      rtx set3 = single_set (ins3);
      if (!set1 || !set2 || !set3)
	continue;

      rtx dst1 = SET_DEST(set1);
      rtx dst2 = SET_DEST(set2);
      rtx dst3 = SET_DEST(set3);
      if (!REG_P(dst1) || !REG_P(dst2) || !REG_P(dst3))
	continue;

      CC_STATUS_INIT;
      NOTICE_UPDATE_CC(PATTERN (ins2), ins2);
      if (cc_status.value1 || cc_status.value2)
	continue;

      rtx src1 = SET_SRC(set1);
      rtx src2 = SET_SRC(set2);
      rtx src3 = SET_SRC(set3);
      if (GET_CODE(src1) != PLUS || GET_CODE(src2) != PLUS || GET_CODE(src3) != PLUS)
	continue;

      rtx l1 = XEXP(src1, 0);
//      rtx l2 = XEXP(src2, 0);
//      rtx l3 = XEXP(src3, 0);

      rtx r1 = XEXP(src1, 1);
      rtx r2 = XEXP(src2, 1);
      rtx r3 = XEXP(src3, 1);
      if (!CONST_INT_P(r1) || !REG_P(r2) || !CONST_INT_P(r3))
	continue;

      if (REGNO(dst1) != REGNO(dst2) || REGNO(r2) != REGNO(dst3))
	continue;

      log ("merge_add applied\n");

      rtx_insn * newins1 = make_insn_raw (gen_rtx_SET(dst1, l1));
      add_insn_after (newins1, ins1, 0);
      SET_INSN_DELETED(ins1);

      rtx_insn * newins2 = make_insn_raw (PATTERN (ins2));
      add_insn_after (newins2, ins3, 0);
      SET_INSN_DELETED(ins2);
    }
  return change_count;
}

/*
 * Move the insns back from temp to insns.
 */
static void
clear_temp ()
{
  for (unsigned i = 0; i < temp.size (); ++i)
    if (temp[i])
      insns[i] = temp[i], temp[i] = 0;
}

/**
 * 1. scan for all used registers.
 * 2. scan the stack from for omittable push/pop
 * 3. adjust stack frame + insns referring to stack pointer
 * typical code:
 subq.l #4,sp
 movem.l #16190,-(sp)
 move.l 52(sp),d2
 move.l 56(sp),d3

 * or
 link a5,#4
 movem.l #16190,-(sp)
 move.l 8(a5),d2
 move.l 12(a5),d3
 *
 * => with a5 check only prolog/epilog
 * => without a5 adjust insns referring sp if offset > startoffset + current sp diff
 *
 * startvalue count(pushes)*4
 * newstartvalue = startvalue - omitted pushes
 */
static unsigned
opt_shrink_stack_frame (void)
{
  /* nothing to do. */
  if (!insns.size ())
    return 0;

  std::vector<int> a5pos;
  temp.resize (insns.size ());

  unsigned pos = 0;
  rtx_insn * insn = insns[pos];
  if (JUMP_P(insn)) /* return -> empty function*/
    return 0;

  bool usea5 = false;
  int paramstart = 4;
  int a5offset = 0;

  /*
   * Move prologue to temp.
   * Only register push and parallel insn unless its a link a5 are moved.
   */
  for (; pos < insns.size ();)
    {
      insn = insns[pos];

      if (proepilogue[pos] != 1)
	break;

      rtx pattern = PATTERN (insn);
      if (GET_CODE(pattern) == PARALLEL)
	{
	  rtx set = XVECEXP(pattern, 0, 0);
	  rtx dst = SET_DEST(set);
	  /* ignore link a5 */
	  if (REG_P(dst) && REGNO(dst) == FRAME_POINTER_REGNUM)
	    {
	      a5pos.push_back (pos);
	      usea5 = true;
	      set = XVECEXP(pattern, 0, 2);
	      a5offset = INTVAL(XEXP(SET_SRC(set), 1));
	    }
	  else
	    {
	      /* use movem */
	      temp[pos] = insn;
	      insns[pos] = 0;
	    }
	  ++pos;
	  continue;
	}
      if (GET_CODE(pattern) != SET)
	{
	  /* (set (mem:BLK (scratch) [0  A8]) (unspec:BLK [ ...)) */
	  if (MEM_P(SET_DEST(pattern)) && GET_CODE(SET_SRC(pattern)) == UNSPEC)
	    a5pos.push_back (pos);
	  ++pos;
	  continue;
	}

      /* move only the push statements. */
      rtx src = SET_SRC(pattern);
      rtx dest = SET_DEST(pattern);
      if (REG_P(src))
	{
	  if (MEM_P(dest))
	    {
	      rtx predec = XEXP(dest, 0);
	      if (GET_CODE(predec) == PRE_DEC)
		{
		  rtx reg = XEXP(predec, 0);
		  if (REG_P(reg) && REGNO(reg) == STACK_POINTER_REGNUM)
		    {
		      temp[pos] = insn;
		      insns[pos] = 0;
		    }
		}
	    }
	}
      else if (GET_CODE(src) == PLUS && REG_P(dest) && REGNO(dest) == STACK_POINTER_REGNUM)
	{
	  /* check for stack variables. */
	  rtx reg = XEXP(src, 0);
	  rtx cx = XEXP(src, 1);
	  if (REG_P(reg) && REGNO(reg) == STACK_POINTER_REGNUM && CONST_INT_P(cx))
	    paramstart -= INTVAL(cx);
	}

      if (++pos >= insns.size ())
	{
	  clear_temp ();
	  return 0;
	}
    }

  if (pos == 0)
    return 0;

  unsigned prologueend = pos;

  /* search epilogues - there can be multiple epilogues. */
  while (pos < insns.size ())
    {
      while (pos < insns.size ())
	{
	  if (proepilogue[pos])
	    break;
	  ++pos;
	}

      /* move epilogues away. */
      for (; pos < insns.size (); ++pos)
	{
	  insn = insns[pos];
	  if (JUMP_P(insn) || LABEL_P(insn) || !proepilogue[pos])
	    break;

	  /* omit the frame pointer a5. */
	  rtx pattern = PATTERN (insn);
	  if (GET_CODE(pattern) == PARALLEL)
	    {
	      rtx set = XVECEXP(pattern, 0, 0);
	      rtx dst = SET_DEST(set);
	      /* unlink is last. */
	      if (REG_P(dst) && REGNO(dst) == FRAME_POINTER_REGNUM)
		{
		  a5pos.push_back (pos);
		  break;
		}

	      /* movem. */
	      temp[pos] = insn;
	      insns[pos] = 0;
	    }
	  else if (GET_CODE(pattern) == SET)
	    {
	      /* check for move (a7+), x */
	      rtx src = SET_SRC(pattern);
	      rtx dst = SET_DEST(pattern);
	      if (REG_P(dst))
		{
		  if (MEM_P(src))
		    {
		      rtx postinc = XEXP(src, 0);
		      if (GET_CODE(postinc) == POST_INC)
			{
			  rtx reg = XEXP(postinc, 0);
			  if (REG_P(reg) && REGNO(reg) == STACK_POINTER_REGNUM)
			    {
			      temp[pos] = insn;
			      insns[pos] = 0;
			    }
			}
		    }
		}
	    }
	}
      ++pos;
    }
  /* gather usage stats without prologue/epilogue */
  update_insn_infos ();
  insn_info ii;
  for (unsigned i = 0; i < infos.size (); ++i)
    {
      if (proepilogue[i])
	continue;

      insn_info & jj = infos[i];
      ii |= jj;
    }
  unsigned freemask = ~ii._use;

  rtx a7 = gen_raw_REG (SImode, STACK_POINTER_REGNUM);
  rtx a5 = gen_raw_REG (SImode, FRAME_POINTER_REGNUM);

  unsigned adjust = 0;
  /* now all push/pop insns are in temp. */
  for (unsigned i = 0; i < temp.size (); ++i)
    {
      insn = temp[i];
      if (!insn)
	continue;

      rtx pattern = PATTERN (insn);
      /* check the pushed regs, either a vector or single statements */
      if (GET_CODE(pattern) == PARALLEL)
	{
	  std::vector<rtx> regs;
	  std::vector<rtx> clobbers;
	  for (int j = 0; j < XVECLEN(pattern, 0); ++j)
	    {
	      rtx set = XVECEXP(pattern, 0, j);
	      if (GET_CODE(set) == CLOBBER)
		{
		  clobbers.push_back (set);
		  continue;
		}
	      rtx src = SET_SRC(set);
	      rtx dst = SET_DEST(set);
	      rtx reg;
	      if (MEM_P(src))
		reg = dst;
	      else if (MEM_P(dst))
		reg = src;
	      else
		continue;

	      if (i < prologueend)
		paramstart += 4;
	      unsigned regbit = 1 << REGNO(reg);
	      if (freemask & regbit)
		{
		  log (i < prologueend ? "remove push for %s\n" : "remove pop for %s\n",
		  reg_names[REGNO(reg)]);
		  if (i < prologueend)
		    adjust += 4;
		}
	      else
		regs.push_back (copy_reg (reg, -1));
	    }

	  /* don't touch - clobbers! */
	  if (clobbers.size ())
	    continue;

	  /* add romm for add.
	   * push is always using -(a7) addressing.
	   * If a5 is used a movem offset(a5) is generated to pop saved registers..
	   * Otherwise a7 is used and with (a7)+ addressing.
	   */
	  int add1 = i < prologueend || !usea5 ? 1 : 0;
	  if ((int) regs.size () + add1 < XVECLEN(pattern, 0) || regs.size () <= 2)
	    {
	      log ("shrinking stack frame from %d to %d\n", XVECLEN(pattern, 0) - add1, regs.size ());
	      if (regs.size () <= 2)
		{
		  for (unsigned k = 0; k < regs.size (); ++k)
		    {
		      rtx reg = regs[k];
		      if (i < prologueend)
			{
			  /* push */
			  rtx dec = gen_rtx_PRE_DEC(REGNO(regs[k]) > STACK_POINTER_REGNUM ? XFmode : SImode, a7);
			  rtx mem = gen_rtx_MEM (REGNO(regs[k]) > STACK_POINTER_REGNUM ? XFmode : SImode, dec);
			  rtx set = gen_rtx_SET(mem, reg);
			  emit_insn_after (set, insn);
			}
		      else
			{
			  /* pop */
			  rtx dec = gen_rtx_POST_INC(REGNO(regs[k]) > STACK_POINTER_REGNUM ? XFmode : SImode, a7);
			  rtx mem = gen_rtx_MEM (REGNO(regs[k]) > STACK_POINTER_REGNUM ? XFmode : SImode, dec);
			  rtx set = gen_rtx_SET(reg, mem);
			  emit_insn_before (set, insn);
			}
		    }
		}
	      else
		{
		  rtx parallel = gen_rtx_PARALLEL(VOIDmode, rtvec_alloc (regs.size () + add1));
		  rtx plus;

		  int x = 0;
		  for (unsigned k = 0; k < regs.size (); ++k)
		    x += REGNO(regs[k]) > STACK_POINTER_REGNUM ? 12 : 4;

		  /* no add if a5 is used with pop */
		  if (!usea5 || i < prologueend)
		    {
		      plus = gen_rtx_PLUS(SImode, a7, gen_rtx_CONST_INT (SImode, i < prologueend ? -x : x));
		      XVECEXP(parallel, 0, 0) = gen_rtx_SET(a7, plus);
		    }

		  if (i >= prologueend)
		    x = usea5 ? -x : 0;

		  for (unsigned k = 0; k < regs.size (); ++k)
		    {
		      if (i < prologueend)
			{
			  /* push */
			  plus = gen_rtx_PLUS(SImode, a7, gen_rtx_CONST_INT (SImode, -x));
			  x -= REGNO(regs[k]) > STACK_POINTER_REGNUM ? 12 : 4;
			  rtx mem = gen_rtx_MEM (REGNO(regs[k]) > STACK_POINTER_REGNUM ? XFmode : SImode, plus);
			  rtx set = gen_rtx_SET(mem, regs[k]);
			  XVECEXP(parallel, 0, k + 1) = set;
			}
		      else
			{
			  /* pop */
			  if (usea5)
			    {
			      x += REGNO(regs[k]) > STACK_POINTER_REGNUM ? 12 : 4;
			      plus = gen_rtx_PLUS(SImode, a5, gen_rtx_CONST_INT (SImode, a5offset + x));
			      rtx mem = gen_rtx_MEM (REGNO(regs[k]) > STACK_POINTER_REGNUM ? XFmode : SImode, plus);
			      rtx set = gen_rtx_SET(regs[k], mem);
			      XVECEXP(parallel, 0, k) = set;
			    }
			  else
			    {
			      plus = x ? gen_rtx_PLUS(SImode, a7, gen_rtx_CONST_INT (SImode, x)) : a7;
			      x += REGNO(regs[k]) > STACK_POINTER_REGNUM ? 12 : 4;
			      rtx mem = gen_rtx_MEM (REGNO(regs[k]) > STACK_POINTER_REGNUM ? XFmode : SImode, plus);
			      rtx set = gen_rtx_SET(regs[k], mem);
			      XVECEXP(parallel, 0, k + 1) = set;
			    }
			}
		    }
		  emit_insn_after (parallel, insn);
		}
	      SET_INSN_DELETED(insn);
	    }
	}
      else
	{
	  rtx set = PATTERN (insn);

	  if (i < prologueend)
	    {
	      /* move x,-(a7). */
	      rtx src = SET_SRC(set);
	      paramstart += REGNO(src) > STACK_POINTER_REGNUM ? 12 : 4;
	      unsigned regbit = 1 << REGNO(src);
	      if (freemask & regbit)
		{
		  adjust += REGNO(src) > STACK_POINTER_REGNUM ? 12 : 4;
		  log ("remove push for %s\n", reg_names[REGNO(src)]);
		  SET_INSN_DELETED(insn);
		}
	    }
	  else
	    {
	      /* move (a7)+,x */
	      rtx dst = SET_DEST(set);
	      unsigned regbit = 1 << REGNO(dst);
	      if (freemask & regbit)
		{
		  log ("remove pop for %s\n", reg_names[REGNO(dst)]);
		  SET_INSN_DELETED(insn);
		}
	    }
	}
    }

  /* fix sp offsets. */
  if (!usea5 && adjust)
    {
      for (unsigned index = 0; index < insns.size (); ++index)
	{
	  insn = insns[index];
	  if (!insn || !INSN_P(insn))
	    continue;

	  rtx set = single_set (insn);
	  if (!set)
	    continue;

	  rtx mem = SET_SRC(set);
	  if (MEM_P(mem))
	    {
	      rtx plus = XEXP(mem, 0);
	      if (GET_CODE(plus) == PLUS)
		{
		  rtx sp = XEXP(plus, 0);
		  if (REG_P(sp) && REGNO(sp) == STACK_POINTER_REGNUM)
		    {
		      rtx c = XEXP(plus, 1);
		      if (CONST_INT_P(c))
			{
			  int n = INTVAL(c);
			  if (n >= paramstart)
			    XEXP(plus, 1) = gen_rtx_CONST_INT (SImode, n - adjust);
			}
		    }
		}
	    }
	}
    }

  if (usea5 && a5offset == -4)
    {
      for (std::vector<int>::iterator i = a5pos.begin (); i != a5pos.end (); ++i)
	{
	  temp[*i] = insns[*i];
	  insns[*i] = 0;
	}
      update_insn_infos ();
      insn_info ii;
      for (unsigned i = 0; i < infos.size (); ++i)
	{
	  if (proepilogue[i])
	    continue;

	  insn_info & jj = infos[i];
	  ii |= jj;
	}
      unsigned freemask = ~ii._use;

      if (freemask & (1 << FRAME_POINTER_REGNUM))
	{
	  log ("dropping unused frame pointer\n");
	  for (std::vector<int>::iterator i = a5pos.begin (); i != a5pos.end (); ++i)
	    SET_INSN_DELETED(temp[*i]);
	}
    }

  /* restore stack insns */
  clear_temp ();

  return 0;
}

namespace
{

  const pass_data pass_data_bbb_optimizations =
    { RTL_PASS, /* type */
    "bebbo's-optimizers", /* name */
    OPTGROUP_NONE, /* optinfo_flags */
    TV_NONE, /* tv_id */
    0, /* properties_required */
    0, /* properties_provided */
    0, /* properties_destroyed */
    0, /* todo_flags_start */
    0, //( TODO_df_finish | TODO_df_verify), /* todo_flags_finish */
      };

  class pass_bbb_optimizations : public rtl_opt_pass
  {
  public:
    pass_bbb_optimizations (gcc::context *ctxt) :
	rtl_opt_pass (pass_data_bbb_optimizations, ctxt), pp (0)
    {
    }

    /* opt_pass methods: */
    virtual bool
    gate (function *)
    {
      if (!string_bbb_opts)
	string_bbb_opts = "+";

      return TARGET_AMIGA && optimize > 0 && string_bbb_opts && !strchr (string_bbb_opts, '-');
    }

    virtual unsigned int
    execute (function *)
    {
      return execute_bbb_optimizations ();
    }

    opt_pass *
    clone ()
    {
      pass_bbb_optimizations * bbb = new pass_bbb_optimizations (m_ctxt);
      bbb->pp = pp + 1;
      return bbb;
    }

    unsigned int pp;

    unsigned
    execute_bbb_optimizations (void);
  };
// class pass_bbb_optimizations

  /* Main entry point to the pass.  */
  unsigned
  pass_bbb_optimizations::execute_bbb_optimizations (void)
  {
    // df_set_flags (df_LR_RUN_DCE + df_DEFER_INSN_RESCAN);
    // df_note_add_problem ();
    // df_analyze ();

    be_verbose = strchr (string_bbb_opts, 'v');

    bool do_opt_strcpy = strchr (string_bbb_opts, 's') || strchr (string_bbb_opts, '+');
    bool do_commute_add_move = strchr (string_bbb_opts, 'a') || strchr (string_bbb_opts, '+');
    bool do_propagate_moves = strchr (string_bbb_opts, 'p') || strchr (string_bbb_opts, '+');
    bool do_const_cmp_to_sub = strchr (string_bbb_opts, 'c') || strchr (string_bbb_opts, '+');
    bool do_merge_add = strchr (string_bbb_opts, 'm') || strchr (string_bbb_opts, '+');
    bool do_elim_dead_assign = strchr (string_bbb_opts, 'e') || strchr (string_bbb_opts, '+');
    bool do_bb_reg_rename = strchr (string_bbb_opts, 'r') || strchr (string_bbb_opts, '+');
    bool do_shrink_stack_frame = strchr (string_bbb_opts, 'f') || strchr (string_bbb_opts, '+');

    for (;;)
      {
	int done = 1;
	update_insns ();
	update_insn_infos ();
	if (do_opt_strcpy && opt_strcpy ())
	  done = 0, update_insns ();

	if (do_commute_add_move && opt_commute_add_move ())
	  done = 0, update_insns ();

	if (do_propagate_moves && opt_propagate_moves ())
	  done = 0, update_insns ();

	update_insn_infos ();
	if (do_const_cmp_to_sub && opt_const_cmp_to_sub ())
	  done = 0, update_insns (), update_insn_infos ();

	if (do_merge_add && opt_merge_add ())
	  done = 0, update_insns (), update_insn_infos ();

	if (do_elim_dead_assign && opt_elim_dead_assign ())
	  done = 0, update_insns (), update_insn_infos ();

	if (do_bb_reg_rename)
	  {
	    while (opt_reg_rename ())
	      {
		update_insns ();
		update_insn_infos ();
		done = 0;
	      }
	  }

	if (done)
	  break;
      }

    if (do_shrink_stack_frame)
      {
	opt_shrink_stack_frame ();
	update_insns ();
	update_insn_infos ();
      }

    if (strchr (string_bbb_opts, 'X') || strchr (string_bbb_opts, 'x'))
      dump_insns ("bbb", strchr (string_bbb_opts, 'X'));
    clear ();

    return 0;
  }

}      // anon namespace

rtl_opt_pass *
make_pass_bbb_optimizations (gcc::context * ctxt)
{
  return new pass_bbb_optimizations (ctxt);
}
