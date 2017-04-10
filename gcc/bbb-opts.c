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
 * #2 strcpy_opt
 * check if a temp reg can be eliminated.
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
#include "tree-pass.h"
#include "conditions.h"
#include "cselib.h"
#include <vector>
#include <map>

/* Enough for m68k.
 * Why a class? Maybe extend it for general usage.
 *
 * Track use & def separate to determine starting points.
 */
struct insn_info
{
  unsigned _use;
  unsigned _def;

  insn_info () :
      _use (0), _def (0)
  {
  }

  inline void
  reset ()
  {
    _use = 0;
    _def = 0;
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
  unset (int regno)
  {
    _use &= ~(1 << regno);
    _def &= ~(1 << regno);
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

  inline insn_info
  operator | (insn_info const & o) const
  {
    insn_info t;
    t._use = _use | o._use;
    t._def = _def | o._def;
    return t;
  }

  inline insn_info &
  operator |= (insn_info const & o)
  {
    _use |= o._use;
    _def |= o._def;
    return *this;
  }

  inline insn_info &
  operator &= (insn_info const & o)
  {
    _use &= o._use & o._def;
    _def &= o._def;
    return *this;
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
    return t;
  }

  inline bool
  contains (insn_info const & o) const
  {
    if (o._def & ~_def)
      return false;
    if (o._use & ~_use)
      return false;
    return true;
  }

  void
  scan (rtx);

  unsigned
  get_def_mask () const
  {
    if (!_def || _def > 0xffff)
      return 0;

    unsigned mask = _def - 1;
    if (_def > 0xff)
      mask &= 0xff00;
    return mask;
  }
};

/* scan rtx for registers and set the corresponding flags. */
void
insn_info::scan (rtx x)
{
  if (REG_P(x))
    {
      use (REGNO(x));
      return;
    }

  if (x == cc0_rtx)
    {
      use (FIRST_PSEUDO_REGISTER);
      return;
    }

  RTX_CODE code = GET_CODE(x);
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

/* perform reg renaming. */
static void
do_reg_rename (rtx x, unsigned oldregno, unsigned newregno)
{
  if (REG_P(x))
    {
      if (REGNO(x) == oldregno)
	df_ref_change_reg_with_loc (x, newregno);
      return;
    }

  if (x == cc0_rtx)
    return;

  RTX_CODE code = GET_CODE(x);
  const char *fmt = GET_RTX_FORMAT(code);
  for (int i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	do_reg_rename (XEXP(x, i), oldregno, newregno);
      else if (fmt[i] == 'E')
	for (int j = XVECLEN (x, i) - 1; j >= 0; j--)
	  do_reg_rename (XVECEXP(x, i, j), oldregno, newregno);
    }
}

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
 * Collect some data.
 */
static std::vector<rtx_insn *> insns;
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
  jumps.clear ();
  insn2index.clear ();
  infos.clear ();
}

/*
 *  return true if the register is DEAD.
 */
static bool
is_reg_dead (unsigned regno, unsigned pos)
{
  for (;;)
    {
      if (pos + 1 >= infos.size ())
	return true;

      rtx_insn * insn = insns[pos + 1];
      if (!LABEL_P(insn) && GET_CODE(insn) != USE)
	break;
      ++pos;
    }

  insn_info & ii0 = infos[pos + 1];
  // not dead if usage is reported in the next statement
  return !ii0.is_use (regno);
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
      insn_info & ii = infos[i];

      fprintf (stderr, "%d: ", i);

      for (int j = 0; j < 8; ++j)
	if (ii.is_use (j))
	  fprintf (stderr, ii.is_def (j) ? "*d%d " : "d%d ", j);

      for (int j = 8; j < 16; ++j)
	if (ii.is_use (j))
	  fprintf (stderr, ii.is_def (j) ? "*a%d " : "a%d ", j - 8);

      if (ii.is_use (FIRST_PSEUDO_REGISTER))
	fprintf (stderr, ii.is_def (FIRST_PSEUDO_REGISTER) ? "*cc " : "cc ");

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

  df_insn_rescan_all ();

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
	}
    }
}

static void
update_insn_infos (void)
{
  /* prepare insn_info */
  insn_info ii;
  for (unsigned i = 0; i < insns.size (); ++i)
    {
      infos.push_back (ii);
    }

  /* own analyze reg life */
  std::vector<std::pair<unsigned, insn_info>> todo;
  todo.push_back (std::make_pair (insns.size () - 1, ii));

  int pass = 0;
  while (!todo.empty ())
    {
      std::pair<unsigned, insn_info> p = *todo.rbegin ();
      todo.pop_back ();

      insn_info ii = p.second;

      for (int pos = p.first; pos >= 0; --pos)
	{
	  rtx_insn * insn = insns[pos];
	  if (!insn) // moved to temp for stack frame cleanup
	    continue;

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
	      /* a call sets d0 and maybe also d1,a0,a1. */
	      ii.unset (0);
	      ii.unset (1);
	      ii.unset (8);
	      ii.unset (9);

	      // FIXME: isuse the DECL and read attributes.
	      // use regs depending on flag mregparm
	      for (int i = 0; i < amigaos_regparm; ++i)
		{
		  ii.use (i);
		  ii.use (i + 8);
		}

	      // check for reg use
	      ii.scan (pattern);

	      infos[pos] = ii;
	      continue;
	    }

	  if (JUMP_P(insn))
	    {
	      if (ANY_RETURN_P(pattern))
		{
		  ii.reset ();
		  ii.use (0);
		}
	      else
		{
		  ii |= infos[pos];

		  // check for reg use
		  ii.scan (PATTERN (insn));

		}
	      infos[pos] = ii;
	      continue;
	    }

	  rtx set = single_set (insn);
	  if (set == 0)
	    {
	      if (GET_CODE (pattern) == USE)
		{
		  rtx x = XEXP(pattern, 0);
		  if (REG_P(x))
		    ii.use (REGNO(x));
		  infos[pos] = ii;
		  continue;
		}

	      if (GET_CODE (pattern) != PARALLEL)
		{
		  fprintf (stderr, "##### ");
		  debug_rtx (insn);
		}
	      ii.scan (pattern);
	      infos[pos] = ii;
	      continue;
	    }

	  rtx src = SET_SRC(set);
	  rtx dst = SET_DEST(set);
	  // scan insn for regs
	  // a def stop propagation
	  // a use starts propagation
	  // also add use to current ii
	  insn_info use;
	  insn_info def;

	  use.scan (src);
	  if (REG_P(dst))
	    def.def (REGNO(dst));
	  else if (dst == cc0_rtx)
	    def.def (FIRST_PSEUDO_REGISTER);
	  else
	    use.scan (dst);

	  if (dst != cc0_rtx)
	    {
	      CC_STATUS_INIT;
	      NOTICE_UPDATE_CC(PATTERN (insn), insn);
	      if (cc_status.value1 || cc_status.value2)
		def.def (FIRST_PSEUDO_REGISTER);
	    }

	  infos[pos] = def | use | ii;

	  ii &= ~def;
	  ii |= use;
	}
      ++pass;
    }
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
propagate_moves ()
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

				  fprintf (stderr, ":bbb: propagate_moves condition met, moving regs %d, %d\n",
					   REGNO(srci), REGNO(dsti));

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

//				  df_insn_rescan (ii);
//				  df_insn_rescan (jj);
				  df_insn_rescan (newii);
				  df_insn_rescan (newjj);

				  /* add fixes if there were jumps out of the loop. */
				  if (jump_out.size ())
				    {
				      fprintf (stderr, ":bbb: propagate_moves fixing %d jump outs\n", jump_out.size ());

				      for (unsigned k = 0; k < jump_out.size (); ++k)
					{
					  rtx neu = gen_rtx_SET(
					      dstj, gen_rtx_PLUS(Pmode, dsti, gen_rtx_CONST_INT(Pmode, fixups[k])));
					  rtx_insn * neui = emit_insn_after (neu, jump_out[k]);
					  df_insn_rescan (neui);
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
	continue;

      rtx set = single_set (insn);
      if (!set)
	continue;

      if (x2reg && reg2x)
	{
	  rtx src = SET_SRC(set);
	  if (GET_CODE(src) == COMPARE)
	    {
	      rtx dst = XEXP(src, 0);
	      src = XEXP(src, 1);

	      if (CONST_INT_P(src) && INTVAL(src) == 0 && find_reg_note (insn, REG_DEAD, dst))
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

			  fprintf (
			  stderr,
				   ":bbb: opt_strcpy condition met, removing compare and joining insns - omit reg %d\n",
				   REGNO(dst));

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

			  df_insn_rescan (reg2x);

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
commute_add_move (void)
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
	  fprintf (stderr, ":bbb: commute_add_move found\n");

	  SET_INSN_DELETED(insn);

	  insn = emit_insn_before (newinsn, next);

	  add_reg_note (next, REG_INC, reg1dst);

	  df_insn_rescan (insn);
	  df_insn_rescan (next);

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
const_cmp_to_sub (void)
{
  unsigned change_count = 0;
#if HAVE_cc0
  for (unsigned index = insns.size () - 2; index > 0; --index)
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
      if (!REG_P(left) || !REG_P(right))
	continue;

      // TODO
      // FEATURE: check if the next uses are also a add/sub
      // then maybe that add/sub can be adjusted too

      //      if (!find_reg_note (insn, REG_DEAD, left) || !find_reg_note (insn, REG_DEAD, right))
      //	continue;
      // use own reg_dead
      if (!is_reg_dead (REGNO(left), index) || !is_reg_dead (REGNO(right), index))
	continue;

      fprintf (stderr, ":bbb: found reg-reg compare with both dead: %d %d\n", is_reg_dead (REGNO(left), index),
	       is_reg_dead (REGNO(right), index));

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
      if (intval < -8 || intval > 7)
	continue;

      enum machine_mode mode = GET_MODE(dstp);
      rtx reg = dstp == left ? right : left;
      rtx plus = gen_rtx_PLUS(mode, reg, gen_rtx_CONST_INT (mode, intval));

      rtx_insn * neuprev = make_insn_raw (gen_rtx_SET(reg, plus));

      int num_clobbers_to_add = 0;
      int insn_code_number = recog (PATTERN (neuprev), neuprev, &num_clobbers_to_add);
      if (insn_code_number >= 0 && !check_asm_operands (PATTERN (neuprev)))
	continue;

      // also convert current statement to cmp #0, reg
      SET_INSN_DELETED(insn);
      rtx neu = gen_rtx_SET(cc0_rtx, gen_rtx_COMPARE(mode, reg, gen_rtx_CONST_INT(mode, 0)));
      insn = emit_insn_after (neu, prev);
      add_reg_note (insn, REG_DEAD, reg);

      SET_INSN_DELETED(prev);
      prev = emit_insn_before (neuprev, insn);

// urks - unknown side effects
//      int omitted_regno = REGNO(dstp);
//      cselib_invalidate_rtx (dstp);
//      if (!(df->hard_regs_live_count[omitted_regno] -= 2))
//	df_set_regs_ever_live (omitted_regno, false);

      fprintf (stderr, ":bbb: const_cmp_to_sub replaced reg-reg compare with sub\n");

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
		      fprintf (stderr, ":bbb: patch jcc %d -> %d\n", code, newcode);
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

static unsigned
elim_dead_assign (void)
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
	  fprintf (stderr, ":bbb: elim_dead_assign to %d\n", REGNO(dst));
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
merge_add (void)
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

      fprintf (stderr, ":bbb: merge_add applied\n");

      SET_SRC(set1) = l1;
      rtx_insn * newins2 = make_insn_raw (PATTERN (ins2));
      add_insn_after (newins2, ins3, 0);
      SET_INSN_DELETED(ins2);
      df_insn_rescan (ins1);
    }
  return change_count;
}

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
shrink_stack_frame (void)
{
  /* nothing to do. */
  if (!insns.size ())
    return 0;

  temp.resize (insns.size ());

  unsigned pos = 0;
  rtx_insn * insn = insns[pos];
  if (JUMP_P(insn)) /* return -> empty function*/
    return 0;

  bool usea5 = false;
  unsigned paramstart = 4;
  /*
   * Move prologue to temp.
   * Only register push and parallel insn unless its a link a5 are moved.
   */
  rtx_insn * prev = get_insns ();
  for (; pos < insns.size ();)
    {
      insn = insns[pos];

      /* check for prologue end. */
      for (; prev != insn; prev = NEXT_INSN (prev))
	if (NOTE_P(prev) && NOTE_KIND(prev) == NOTE_INSN_PROLOGUE_END)
	  break;
      if (prev != insn)
	break;

      rtx pattern = PATTERN (insn);
      if (GET_CODE(pattern) == PARALLEL)
	{
	  rtx set = XVECEXP(pattern, 0, 0);
	  rtx dst = SET_DEST(set);
	  /* ignore link a5 */
	  if (REG_P(dst) && REGNO(dst) == 13)
	    usea5 = true;
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
		  if (REG_P(reg) && REGNO(reg) == 15)
		    {
		      temp[pos] = insn;
		      insns[pos] = 0;
		    }
		}
	    }
	}
      else if (GET_CODE(src) == PLUS && REG_P(dest) && REGNO(dest) == 15)
	{
	  /* check for stack variables. */
	  rtx reg = XEXP(src, 0);
	  rtx cx = XEXP(src, 1);
	  if (REG_P(reg) && REGNO(reg) == 15 && CONST_INT_P(cx))
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
	  insn = insns[pos];
	  for (; prev != insn; prev = NEXT_INSN (prev))
	    if (NOTE_P(prev) && NOTE_KIND(prev) == NOTE_INSN_EPILOGUE_BEG)
	      break;

	  if (prev != insn)
	    break;

	  ++pos;
	}

      /* move epilogues away. */
      for (; pos < insns.size (); ++pos)
	{
	  insn = insns[pos];
	  if (JUMP_P(insn)) /* return */
	    break;

	  if (LABEL_P(insn))
	    break;

	  /* omitt the frame pointer a5. */
	  rtx pattern = PATTERN (insn);
	  if (GET_CODE(pattern) == PARALLEL)
	    {
	      rtx set = XVECEXP(pattern, 0, 0);
	      rtx dst = SET_DEST(set);
	      /* unlink is last. */
	      if (REG_P(dst) && REGNO(dst) == 13)
		break;

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
			  if (REG_P(reg) && REGNO(reg) == 15)
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
    ii |= infos[i];
  unsigned freemask = ~ii._use;

  rtx a7 = gen_raw_REG (SImode, 15);

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
		  fprintf (stderr, i < prologueend ? "remove push for %d\n" : "remove pop for %d\n", REGNO(reg));
		  if (i < prologueend)
		    adjust += 4;
		}
	      else
		regs.push_back (reg);
	    }

	  /* don't touch - clobbers! */
	  if (clobbers.size())
	    continue;

	  if ((int) regs.size () + 1 < XVECLEN(pattern, 0) || regs.size () <= 2)
	    {
	      if (regs.size () <= 2)
		{
		  for (unsigned k = 0; k < regs.size (); ++k)
		    {
		      rtx reg = regs[k];
		      if (i < prologueend)
			{
			  /* push */
			  rtx dec = gen_rtx_PRE_DEC(SImode, a7);
			  rtx mem = gen_rtx_MEM (SImode, dec);
			  rtx set = gen_rtx_SET(mem, reg);
			  emit_insn_after (set, insn);
			}
		      else
			{
			  /* pop */
			  rtx dec = gen_rtx_POST_INC(SImode, a7);
			  rtx mem = gen_rtx_MEM (SImode, dec);
			  rtx set = gen_rtx_SET(reg, mem);
			  emit_insn_before (set, insn);
			}
		    }
		}
	      else
		{
		  rtx parallel = gen_rtx_PARALLEL(VOIDmode, rtvec_alloc (regs.size () + 1));
		  int x = regs.size () * 4 + 4;

		  rtx plus = gen_rtx_PLUS(SImode, a7, gen_rtx_CONST_INT (SImode, i < prologueend ? -x : x));
		  XVECEXP(parallel, 0, 0) = gen_rtx_SET(a7, plus);

		  if (i >= prologueend)
		    x = 0;

		  for (unsigned k = 0; k < regs.size (); ++k)
		    {
		      if (i < prologueend)
			{
			  /* push */
			  plus = gen_rtx_PLUS(SImode, a7, gen_rtx_CONST_INT (SImode, -x));
			  x -= 4;
			  rtx mem = gen_rtx_MEM (SImode, plus);
			  rtx set = gen_rtx_SET(mem, regs[k]);
			  XVECEXP(parallel, 0, k + 1) = set;
			}
		      else
			{
			  /* pop */
			  plus = x ? gen_rtx_PLUS(SImode, a7, gen_rtx_CONST_INT (SImode, x)) : a7;
			  x += 4;
			  rtx mem = gen_rtx_MEM (SImode, plus);
			  rtx set = gen_rtx_SET(regs[k], mem);
			  XVECEXP(parallel, 0, k + 1) = set;
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
	      paramstart += 4;
	      rtx src = SET_SRC(set);
	      unsigned regbit = 1 << REGNO(src);
	      if (freemask & regbit)
		{
		  adjust += 4;
		  fprintf (stderr, "remove push for %d\n", REGNO(src));
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
		  fprintf (stderr, "remove pop for %d\n", REGNO(dst));
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
		  if (REG_P(sp) && REGNO(sp) == 15)
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

  /* restore stack insns */
  clear_temp ();

  return 0;
}

extern class opt_pass * global_pass_regrename;

namespace
{

  const pass_data pass_data_bbb_optimizations =
    { RTL_PASS, /* type */
    "bbb", /* name */
    OPTGROUP_NONE, /* optinfo_flags */
    TV_NONE, /* tv_id */
    0, /* properties_required */
    0, /* properties_provided */
    0, /* properties_destroyed */
    0, /* todo_flags_start */
    ( TODO_df_finish | TODO_df_verify), /* todo_flags_finish */
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
      return TARGET_AMIGA && flag_bbb_opts;
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
      // bbb->pp = pp + 1;
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
    df_set_flags (DF_LR_RUN_DCE + DF_DEFER_INSN_RESCAN);
    df_note_add_problem ();
    df_analyze ();

    update_insns ();

    bool do_opt_strcpy = strchr (flag_bbb_opts, 's') || strchr (flag_bbb_opts, '*');
    bool do_commute_add_move = strchr (flag_bbb_opts, 'a') || strchr (flag_bbb_opts, '*');
    bool do_propagate_moves = strchr (flag_bbb_opts, 'p') || strchr (flag_bbb_opts, '*');
    bool do_const_cmp_to_sub = strchr (flag_bbb_opts, 'c') || strchr (flag_bbb_opts, '*');
    bool do_merge_add = strchr (flag_bbb_opts, 'm') || strchr (flag_bbb_opts, '*');
    bool do_elim_dead_assign = strchr (flag_bbb_opts, 'e') || strchr (flag_bbb_opts, '*');
    bool do_bb_reg_rename = strchr (flag_bbb_opts, 'r') || strchr (flag_bbb_opts, '*');
    bool do_shrink_stack_frame = strchr (flag_bbb_opts, 'f') || strchr (flag_bbb_opts, '*');

    for (;;)
      {
	int done = 1;
	if (do_opt_strcpy && opt_strcpy ())
	  done = 0, update_insns ();

	if (do_commute_add_move && commute_add_move ())
	  done = 0, update_insns ();

	if (do_propagate_moves && propagate_moves ())
	  done = 0, update_insns ();

	update_insn_infos ();
	if (do_const_cmp_to_sub && const_cmp_to_sub ())
	  done = 0, update_insns (), update_insn_infos ();

	if (do_merge_add && merge_add ())
	  done = 0, update_insns (), update_insn_infos ();

	if (do_elim_dead_assign && elim_dead_assign ())
	  done = 0, update_insns ();

	if (done)
	  break;
      }

    if (do_bb_reg_rename && ::global_pass_regrename)
      {
	class opt_pass * rr = ::global_pass_regrename->clone ();
	rr->execute (0);
	update_insns ();
      }

    if (do_shrink_stack_frame)
      {
	shrink_stack_frame ();
	update_insns ();
      }

    if (strchr (flag_bbb_opts, 'X') || strchr (flag_bbb_opts, 'x'))
      dump_insns ("bbb 1", strchr (flag_bbb_opts, 'X'));
    clear ();

    return 0;
  }

}      // anon namespace

rtl_opt_pass *
make_pass_bbb_optimizations (gcc::context * ctxt)
{
  return new pass_bbb_optimizations (ctxt);
}
