/* Bebbo's Optimizations.
 Copyright (C) 2010-2016 Free Software Foundation, Inc.

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
 * move a,reg
 * move reg,b
 * cmp  #0,reg
 * jcc
 *
 * ->
 * move a,b
 * jcc
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
#include <vector>
#include <map>

/* enough for m68k. */
typedef unsigned insn_info; //[(FIRST_PSEUDO_REGISTER + sizeof(unsigned) * 8 - 1) / (sizeof(unsigned) * 8)];

static inline void
resetii (unsigned & ii)
{
  ii = 0;
}

static inline void
setii (unsigned regno, unsigned & ii)
{
  ii |= 1 << regno;
}

static inline void
clearii (unsigned regno, unsigned & ii)
{
  ii &= ~(1 << regno);
}

static inline bool
getii (unsigned regno, unsigned & ii)
{
  return ii & (1 << regno) ? true : false;
}

/* scan rtx for registers. */
static void
scanii (rtx x, unsigned & ii)
{
  if (REG_P(x))
    {
      setii (REGNO(x), ii);
      return;
    }

  RTX_CODE code = GET_CODE(x);
  const char *fmt = GET_RTX_FORMAT(code);
  for (int i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	scanii (XEXP(x, i), ii);
      else if (fmt[i] == 'E')
	for (int j = XVECLEN (x, i) - 1; j >= 0; j--)
	  scanii (XVECEXP(x, i, j), ii);
    }
}

static std::vector<rtx_insn *> insns;
static std::vector<rtx_insn *> jumps;
static std::map<rtx_insn *, unsigned> insn2index;
static std::vector<insn_info> infos;

static void
clear (void)
{
  insns.clear ();
  jumps.clear ();
  insn2index.clear ();
  infos.clear ();
}

static void
dump_costs (void)
{
  rtx d0 = gen_raw_REG (SImode, 0);
  rtx d0b = gen_raw_REG (QImode, 0);
  rtx d1 = gen_raw_REG (SImode, 1);
  rtx d1b = gen_raw_REG (QImode, 1);
  rtx a0 = gen_raw_REG (SImode, 8);
  rtx a1 = gen_raw_REG (SImode, 9);

  struct ICO
  {
    char const * name;
    rtx set;
  } data[] =
    {
      { "move d0,(a0)", gen_rtx_SET(gen_rtx_MEM (SImode, a0), d0) },
      { "move a0,d0", gen_rtx_SET(d0, a0) },
      { "move (a0),d0", gen_rtx_SET(d0, gen_rtx_MEM (SImode, a0)) },
      { "move d0,(a0)", gen_rtx_SET(gen_rtx_MEM (SImode, a0), d0) },
      { "move d1,(a1)+", gen_rtx_SET(d1, gen_rtx_MEM (SImode, gen_rtx_POST_INC (SImode, a1))) },
      { "move (a0),(a1)", gen_rtx_SET(gen_rtx_MEM (SImode, a1), gen_rtx_MEM (SImode, a0)) },
      { 0, 0 } }, *p;

  for (p = data; p->name; ++p)
    {
      int cost = insn_rtx_cost (p->set, true);
      fprintf (stderr, "%s: %d\n", p->name, cost);
    }
}


static void
dump_insns (char const * name)
{
  rtx_insn *insn, *next;
  fprintf(stderr, "====================================: %s\n", name);
  for (unsigned i = 0; i < insns.size(); ++i)
    {
      insn_info & ii = infos[i];

      for (int j = 0; j < 8; ++j)
	if (getii(j, ii))
	  fprintf(stderr, "d%d ", j);

      for (int j = 8; j < 16; ++j)
	if (getii(j, ii))
	  fprintf(stderr, "a%d ", j-8);

      fprintf(stderr, "\t");
      debug_rtx(insns[i]);
    }
}


/*
 * Create a filtered view of insns - keep only those to work with.
 */
static void
filter_insns ()
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
	  //	  debug_rtx (insn);
	  if (JUMP_P(insn))
	    jumps.push_back(insn);

	  insn2index.insert (std::make_pair (insn, insns.size ()));
	  insns.push_back (insn);
	}
    }

  /* prepare insn_info */
  insn_info ii;
  resetii (ii);
  for (unsigned i = 0; i < insns.size (); ++i)
    {
      infos.push_back (ii);
    }

  /* own analyze life */
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

	  if (pass && ii == infos[pos])
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
		      if (j != insn2index.end())
			todo.push_back (std::make_pair (j->second, ii));
		    }
		}
	      continue;
	    }

	  rtx set = single_set (insn);
	  if (set == 0)
	    {
	      if (JUMP_P(insn)) {
		  resetii(ii);
		  setii(0, ii);
		  infos[pos] = ii;
		  continue;
	      }
	      fprintf(stderr, "##### ");
	      debug_rtx(insn);
	    continue;
	    }

	  rtx src = SET_SRC(set);
	  rtx dst = SET_DEST(set);

	  debug_rtx(insn);

	  if (CALL_P(insn))
	    {
	      /* a call sets d0 and kills d1,a0,a1. */
	      if (getii (1, ii))
		fprintf (stderr, "d1 used after call\n");
	      if (getii (8, ii))
		fprintf (stderr, "a0 used after call\n");
	      if (getii (9, ii))
		fprintf (stderr, "a1 used after call\n");

	      resetii (ii);
	      // use regs depending on flag mregparm
	      for (int i = 0; i < amigaos_regparm; ++i)
		{
		  setii (i, ii);
		  setii (i + 8, ii);
		}

	      // check for reg use
	      if (REG_P(src))
		setii (REGNO(src), ii);

	      infos[pos] = ii;
	      continue;
	    }

	  if (JUMP_P(insn))
	    {
	      if (ANY_RETURN_P(src))
		{
		  resetii (ii);
		  setii (0, ii);
		}
	      else
		{
		  ii |= infos[pos];

		  // check for reg use
		  if (REG_P(src))
		    setii (REGNO(src), ii);

		}
	      infos[pos] = ii;
	      continue;
	    }

	  // scan insn for regs
	  // a def stop propagation
	  // a use starts propagation
	  // also add use to current ii
	  insn_info use;
	  resetii (use);

	  insn_info def;
	  resetii (def);

	  scanii (src, use);
	  if (REG_P(dst))
	    setii (REGNO(dst), def);
	  else
	    scanii (dst, use);

	  infos[pos] = use | ii;

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
				  fprintf (stderr, "need %d jump out fixups\n", jump_out.size ());

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

				  fprintf (stderr, "condition met, moving regs %d, %d\n", REGNO(srci), REGNO(dsti));

				  /* Move in front of loop and mark as dead. */
				  remove_insn (ii);
				  add_insn_after (ii, before, 0);
				  add_reg_note (ii, REG_DEAD, srci);

				  /* Plus check if the reg was just loaded. */
				  if (bset)
				    {
				      rtx bdst = SET_DEST(bset);
				      if (REG_P(bdst) && REGNO(bdst) == REGNO(srci))
					{
					  SET_DEST(bset) = dsti;
					  SET_INSN_DELETED(ii);
					}
				    }

				  /* Move behind loop - into next BB. */
				  remove_insn (jj);
				  add_insn_before (jj, after, 0);

				  reg_reg.erase (j);
				  reg_reg.erase (i);
				  j = reg_reg.end ();
				  inc = false;

				  df_insn_rescan (ii);
				  df_insn_rescan (jj);

				  /* add fixes if there were jumps out of the loop. */
				  if (jump_out.size ())
				    {
				      fprintf (stderr, "fixing %d jump outs\n", jump_out.size ());

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

		      SET_SRC(single_set(reg2x)) = SET_SRC(single_set (x2reg));
		      insn_code_number = recog (PATTERN (reg2x), reg2x, &num_clobbers_to_add);

		      if (insn_code_number < 0)
			{
			  /* restore register. */
			  SET_SRC(single_set(reg2x)) = SET_DEST(single_set (x2reg));
			}
		      else
			{

			  rtx link;

			  fprintf (
			  stderr,
				   "condition met, removing compare and joining insns - omit reg %d\n", REGNO(dst));

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
 * Convert loops using a counting reg as offset with an address reg
 * into a loop with auto inc address regs.
 */
static unsigned
offset_2_autoinc (void)
{
  unsigned change_count = 0;
#if 0
  rtx_insn * reg_const = 0;

  for (unsigned index = 0; index < insns.size (); ++index)
    {
      rtx_insn * insn = insns[index];

      if (!next || !LABEL_P(next) || LABEL_NUSES(next) != 1)
      continue;

      if (!NONJUMP_INSN_P(insn))
      continue;

      rtx set = single_set (insn);
      if (!set)
      continue;

      rtx reg = SET_DEST(set);
      if (!REG_P(reg))
      continue;

      rtx val = SET_SRC(set);

//      fprintf(stderr, "possible start for offset_2_autoinc\n");
//      //debug_rtx(insn);
//      //debug_rtx(next);

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

      rtx reg1 = SET_DEST(set);
      if (!REG_P(reg1))
	continue;

      rtx plus = SET_SRC(set);
      if (GET_CODE(plus) != PLUS)
	continue;

      rtx reg2 = XEXP(plus, 0);
      if (!REG_P(reg2))
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
      if (!REG_P(memreg) || REGNO(memreg) != REGNO(reg2))
	continue;

      int oldcost1 = insn_rtx_cost (set, true);
      int oldcost2 = insn_rtx_cost (set2, true);

      fprintf (stderr, "commute_add_move found, oldcost: %d = %d + %d\n", oldcost1 + oldcost2, oldcost1, oldcost2);

      //debug_rtx (insn);
      //debug_rtx (next);

      rtx pinc = gen_rtx_POST_INC(GET_MODE(dst), reg1);
      rtx newmem = replace_equiv_address_nv (dst, pinc);

      if (validate_change (next, &SET_DEST(set2), newmem, 0))
	{
	  SET_INSN_DELETED(insn);

	  insn = emit_insn_before (gen_movsi (reg1, reg2), next);

	  add_reg_note (next, REG_INC, reg1);

	  int newcost1 = insn_rtx_cost (set, true);
	  int newcost2 = insn_rtx_cost (set2, true);

	  fprintf (stderr, "commute_add_move found, newcost: %d = %d + %d\n", newcost1 + newcost2, newcost1, newcost2);

	  //debug_rtx (insn);
	  //debug_rtx (next);

	  df_insn_rescan (insn);
	  df_insn_rescan (next);

	  ++change_count;
	}
    }
  return change_count;
}

static unsigned
const_cmp_to_sub (void)
{
  unsigned change_count = 0;
#if HAVE_cc0
  for (unsigned index = 0; index + 1 < insns.size (); ++index)
    {
      rtx_insn * insn = insns[index];
      rtx set = single_set (insn);
      if (!set)
	continue;

      rtx dst = SET_DEST(set);
      if (dst != cc0_rtx)
	continue;

      fprintf (stderr, "cc0:");
      debug_rtx (insn);
    }
#endif
  return change_count;
}

/* Main entry point to the pass.  */
static unsigned int
execute_bbb_optimizations (void)
{
  df_set_flags (DF_LR_RUN_DCE + DF_DEFER_INSN_RESCAN);
  df_note_add_problem ();
  df_analyze ();

//  dump_insns ("bbb 0");
  filter_insns ();

  for (;;)
    {
      int done = 1;
      if (propagate_moves ())
	done = 0, filter_insns ();

      if (offset_2_autoinc ())
	done = 0, filter_insns ();

      if (opt_strcpy ())
	done = 0, filter_insns ();

      if (commute_add_move ())
	done = 0, filter_insns ();

      if (const_cmp_to_sub ())
	done = 0, filter_insns ();

      if (done)
	break;
    }

  dump_insns ("bbb 1");
  clear ();

  dump_costs ();

  return 0;
}

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
	rtl_opt_pass (pass_data_bbb_optimizations, ctxt)
    {
    }

    /* opt_pass methods: */
    virtual bool
    gate (function *)
    {
      return TARGET_AMIGA;
    }

    virtual unsigned int
    execute (function *)
    {
      return execute_bbb_optimizations ();
    }

    opt_pass *
    clone ()
    {
      return new pass_bbb_optimizations (m_ctxt);
    }

  };
// class pass_bbb_optimizations

}// anon namespace

rtl_opt_pass *
make_pass_bbb_optimizations (gcc::context * ctxt)
{
  return new pass_bbb_optimizations (ctxt);
}
