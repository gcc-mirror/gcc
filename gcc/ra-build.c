/* Graph coloring register allocator
   Copyright (C) 2001, 2002 Free Software Foundation, Inc.
   Contributed by Michael Matz <matz@suse.de>
   and Daniel Berlin <dan@cgsoftware.com>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it under the
   terms of the GNU General Public License as published by the Free Software
   Foundation; either version 2, or (at your option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
   details.

   You should have received a copy of the GNU General Public License along
   with GCC; see the file COPYING.  If not, write to the Free Software
   Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "tm_p.h"
#include "insn-config.h"
#include "recog.h"
#include "reload.h"
#include "function.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "df.h"
#include "output.h"
#include "ggc.h"
#include "ra.h"

/* This file is part of the graph coloring register alloctor.
   It deals with building the interference graph.  When rebuilding
   the graph for a function after spilling, we rebuild only those
   parts needed, i.e. it works incrementally.

   The first part (the functions called from build_web_parts_and_conflicts()
   ) constructs a web_part for each pseudo register reference in the insn
   stream, then goes backward from each use, until it reaches defs for that
   pseudo.  While going back it remember seen defs for other registers as
   conflicts.  By connecting the uses and defs, which reach each other, webs
   (or live ranges) are built conceptually.

   The second part (make_webs() and childs) deals with converting that
   structure to the nodes and edges, on which our interference graph is
   built.  For each root web part constructed above, an instance of struct
   web is created.  For all subregs of pseudos, which matter for allocation,
   a subweb of the corresponding super web is built.  Finally all the
   conflicts noted in the first part (as bitmaps) are transformed into real
   edges.

   As part of that process the webs are also classified (their spill cost
   is calculated, and if they are spillable at all, and if not, for what
   reason; or if they are rematerializable), and move insns are collected,
   which are potentially coalescable.

   The top-level entry of this file (build_i_graph) puts it all together,
   and leaves us with a complete interference graph, which just has to
   be colored.  */


struct curr_use;

static unsigned HOST_WIDE_INT rtx_to_undefined PARAMS ((rtx));
static bitmap find_sub_conflicts PARAMS ((struct web_part *, unsigned int));
static bitmap get_sub_conflicts PARAMS ((struct web_part *, unsigned int));
static unsigned int undef_to_size_word PARAMS ((rtx, unsigned HOST_WIDE_INT *));
static bitmap undef_to_bitmap PARAMS ((struct web_part *,
				       unsigned HOST_WIDE_INT *));
static struct web_part * find_web_part_1 PARAMS ((struct web_part *));
static struct web_part * union_web_part_roots
				PARAMS ((struct web_part *, struct web_part *));
static int defuse_overlap_p_1 PARAMS ((rtx, struct curr_use *));
static int live_out_1 PARAMS ((struct df *, struct curr_use *, rtx));
static int live_out PARAMS ((struct df *, struct curr_use *, rtx));
static rtx live_in_edge PARAMS (( struct df *, struct curr_use *, edge));
static void live_in PARAMS ((struct df *, struct curr_use *, rtx));
static int copy_insn_p PARAMS ((rtx, rtx *, rtx *));
static void remember_move PARAMS ((rtx));
static void handle_asm_insn PARAMS ((struct df *, rtx));
static void prune_hardregs_for_mode PARAMS ((HARD_REG_SET *,
					     enum machine_mode));
static void init_one_web_common PARAMS ((struct web *, rtx));
static void init_one_web PARAMS ((struct web *, rtx));
static void reinit_one_web PARAMS ((struct web *, rtx));
static struct web * add_subweb PARAMS ((struct web *, rtx));
static struct web * add_subweb_2 PARAMS ((struct web *, unsigned int));
static void init_web_parts PARAMS ((struct df *));
static void copy_conflict_list PARAMS ((struct web *));
static void add_conflict_edge PARAMS ((struct web *, struct web *));
static void build_inverse_webs PARAMS ((struct web *));
static void copy_web PARAMS ((struct web *, struct web_link **));
static void compare_and_free_webs PARAMS ((struct web_link **));
static void init_webs_defs_uses PARAMS ((void));
static unsigned int parts_to_webs_1 PARAMS ((struct df *, struct web_link **,
					     struct df_link *));
static void parts_to_webs PARAMS ((struct df *));
static void reset_conflicts PARAMS ((void));
#if 0
static void check_conflict_numbers PARAMS ((void));
#endif
static void conflicts_between_webs PARAMS ((struct df *));
static void remember_web_was_spilled PARAMS ((struct web *));
static void detect_spill_temps PARAMS ((void));
static int contains_pseudo PARAMS ((rtx));
static int want_to_remat PARAMS ((rtx x));
static void detect_remat_webs PARAMS ((void));
static void determine_web_costs PARAMS ((void));
static void detect_webs_set_in_cond_jump PARAMS ((void));
static void make_webs PARAMS ((struct df *));
static void moves_to_webs PARAMS ((struct df *));
static void connect_rmw_web_parts PARAMS ((struct df *));
static void update_regnos_mentioned PARAMS ((void));
static void livethrough_conflicts_bb PARAMS ((basic_block));
static void init_bb_info PARAMS ((void));
static void free_bb_info PARAMS ((void));
static void build_web_parts_and_conflicts PARAMS ((struct df *));


/* A sbitmap of DF_REF_IDs of uses, which are live over an abnormal
   edge.  */
static sbitmap live_over_abnormal;

/* To cache if we already saw a certain edge while analyzing one
   use, we use a tick count incremented per use.  */
static unsigned int visited_pass;

/* A sbitmap of UIDs of move insns, which we already analyzed.  */
static sbitmap move_handled;

/* One such structed is allocated per insn, and traces for the currently
   analyzed use, which web part belongs to it, and how many bytes of
   it were still undefined when that insn was reached.  */
struct visit_trace
{
  struct web_part *wp;
  unsigned HOST_WIDE_INT undefined;
};
/* Indexed by UID.  */
static struct visit_trace *visit_trace;

/* Per basic block we have one such structure, used to speed up
   the backtracing of uses.  */
struct ra_bb_info
{
  /* The value of visited_pass, as the first insn of this BB was reached
     the last time.  If this equals the current visited_pass, then
     undefined is valid.  Otherwise not.  */
  unsigned int pass;
  /* The still undefined bytes at that time.  The use to which this is
     relative is the current use.  */
  unsigned HOST_WIDE_INT undefined;
  /* Bit regno is set, if that regno is mentioned in this BB as a def, or
     the source of a copy insn.  In these cases we can not skip the whole
     block if we reach it from the end.  */
  bitmap regnos_mentioned;
  /* If a use reaches the end of a BB, and that BB doesn't mention its
     regno, we skip the block, and remember the ID of that use
     as living throughout the whole block.  */
  bitmap live_throughout;
  /* The content of the aux field before placing a pointer to this
     structure there.  */
  void *old_aux;
};

/* We need a fast way to describe a certain part of a register.
   Therefore we put together the size and offset (in bytes) in one
   integer.  */
#define BL_TO_WORD(b, l) ((((b) & 0xFFFF) << 16) | ((l) & 0xFFFF))
#define BYTE_BEGIN(i) (((unsigned int)(i) >> 16) & 0xFFFF)
#define BYTE_LENGTH(i) ((unsigned int)(i) & 0xFFFF)

/* For a REG or SUBREG expression X return the size/offset pair
   as an integer.  */

unsigned int
rtx_to_bits (x)
     rtx x;
{
  unsigned int len, beg;
  len = GET_MODE_SIZE (GET_MODE (x));
  beg = (GET_CODE (x) == SUBREG) ? SUBREG_BYTE (x) : 0;
  return BL_TO_WORD (beg, len);
}

/* X is a REG or SUBREG rtx.  Return the bytes it touches as a bitmask.  */

static unsigned HOST_WIDE_INT
rtx_to_undefined (x)
     rtx x;
{
  unsigned int len, beg;
  unsigned HOST_WIDE_INT ret;
  len = GET_MODE_SIZE (GET_MODE (x));
  beg = (GET_CODE (x) == SUBREG) ? SUBREG_BYTE (x) : 0;
  ret = ~ ((unsigned HOST_WIDE_INT) 0);
  ret = (~(ret << len)) << beg;
  return ret;
}

/* We remember if we've analyzed an insn for being a move insn, and if yes
   between which operands.  */
struct copy_p_cache
{
  int seen;
  rtx source;
  rtx target;
};

/* On demand cache, for if insns are copy insns, and if yes, what
   source/target they have.  */
static struct copy_p_cache * copy_cache;

int *number_seen;

/* For INSN, return nonzero, if it's a move insn, we consider to coalesce
   later, and place the operands in *SOURCE and *TARGET, if they are
   not NULL.  */

static int
copy_insn_p (insn, source, target)
     rtx insn;
     rtx *source;
     rtx *target;
{
  rtx d, s;
  unsigned int d_regno, s_regno;
  int uid = INSN_UID (insn);

  if (!INSN_P (insn))
    abort ();

  /* First look, if we already saw this insn.  */
  if (copy_cache[uid].seen)
    {
      /* And if we saw it, if it's actually a copy insn.  */
      if (copy_cache[uid].seen == 1)
	{
	  if (source)
	    *source = copy_cache[uid].source;
	  if (target)
	    *target = copy_cache[uid].target;
	  return 1;
	}
      return 0;
    }

  /* Mark it as seen, but not being a copy insn.  */
  copy_cache[uid].seen = 2;
  insn = single_set (insn);
  if (!insn)
    return 0;
  d = SET_DEST (insn);
  s = SET_SRC (insn);

  /* We recognize moves between subreg's as copy insns.  This is used to avoid
     conflicts of those subwebs.  But they are currently _not_ used for
     coalescing (the check for this is in remember_move() below).  */
  while (GET_CODE (d) == STRICT_LOW_PART)
    d = XEXP (d, 0);
  if (GET_CODE (d) != REG
      && (GET_CODE (d) != SUBREG || GET_CODE (SUBREG_REG (d)) != REG))
    return 0;
  while (GET_CODE (s) == STRICT_LOW_PART)
    s = XEXP (s, 0);
  if (GET_CODE (s) != REG
      && (GET_CODE (s) != SUBREG || GET_CODE (SUBREG_REG (s)) != REG))
    return 0;

  s_regno = (unsigned) REGNO (GET_CODE (s) == SUBREG ? SUBREG_REG (s) : s);
  d_regno = (unsigned) REGNO (GET_CODE (d) == SUBREG ? SUBREG_REG (d) : d);

  /* Copies between hardregs are useless for us, as not coalesable anyway.  */
  if ((s_regno < FIRST_PSEUDO_REGISTER
       && d_regno < FIRST_PSEUDO_REGISTER)
      || s_regno >= max_normal_pseudo
      || d_regno >= max_normal_pseudo)
    return 0;

  if (source)
    *source = s;
  if (target)
    *target = d;

  /* Still mark it as seen, but as a copy insn this time.  */
  copy_cache[uid].seen = 1;
  copy_cache[uid].source = s;
  copy_cache[uid].target = d;
  return 1;
}

/* We build webs, as we process the conflicts.  For each use we go upward
   the insn stream, noting any defs as potentially conflicting with the
   current use.  We stop at defs of the current regno.  The conflicts are only
   potentially, because we may never reach a def, so this is an undefined use,
   which conflicts with nothing.  */


/* Given a web part WP, and the location of a reg part SIZE_WORD
   return the conflict bitmap for that reg part, or NULL if it doesn't
   exist yet in WP.  */

static bitmap
find_sub_conflicts (wp, size_word)
     struct web_part *wp;
     unsigned int size_word;
{
  struct tagged_conflict *cl;
  cl = wp->sub_conflicts;
  for (; cl; cl = cl->next)
    if (cl->size_word == size_word)
      return cl->conflicts;
  return NULL;
}

/* Similar to find_sub_conflicts(), but creates that bitmap, if it
   doesn't exist.  I.e. this never returns NULL.  */

static bitmap
get_sub_conflicts (wp, size_word)
     struct web_part *wp;
     unsigned int size_word;
{
  bitmap b = find_sub_conflicts (wp, size_word);
  if (!b)
    {
      struct tagged_conflict *cl =
	(struct tagged_conflict *) ra_alloc (sizeof *cl);
      cl->conflicts = BITMAP_XMALLOC ();
      cl->size_word = size_word;
      cl->next = wp->sub_conflicts;
      wp->sub_conflicts = cl;
      b = cl->conflicts;
    }
  return b;
}

/* Helper table for undef_to_size_word() below for small values
   of UNDEFINED.  Offsets and lengths are byte based.  */
static struct undef_table_s {
  unsigned int new_undef;
  /* size | (byte << 16)  */
  unsigned int size_word;
} const undef_table [] = {
  { 0, BL_TO_WORD (0, 0)}, /* 0 */
  { 0, BL_TO_WORD (0, 1)},
  { 0, BL_TO_WORD (1, 1)},
  { 0, BL_TO_WORD (0, 2)},
  { 0, BL_TO_WORD (2, 1)}, /* 4 */
  { 1, BL_TO_WORD (2, 1)},
  { 2, BL_TO_WORD (2, 1)},
  { 3, BL_TO_WORD (2, 1)},
  { 0, BL_TO_WORD (3, 1)}, /* 8 */
  { 1, BL_TO_WORD (3, 1)},
  { 2, BL_TO_WORD (3, 1)},
  { 3, BL_TO_WORD (3, 1)},
  { 0, BL_TO_WORD (2, 2)}, /* 12 */
  { 1, BL_TO_WORD (2, 2)},
  { 2, BL_TO_WORD (2, 2)},
  { 0, BL_TO_WORD (0, 4)}};

/* Interpret *UNDEFINED as bitmask where each bit corresponds to a byte.
   A set bit means an undefined byte.  Factor all undefined bytes into
   groups, and return a size/ofs pair of consecutive undefined bytes,
   but according to certain borders.  Clear out those bits corrsponding
   to bytes overlaid by that size/ofs pair.  REG is only used for
   the mode, to detect if it's a floating mode or not.

   For example:	*UNDEFINED	size+ofs	new *UNDEFINED
		 1111		4+0		  0
		 1100		2+2		  0
		 1101		2+2		  1
		 0001		1+0		  0
		10101		1+4		101

   */

static unsigned int
undef_to_size_word (reg, undefined)
     rtx reg;
     unsigned HOST_WIDE_INT *undefined;
{
  /* When only the lower four bits are possibly set, we use
     a fast lookup table.  */
  if (*undefined <= 15)
    {
      struct undef_table_s u;
      u = undef_table[*undefined];
      *undefined = u.new_undef;
      return u.size_word;
    }

  /* Otherwise we handle certain cases directly.  */
  switch (*undefined)
    {
      case 0x00f0 : *undefined = 0; return BL_TO_WORD (4, 4);
      case 0x00ff : *undefined = 0; return BL_TO_WORD (0, 8);
      case 0x0f00 : *undefined = 0; return BL_TO_WORD (8, 4);
      case 0x0ff0 : *undefined = 0xf0; return BL_TO_WORD (8, 4);
      case 0x0fff :
	if (INTEGRAL_MODE_P (GET_MODE (reg)))
	  { *undefined = 0xff; return BL_TO_WORD (8, 4); }
	else
	  { *undefined = 0; return BL_TO_WORD (0, 12); /* XFmode */ }
      case 0xf000 : *undefined = 0; return BL_TO_WORD (12, 4);
      case 0xff00 : *undefined = 0; return BL_TO_WORD (8, 8);
      case 0xfff0 : *undefined = 0xf0; return BL_TO_WORD (8, 8);
      case 0xffff : *undefined = 0; return BL_TO_WORD (0, 16);

      /* And if nothing matched fall back to the general solution.
	 For now unknown undefined bytes are converted to sequences
	 of maximal length 4 bytes.  We could make this larger if
	 necessary.  */
      default :
	{
	  unsigned HOST_WIDE_INT u = *undefined;
	  int word;
	  struct undef_table_s tab;
	  for (word = 0; (u & 15) == 0; word += 4)
	    u >>= 4;
	  u = u & 15;
	  tab = undef_table[u];
	  u = tab.new_undef;
	  u = (*undefined & ~((unsigned HOST_WIDE_INT)15 << word))
	      | (u << word);
	  *undefined = u;
	  /* Size remains the same, only the begin is moved up move bytes.  */
	  return tab.size_word + BL_TO_WORD (word, 0);
	}
	break;
    }
}

/* Put the above three functions together.  For a set of undefined bytes
   as bitmap *UNDEFINED, look for (create if necessary) and return the
   corresponding conflict bitmap.  Change *UNDEFINED to remove the bytes
   covered by the part for that bitmap.  */

static bitmap
undef_to_bitmap (wp, undefined)
     struct web_part *wp;
     unsigned HOST_WIDE_INT *undefined;
{
  unsigned int size_word = undef_to_size_word (DF_REF_REAL_REG (wp->ref),
					       undefined);
  return get_sub_conflicts (wp, size_word);
}

/* Returns the root of the web part P is a member of.  Additionally
   it compresses the path.  P may not be NULL.  */

static struct web_part *
find_web_part_1 (p)
     struct web_part *p;
{
  struct web_part *r = p;
  struct web_part *p_next;
  while (r->uplink)
    r = r->uplink;
  for (; p != r; p = p_next)
    {
      p_next = p->uplink;
      p->uplink = r;
    }
  return r;
}

/* Fast macro for the common case (WP either being the root itself, or
   the end of an already compressed path.  */

#define find_web_part(wp) ((! (wp)->uplink) ? (wp) \
  : (! (wp)->uplink->uplink) ? (wp)->uplink : find_web_part_1 (wp))

/* Unions together the parts R1 resp. R2 is a root of.
   All dynamic information associated with the parts (number of spanned insns
   and so on) is also merged.
   The root of the resulting (possibly larger) web part is returned.  */

static struct web_part *
union_web_part_roots (r1, r2)
     struct web_part *r1, *r2;
{
  if (r1 != r2)
    {
      /* The new root is the smaller (pointerwise) of both.  This is crucial
         to make the construction of webs from web parts work (so, when
	 scanning all parts, we see the roots before all it's childs).
         Additionally this ensures, that if the web has a def at all, than
         the root is a def (because all def parts are before use parts in the
	 web_parts[] array), or put another way, as soon, as the root of a
         web part is not a def, this is an uninitialized web part.  The
         way we construct the I-graph ensures, that if a web is initialized,
         then the first part we find (besides trivial 1 item parts) has a
         def.  */
      if (r1 > r2)
	{
	  struct web_part *h = r1;
	  r1 = r2;
	  r2 = h;
	}
      r2->uplink = r1;
      num_webs--;

      /* Now we merge the dynamic information of R1 and R2.  */
      r1->spanned_deaths += r2->spanned_deaths;

      if (!r1->sub_conflicts)
	r1->sub_conflicts = r2->sub_conflicts;
      else if (r2->sub_conflicts)
	/* We need to merge the conflict bitmaps from R2 into R1.  */
	{
	  struct tagged_conflict *cl1, *cl2;
	  /* First those from R2, which are also contained in R1.
	     We union the bitmaps, and free those from R2, resetting them
	     to 0.  */
	  for (cl1 = r1->sub_conflicts; cl1; cl1 = cl1->next)
	    for (cl2 = r2->sub_conflicts; cl2; cl2 = cl2->next)
	      if (cl1->size_word == cl2->size_word)
		{
		  bitmap_operation (cl1->conflicts, cl1->conflicts,
				    cl2->conflicts, BITMAP_IOR);
		  BITMAP_XFREE (cl2->conflicts);
		  cl2->conflicts = NULL;
		}
	  /* Now the conflict lists from R2 which weren't in R1.
	     We simply copy the entries from R2 into R1' list.  */
	  for (cl2 = r2->sub_conflicts; cl2;)
	    {
	      struct tagged_conflict *cl_next = cl2->next;
	      if (cl2->conflicts)
		{
		  cl2->next = r1->sub_conflicts;
		  r1->sub_conflicts = cl2;
		}
	      cl2 = cl_next;
	    }
	}
      r2->sub_conflicts = NULL;
      r1->crosses_call |= r2->crosses_call;
    }
  return r1;
}

/* Convenience macro, that is cabable of unioning also non-roots.  */
#define union_web_parts(p1, p2) \
  ((p1 == p2) ? find_web_part (p1) \
      : union_web_part_roots (find_web_part (p1), find_web_part (p2)))

/* Remember that we've handled a given move, so we don't reprocess it.  */

static void
remember_move (insn)
     rtx insn;
{
  if (!TEST_BIT (move_handled, INSN_UID (insn)))
    {
      rtx s, d;
      SET_BIT (move_handled, INSN_UID (insn));
      if (copy_insn_p (insn, &s, &d))
	{
	  /* Some sanity test for the copy insn.  */
	  struct df_link *slink = DF_INSN_USES (df, insn);
	  struct df_link *link = DF_INSN_DEFS (df, insn);
	  if (!link || !link->ref || !slink || !slink->ref)
	    abort ();
	  /* The following (link->next != 0) happens when a hardreg
	     is used in wider mode (REG:DI %eax).  Then df.* creates
	     a def/use for each hardreg contained therein.  We only
	     allow hardregs here.  */
	  if (link->next
	      && DF_REF_REGNO (link->next->ref) >= FIRST_PSEUDO_REGISTER)
	    abort ();
	}
      else
	abort ();
      /* XXX for now we don't remember move insns involving any subregs.
	 Those would be difficult to coalesce (we would need to implement
	 handling of all the subwebs in the allocator, including that such
	 subwebs could be source and target of coalesing).  */
      if (GET_CODE (s) == REG && GET_CODE (d) == REG)
	{
	  struct move *m = (struct move *) ra_calloc (sizeof (struct move));
	  struct move_list *ml;
	  m->insn = insn;
	  ml = (struct move_list *) ra_alloc (sizeof (struct move_list));
	  ml->move = m;
	  ml->next = wl_moves;
	  wl_moves = ml;
	}
    }
}

/* This describes the USE currently looked at in the main-loop in
   build_web_parts_and_conflicts().  */
struct curr_use {
  struct web_part *wp;
  /* This has a 1-bit for each byte in the USE, which is still undefined.  */
  unsigned HOST_WIDE_INT undefined;
  /* For easy access.  */
  unsigned int regno;
  rtx x;
  /* If some bits of this USE are live over an abnormal edge.  */
  unsigned int live_over_abnormal;
};

/* Returns nonzero iff rtx DEF and USE have bits in common (but see below).
   It is only called with DEF and USE being (reg:M a) or (subreg:M1 (reg:M2 a)
   x) rtx's.  Furthermore if it's a subreg rtx M1 is at least one word wide,
   and a is a multi-word pseudo.  If DEF or USE are hardregs, they are in
   word_mode, so we don't need to check for further hardregs which would result
   from wider references.  We are never called with paradoxical subregs.

   This returns:
   0 for no common bits,
   1 if DEF and USE exactly cover the same bytes,
   2 if the DEF only covers a part of the bits of USE
   3 if the DEF covers more than the bits of the USE, and
   4 if both are SUBREG's of different size, but have bytes in common.
   -1 is a special case, for when DEF and USE refer to the same regno, but
      have for other reasons no bits in common (can only happen with
      subregs refering to different words, or to words which already were
      defined for this USE).
   Furthermore it modifies use->undefined to clear the bits which get defined
   by DEF (only for cases with partial overlap).
   I.e. if bit 1 is set for the result != -1, the USE was completely covered,
   otherwise a test is needed to track the already defined bytes.  */

static int
defuse_overlap_p_1 (def, use)
     rtx def;
     struct curr_use *use;
{
  int mode = 0;
  if (def == use->x)
    return 1;
  if (!def)
    return 0;
  if (GET_CODE (def) == SUBREG)
    {
      if (REGNO (SUBREG_REG (def)) != use->regno)
	return 0;
      mode |= 1;
    }
  else if (REGNO (def) != use->regno)
    return 0;
  if (GET_CODE (use->x) == SUBREG)
    mode |= 2;
  switch (mode)
    {
      case 0: /* REG, REG */
	return 1;
      case 1: /* SUBREG, REG */
	{
	  unsigned HOST_WIDE_INT old_u = use->undefined;
	  use->undefined &= ~ rtx_to_undefined (def);
	  return (old_u != use->undefined) ? 2 : -1;
	}
      case 2: /* REG, SUBREG */
	return 3;
      case 3: /* SUBREG, SUBREG */
	if (GET_MODE_SIZE (GET_MODE (def)) == GET_MODE_SIZE (GET_MODE (use->x)))
	  /* If the size of both things is the same, the subreg's overlap
	     if they refer to the same word.  */
	  if (SUBREG_BYTE (def) == SUBREG_BYTE (use->x))
	    return 1;
	/* Now the more difficult part: the same regno is refered, but the
	   sizes of the references or the words differ.  E.g.
           (subreg:SI (reg:CDI a) 0) and (subreg:DI (reg:CDI a) 2) do not
	   overlap, wereas the latter overlaps with (subreg:SI (reg:CDI a) 3).
	   */
	{
	  unsigned HOST_WIDE_INT old_u;
	  int b1, e1, b2, e2;
	  unsigned int bl1, bl2;
	  bl1 = rtx_to_bits (def);
	  bl2 = rtx_to_bits (use->x);
	  b1 = BYTE_BEGIN (bl1);
	  b2 = BYTE_BEGIN (bl2);
	  e1 = b1 + BYTE_LENGTH (bl1) - 1;
	  e2 = b2 + BYTE_LENGTH (bl2) - 1;
	  if (b1 > e2 || b2 > e1)
	    return -1;
	  old_u = use->undefined;
	  use->undefined &= ~ rtx_to_undefined (def);
	  return (old_u != use->undefined) ? 4 : -1;
	}
      default:
        abort ();
    }
}

/* Macro for the common case of either def and use having the same rtx,
   or based on different regnos.  */
#define defuse_overlap_p(def, use) \
  ((def) == (use)->x ? 1 : \
     (REGNO (GET_CODE (def) == SUBREG \
	     ? SUBREG_REG (def) : def) != use->regno \
      ? 0 : defuse_overlap_p_1 (def, use)))


/* The use USE flows into INSN (backwards).  Determine INSNs effect on it,
   and return nonzero, if (parts of) that USE are also live before it.
   This also notes conflicts between the USE and all DEFS in that insn,
   and modifies the undefined bits of USE in case parts of it were set in
   this insn.  */

static int
live_out_1 (df, use, insn)
     struct df *df ATTRIBUTE_UNUSED;
     struct curr_use *use;
     rtx insn;
{
  int defined = 0;
  int uid = INSN_UID (insn);
  struct web_part *wp = use->wp;

  /* Mark, that this insn needs this webpart live.  */
  visit_trace[uid].wp = wp;
  visit_trace[uid].undefined = use->undefined;

  if (INSN_P (insn))
    {
      unsigned int source_regno = ~0;
      unsigned int regno = use->regno;
      unsigned HOST_WIDE_INT orig_undef = use->undefined;
      unsigned HOST_WIDE_INT final_undef = use->undefined;
      rtx s = NULL;
      unsigned int n, num_defs = insn_df[uid].num_defs;
      struct ref **defs = insn_df[uid].defs;

      /* We want to access the root webpart.  */
      wp = find_web_part (wp);
      if (GET_CODE (insn) == CALL_INSN)
	wp->crosses_call = 1;
      else if (copy_insn_p (insn, &s, NULL))
	source_regno = REGNO (GET_CODE (s) == SUBREG ? SUBREG_REG (s) : s);

      /* Look at all DEFS in this insn.  */
      for (n = 0; n < num_defs; n++)
	{
	  struct ref *ref = defs[n];
	  int lap;

	  /* Reset the undefined bits for each iteration, in case this
	     insn has more than one set, and one of them sets this regno.
	     But still the original undefined part conflicts with the other
	     sets.  */
	  use->undefined = orig_undef;
	  if ((lap = defuse_overlap_p (DF_REF_REG (ref), use)) != 0)
	    {
	      if (lap == -1)
		  /* Same regnos but non-overlapping or already defined bits,
		     so ignore this DEF, or better said make the yet undefined
		     part and this DEF conflicting.  */
		{
		  unsigned HOST_WIDE_INT undef;
		  undef = use->undefined;
		  while (undef)
		    bitmap_set_bit (undef_to_bitmap (wp, &undef),
				    DF_REF_ID (ref));
		  continue;
		}
	      if ((lap & 1) != 0)
		  /* The current DEF completely covers the USE, so we can
		     stop traversing the code looking for further DEFs.  */
		defined = 1;
	      else
		  /* We have a partial overlap.  */
		{
		  final_undef &= use->undefined;
		  if (final_undef == 0)
		    /* Now the USE is completely defined, which means, that
		       we can stop looking for former DEFs.  */
		    defined = 1;
		  /* If this is a partial overlap, which left some bits
		     in USE undefined, we normally would need to create
		     conflicts between that undefined part and the part of
		     this DEF which overlapped with some of the formerly
		     undefined bits.  We don't need to do this, because both
		     parts of this DEF (that which overlaps, and that which
		     doesn't) are written together in this one DEF, and can
		     not be colored in a way which would conflict with
		     the USE.  This is only true for partial overlap,
		     because only then the DEF and USE have bits in common,
		     which makes the DEF move, if the USE moves, making them
		     aligned.
		     If they have no bits in common (lap == -1), they are
		     really independent.  Therefore we there made a
		     conflict above.  */
		}
	      /* This is at least a partial overlap, so we need to union
		 the web parts.  */
	      wp = union_web_parts (wp, &web_parts[DF_REF_ID (ref)]);
	    }
	  else
	    {
	      /* The DEF and the USE don't overlap at all, different
		 regnos.  I.e. make conflicts between the undefined bits,
	         and that DEF.  */
	      unsigned HOST_WIDE_INT undef = use->undefined;

	      if (regno == source_regno)
		/* This triggers only, when this was a copy insn and the
		   source is at least a part of the USE currently looked at.
		   In this case only the bits of the USE conflict with the
		   DEF, which are not covered by the source of this copy
		   insn, and which are still undefined.  I.e. in the best
		   case (the whole reg being the source), _no_ conflicts
		   between that USE and this DEF (the target of the move)
		   are created by this insn (though they might be by
		   others).  This is a super case of the normal copy insn
		   only between full regs.  */
		{
		  undef &= ~ rtx_to_undefined (s);
		}
	      if (undef)
		{
		  /*struct web_part *cwp;
		    cwp = find_web_part (&web_parts[DF_REF_ID
		    (ref)]);*/

		  /* TODO: somehow instead of noting the ID of the LINK
		     use an ID nearer to the root webpart of that LINK.
		     We can't use the root itself, because we later use the
		     ID to look at the form (reg or subreg, and if yes,
		     which subreg) of this conflict.  This means, that we
		     need to remember in the root an ID for each form, and
		     maintaining this, when merging web parts.  This makes
		     the bitmaps smaller.  */
		  do
		    bitmap_set_bit (undef_to_bitmap (wp, &undef),
				    DF_REF_ID (ref));
		  while (undef);
		}
	    }
	}
      if (defined)
	use->undefined = 0;
      else
	{
	  /* If this insn doesn't completely define the USE, increment also
	     it's spanned deaths count (if this insn contains a death).  */
	  if (uid >= death_insns_max_uid)
	    abort ();
	  if (TEST_BIT (insns_with_deaths, uid))
	    wp->spanned_deaths++;
	  use->undefined = final_undef;
	}
    }

  return !defined;
}

/* Same as live_out_1() (actually calls it), but caches some information.
   E.g. if we reached this INSN with the current regno already, and the
   current undefined bits are a subset of those as we came here, we
   simply connect the web parts of the USE, and the one cached for this
   INSN, and additionally return zero, indicating we don't need to traverse
   this path any longer (all effect were already seen, as we first reached
   this insn).  */

static inline int
live_out (df, use, insn)
     struct df *df;
     struct curr_use *use;
     rtx insn;
{
  unsigned int uid = INSN_UID (insn);
  if (visit_trace[uid].wp
      && DF_REF_REGNO (visit_trace[uid].wp->ref) == use->regno
      && (use->undefined & ~visit_trace[uid].undefined) == 0)
    {
      union_web_parts (visit_trace[uid].wp, use->wp);
      /* Don't search any further, as we already were here with this regno.  */
      return 0;
    }
  else
    return live_out_1 (df, use, insn);
}

/* The current USE reached a basic block head.  The edge E is one
   of the predecessors edges.  This evaluates the effect of the predecessor
   block onto the USE, and returns the next insn, which should be looked at.
   This either is the last insn of that pred. block, or the first one.
   The latter happens, when the pred. block has no possible effect on the
   USE, except for conflicts.  In that case, it's remembered, that the USE
   is live over that whole block, and it's skipped.  Otherwise we simply
   continue with the last insn of the block.

   This also determines the effects of abnormal edges, and remembers
   which uses are live at the end of that basic block.  */

static rtx
live_in_edge (df, use, e)
     struct df *df;
     struct curr_use *use;
     edge e;
{
  struct ra_bb_info *info_pred;
  rtx next_insn;
  /* Call used hard regs die over an exception edge, ergo
     they don't reach the predecessor block, so ignore such
     uses.  And also don't set the live_over_abnormal flag
     for them.  */
  if ((e->flags & EDGE_EH) && use->regno < FIRST_PSEUDO_REGISTER
      && call_used_regs[use->regno])
    return NULL_RTX;
  if (e->flags & EDGE_ABNORMAL)
    use->live_over_abnormal = 1;
  bitmap_set_bit (live_at_end[e->src->index], DF_REF_ID (use->wp->ref));
  info_pred = (struct ra_bb_info *) e->src->aux;
  next_insn = e->src->end;

  /* If the last insn of the pred. block doesn't completely define the
     current use, we need to check the block.  */
  if (live_out (df, use, next_insn))
    {
      /* If the current regno isn't mentioned anywhere in the whole block,
	 and the complete use is still undefined...  */
      if (!bitmap_bit_p (info_pred->regnos_mentioned, use->regno)
	  && (rtx_to_undefined (use->x) & ~use->undefined) == 0)
	{
	  /* ...we can hop over the whole block and defer conflict
	     creation to later.  */
	  bitmap_set_bit (info_pred->live_throughout,
			  DF_REF_ID (use->wp->ref));
	  next_insn = e->src->head;
	}
      return next_insn;
    }
  else
    return NULL_RTX;
}

/* USE flows into the end of the insns preceding INSN.  Determine
   their effects (in live_out()) and possibly loop over the preceding INSN,
   or call itself recursively on a basic block border.  When a topleve
   call of this function returns the USE is completely analyzed.  I.e.
   its def-use chain (at least) is built, possibly connected with other
   def-use chains, and all defs during that chain are noted.  */

static void
live_in (df, use, insn)
     struct df *df;
     struct curr_use *use;
     rtx insn;
{
  unsigned int loc_vpass = visited_pass;

  /* Note, that, even _if_ we are called with use->wp a root-part, this might
     become non-root in the for() loop below (due to live_out() unioning
     it).  So beware, not to change use->wp in a way, for which only root-webs
     are allowed.  */
  while (1)
    {
      int uid = INSN_UID (insn);
      basic_block bb = BLOCK_FOR_INSN (insn);
      number_seen[uid]++;

      /* We want to be as fast as possible, so explicitely write
	 this loop.  */
      for (insn = PREV_INSN (insn); insn && !INSN_P (insn);
	   insn = PREV_INSN (insn))
	;
      if (!insn)
	return;
      if (bb != BLOCK_FOR_INSN (insn))
	{
	  edge e;
	  unsigned HOST_WIDE_INT undef = use->undefined;
	  struct ra_bb_info *info = (struct ra_bb_info *) bb->aux;
	  if ((e = bb->pred) == NULL)
	    return;
	  /* We now check, if we already traversed the predecessors of this
	     block for the current pass and the current set of undefined
	     bits.  If yes, we don't need to check the predecessors again.
	     So, conceptually this information is tagged to the first
	     insn of a basic block.  */
	  if (info->pass == loc_vpass && (undef & ~info->undefined) == 0)
	    return;
	  info->pass = loc_vpass;
	  info->undefined = undef;
	  /* All but the last predecessor are handled recursively.  */
	  for (; e->pred_next; e = e->pred_next)
	    {
	      insn = live_in_edge (df, use, e);
	      if (insn)
		live_in (df, use, insn);
	      use->undefined = undef;
	    }
	  insn = live_in_edge (df, use, e);
	  if (!insn)
	    return;
	}
      else if (!live_out (df, use, insn))
	return;
    }
}

/* Determine all regnos which are mentioned in a basic block, in an
   interesting way.  Interesting here means either in a def, or as the
   source of a move insn.  We only look at insns added since the last
   pass.  */

static void
update_regnos_mentioned ()
{
  int last_uid = last_max_uid;
  rtx insn;
  basic_block bb;
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    if (INSN_P (insn))
      {
	/* Don't look at old insns.  */
	if (INSN_UID (insn) < last_uid)
	  {
	    /* XXX We should also remember moves over iterations (we already
	       save the cache, but not the movelist).  */
	    if (copy_insn_p (insn, NULL, NULL))
	      remember_move (insn);
	  }
	else if ((bb = BLOCK_FOR_INSN (insn)) != NULL)
	  {
	    rtx source;
	    struct ra_bb_info *info = (struct ra_bb_info *) bb->aux;
	    bitmap mentioned = info->regnos_mentioned;
	    struct df_link *link;
	    if (copy_insn_p (insn, &source, NULL))
	      {
		remember_move (insn);
		bitmap_set_bit (mentioned,
				REGNO (GET_CODE (source) == SUBREG
				       ? SUBREG_REG (source) : source));
	      }
	    for (link = DF_INSN_DEFS (df, insn); link; link = link->next)
	      if (link->ref)
		bitmap_set_bit (mentioned, DF_REF_REGNO (link->ref));
	  }
      }
}

/* Handle the uses which reach a block end, but were defered due
   to it's regno not being mentioned in that block.  This adds the
   remaining conflicts and updates also the crosses_call and
   spanned_deaths members.  */

static void
livethrough_conflicts_bb (bb)
     basic_block bb;
{
  struct ra_bb_info *info = (struct ra_bb_info *) bb->aux;
  rtx insn;
  bitmap all_defs;
  int first, use_id;
  unsigned int deaths = 0;
  unsigned int contains_call = 0;

  /* If there are no defered uses, just return.  */
  if ((first = bitmap_first_set_bit (info->live_throughout)) < 0)
    return;

  /* First collect the IDs of all defs, count the number of death
     containing insns, and if there's some call_insn here.  */
  all_defs = BITMAP_XMALLOC ();
  for (insn = bb->head; insn; insn = NEXT_INSN (insn))
    {
      if (INSN_P (insn))
	{
	  unsigned int n;
	  struct ra_insn_info info;

	  info = insn_df[INSN_UID (insn)];
	  for (n = 0; n < info.num_defs; n++)
	    bitmap_set_bit (all_defs, DF_REF_ID (info.defs[n]));
	  if (TEST_BIT (insns_with_deaths, INSN_UID (insn)))
	    deaths++;
	  if (GET_CODE (insn) == CALL_INSN)
	    contains_call = 1;
	}
      if (insn == bb->end)
	break;
    }

  /* And now, if we have found anything, make all live_through
     uses conflict with all defs, and update their other members.  */
  if (deaths > 0 || bitmap_first_set_bit (all_defs) >= 0)
    EXECUTE_IF_SET_IN_BITMAP (info->live_throughout, first, use_id,
      {
        struct web_part *wp = &web_parts[df->def_id + use_id];
        unsigned int bl = rtx_to_bits (DF_REF_REG (wp->ref));
        bitmap conflicts;
        wp = find_web_part (wp);
        wp->spanned_deaths += deaths;
	wp->crosses_call |= contains_call;
        conflicts = get_sub_conflicts (wp, bl);
        bitmap_operation (conflicts, conflicts, all_defs, BITMAP_IOR);
      });

  BITMAP_XFREE (all_defs);
}

/* Allocate the per basic block info for traversing the insn stream for
   building live ranges.  */

static void
init_bb_info ()
{
  basic_block bb;
  FOR_ALL_BB (bb)
    {
      struct ra_bb_info *info =
	(struct ra_bb_info *) xcalloc (1, sizeof *info);
      info->regnos_mentioned = BITMAP_XMALLOC ();
      info->live_throughout = BITMAP_XMALLOC ();
      info->old_aux = bb->aux;
      bb->aux = (void *) info;
    }
}

/* Free that per basic block info.  */

static void
free_bb_info ()
{
  basic_block bb;
  FOR_ALL_BB (bb)
    {
      struct ra_bb_info *info = (struct ra_bb_info *) bb->aux;
      BITMAP_XFREE (info->regnos_mentioned);
      BITMAP_XFREE (info->live_throughout);
      bb->aux = info->old_aux;
      free (info);
    }
}

/* Toplevel function for the first part of this file.
   Connect web parts, thereby implicitely building webs, and remember
   their conflicts.  */

static void
build_web_parts_and_conflicts (df)
     struct df *df;
{
  struct df_link *link;
  struct curr_use use;
  basic_block bb;

  number_seen = (int *) xcalloc (get_max_uid (), sizeof (int));
  visit_trace = (struct visit_trace *) xcalloc (get_max_uid (),
						sizeof (visit_trace[0]));
  update_regnos_mentioned ();

  /* Here's the main loop.
     It goes through all insn's, connects web parts along the way, notes
     conflicts between webparts, and remembers move instructions.  */
  visited_pass = 0;
  for (use.regno = 0; use.regno < (unsigned int)max_regno; use.regno++)
    if (use.regno >= FIRST_PSEUDO_REGISTER || !fixed_regs[use.regno])
      for (link = df->regs[use.regno].uses; link; link = link->next)
        if (link->ref)
	  {
	    struct ref *ref = link->ref;
	    rtx insn = DF_REF_INSN (ref);
	    /* Only recheck marked or new uses, or uses from hardregs.  */
	    if (use.regno >= FIRST_PSEUDO_REGISTER
		&& DF_REF_ID (ref) < last_use_id
		&& !TEST_BIT (last_check_uses, DF_REF_ID (ref)))
	      continue;
	    use.wp = &web_parts[df->def_id + DF_REF_ID (ref)];
	    use.x = DF_REF_REG (ref);
	    use.live_over_abnormal = 0;
	    use.undefined = rtx_to_undefined (use.x);
	    visited_pass++;
	    live_in (df, &use, insn);
	    if (use.live_over_abnormal)
	      SET_BIT (live_over_abnormal, DF_REF_ID (ref));
	  }

  dump_number_seen ();
  FOR_ALL_BB (bb)
    {
      struct ra_bb_info *info = (struct ra_bb_info *) bb->aux;
      livethrough_conflicts_bb (bb);
      bitmap_zero (info->live_throughout);
      info->pass = 0;
    }
  free (visit_trace);
  free (number_seen);
}

/* Here we look per insn, for DF references being in uses _and_ defs.
   This means, in the RTL a (REG xx) expression was seen as a
   read/modify/write, as happens for (set (subreg:SI (reg:DI xx)) (...))
   e.g.  Our code has created two webs for this, as it should.  Unfortunately,
   as the REG reference is only one time in the RTL we can't color
   both webs different (arguably this also would be wrong for a real
   read-mod-write instruction), so we must reconnect such webs.  */

static void
connect_rmw_web_parts (df)
     struct df *df;
{
  unsigned int i;

  for (i = 0; i < df->use_id; i++)
    {
      struct web_part *wp1 = &web_parts[df->def_id + i];
      rtx reg;
      struct df_link *link;
      if (!wp1->ref)
	continue;
      /* If it's an uninitialized web, we don't want to connect it to others,
	 as the read cycle in read-mod-write had probably no effect.  */
      if (find_web_part (wp1) >= &web_parts[df->def_id])
	continue;
      reg = DF_REF_REAL_REG (wp1->ref);
      link = DF_INSN_DEFS (df, DF_REF_INSN (wp1->ref));
      for (; link; link = link->next)
        if (reg == DF_REF_REAL_REG (link->ref))
	  {
	    struct web_part *wp2 = &web_parts[DF_REF_ID (link->ref)];
	    union_web_parts (wp1, wp2);
	  }
    }
}

/* Deletes all hardregs from *S which are not allowed for MODE.  */

static void
prune_hardregs_for_mode (s, mode)
     HARD_REG_SET *s;
     enum machine_mode mode;
{
  AND_HARD_REG_SET (*s, hardregs_for_mode[(int) mode]);
}

/* Initialize the members of a web, which are deducible from REG.  */

static void
init_one_web_common (web, reg)
     struct web *web;
     rtx reg;
{
  if (GET_CODE (reg) != REG)
    abort ();
  /* web->id isn't initialized here.  */
  web->regno = REGNO (reg);
  web->orig_x = reg;
  if (!web->dlink)
    {
      web->dlink = (struct dlist *) ra_calloc (sizeof (struct dlist));
      DLIST_WEB (web->dlink) = web;
    }
  /* XXX
     the former (superunion) doesn't constrain the graph enough. E.g.
     on x86 QImode _requires_ QI_REGS, but as alternate class usually
     GENERAL_REGS is given.  So the graph is not constrained enough,
     thinking it has more freedom then it really has, which leads
     to repeated spill tryings.  OTOH the latter (only using preferred
     class) is too constrained, as normally (e.g. with all SImode
     pseudos), they can be allocated also in the alternate class.
     What we really want, are the _exact_ hard regs allowed, not
     just a class.  Later.  */
  /*web->regclass = reg_class_superunion
		    [reg_preferred_class (web->regno)]
		    [reg_alternate_class (web->regno)];*/
  /*web->regclass = reg_preferred_class (web->regno);*/
  web->regclass = reg_class_subunion
    [reg_preferred_class (web->regno)] [reg_alternate_class (web->regno)];
  web->regclass = reg_preferred_class (web->regno);
  if (web->regno < FIRST_PSEUDO_REGISTER)
    {
      web->color = web->regno;
      put_web (web, PRECOLORED);
      web->num_conflicts = UINT_MAX;
      web->add_hardregs = 0;
      CLEAR_HARD_REG_SET (web->usable_regs);
      SET_HARD_REG_BIT (web->usable_regs, web->regno);
      web->num_freedom = 1;
    }
  else
    {
      HARD_REG_SET alternate;
      web->color = -1;
      put_web (web, INITIAL);
      /* add_hardregs is wrong in multi-length classes, e.g.
	 using a DFmode pseudo on x86 can result in class FLOAT_INT_REGS,
	 where, if it finally is allocated to GENERAL_REGS it needs two,
	 if allocated to FLOAT_REGS only one hardreg.  XXX */
      web->add_hardregs =
	CLASS_MAX_NREGS (web->regclass, PSEUDO_REGNO_MODE (web->regno)) - 1;
      web->num_conflicts = 0 * web->add_hardregs;
      COPY_HARD_REG_SET (web->usable_regs,
			reg_class_contents[reg_preferred_class (web->regno)]);
      COPY_HARD_REG_SET (alternate,
			reg_class_contents[reg_alternate_class (web->regno)]);
      IOR_HARD_REG_SET (web->usable_regs, alternate);
      /*IOR_HARD_REG_SET (web->usable_regs,
			reg_class_contents[reg_alternate_class
			(web->regno)]);*/
      AND_COMPL_HARD_REG_SET (web->usable_regs, never_use_colors);
      prune_hardregs_for_mode (&web->usable_regs,
			       PSEUDO_REGNO_MODE (web->regno));
#ifdef CLASS_CANNOT_CHANGE_MODE
      if (web->mode_changed)
        AND_COMPL_HARD_REG_SET (web->usable_regs, reg_class_contents[
			          (int) CLASS_CANNOT_CHANGE_MODE]);
#endif
      web->num_freedom = hard_regs_count (web->usable_regs);
      web->num_freedom -= web->add_hardregs;
      if (!web->num_freedom)
	abort();
    }
  COPY_HARD_REG_SET (web->orig_usable_regs, web->usable_regs);
}

/* Initializes WEBs members from REG or zero them.  */

static void
init_one_web (web, reg)
     struct web *web;
     rtx reg;
{
  memset (web, 0, sizeof (struct web));
  init_one_web_common (web, reg);
  web->useless_conflicts = BITMAP_XMALLOC ();
}

/* WEB is an old web, meaning it came from the last pass, and got a
   color.  We want to remember some of it's info, so zero only some
   members.  */

static void
reinit_one_web (web, reg)
     struct web *web;
     rtx reg;
{
  web->old_color = web->color + 1;
  init_one_web_common (web, reg);
  web->span_deaths = 0;
  web->spill_temp = 0;
  web->orig_spill_temp = 0;
  web->use_my_regs = 0;
  web->spill_cost = 0;
  web->was_spilled = 0;
  web->is_coalesced = 0;
  web->artificial = 0;
  web->live_over_abnormal = 0;
  web->mode_changed = 0;
  web->move_related = 0;
  web->in_load = 0;
  web->target_of_spilled_move = 0;
  web->num_aliased = 0;
  if (web->type == PRECOLORED)
    {
      web->num_defs = 0;
      web->num_uses = 0;
      web->orig_spill_cost = 0;
    }
  CLEAR_HARD_REG_SET (web->bias_colors);
  CLEAR_HARD_REG_SET (web->prefer_colors);
  web->reg_rtx = NULL;
  web->stack_slot = NULL;
  web->pattern = NULL;
  web->alias = NULL;
  if (web->moves)
    abort ();
  if (!web->useless_conflicts)
    abort ();
}

/* Insert and returns a subweb corresponding to REG into WEB (which
   becomes its super web).  It must not exist already.  */

static struct web *
add_subweb (web, reg)
     struct web *web;
     rtx reg;
{
  struct web *w;
  if (GET_CODE (reg) != SUBREG)
    abort ();
  w = (struct web *) xmalloc (sizeof (struct web));
  /* Copy most content from parent-web.  */
  *w = *web;
  /* And initialize the private stuff.  */
  w->orig_x = reg;
  w->add_hardregs = CLASS_MAX_NREGS (web->regclass, GET_MODE (reg)) - 1;
  w->num_conflicts = 0 * w->add_hardregs;
  w->num_defs = 0;
  w->num_uses = 0;
  w->dlink = NULL;
  w->parent_web = web;
  w->subreg_next = web->subreg_next;
  web->subreg_next = w;
  return w;
}

/* Similar to add_subweb(), but instead of relying on a given SUBREG,
   we have just a size and an offset of the subpart of the REG rtx.
   In difference to add_subweb() this marks the new subweb as artificial.  */

static struct web *
add_subweb_2 (web, size_word)
     struct web *web;
     unsigned int size_word;
{
  /* To get a correct mode for the to be produced subreg, we don't want to
     simply do a mode_for_size() for the mode_class of the whole web.
     Suppose we deal with a CDImode web, but search for a 8 byte part.
     Now mode_for_size() would only search in the class MODE_COMPLEX_INT
     and would find CSImode which probably is not what we want.  Instead
     we want DImode, which is in a completely other class.  For this to work
     we instead first search the already existing subwebs, and take
     _their_ modeclasses as base for a search for ourself.  */
  rtx ref_rtx = (web->subreg_next ? web->subreg_next : web)->orig_x;
  unsigned int size = BYTE_LENGTH (size_word) * BITS_PER_UNIT;
  enum machine_mode mode;
  mode = mode_for_size (size, GET_MODE_CLASS (GET_MODE (ref_rtx)), 0);
  if (mode == BLKmode)
    mode = mode_for_size (size, MODE_INT, 0);
  if (mode == BLKmode)
    abort ();
  web = add_subweb (web, gen_rtx_SUBREG (mode, web->orig_x,
					 BYTE_BEGIN (size_word)));
  web->artificial = 1;
  return web;
}

/* Initialize all the web parts we are going to need.  */

static void
init_web_parts (df)
     struct df *df;
{
  int regno;
  unsigned int no;
  num_webs = 0;
  for (no = 0; no < df->def_id; no++)
    {
      if (df->defs[no])
	{
	  if (no < last_def_id && web_parts[no].ref != df->defs[no])
	    abort ();
	  web_parts[no].ref = df->defs[no];
	  /* Uplink might be set from the last iteration.  */
	  if (!web_parts[no].uplink)
	    num_webs++;
	}
      else
	/* The last iteration might have left .ref set, while df_analyse()
	   removed that ref (due to a removed copy insn) from the df->defs[]
	   array.  As we don't check for that in realloc_web_parts()
	   we do that here.  */
	web_parts[no].ref = NULL;
    }
  for (no = 0; no < df->use_id; no++)
    {
      if (df->uses[no])
	{
	  if (no < last_use_id
	      && web_parts[no + df->def_id].ref != df->uses[no])
	    abort ();
	  web_parts[no + df->def_id].ref = df->uses[no];
	  if (!web_parts[no + df->def_id].uplink)
	    num_webs++;
	}
      else
	web_parts[no + df->def_id].ref = NULL;
    }

  /* We want to have only one web for each precolored register.  */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    {
      struct web_part *r1 = NULL;
      struct df_link *link;
      /* Here once was a test, if there is any DEF at all, and only then to
	 merge all the parts.  This was incorrect, we really also want to have
	 only one web-part for hardregs, even if there is no explicit DEF.  */
      /* Link together all defs...  */
      for (link = df->regs[regno].defs; link; link = link->next)
        if (link->ref)
	  {
	    struct web_part *r2 = &web_parts[DF_REF_ID (link->ref)];
	    if (!r1)
	      r1 = r2;
	    else
	      r1 = union_web_parts (r1, r2);
	  }
      /* ... and all uses.  */
      for (link = df->regs[regno].uses; link; link = link->next)
	if (link->ref)
	  {
	    struct web_part *r2 = &web_parts[df->def_id
		                             + DF_REF_ID (link->ref)];
	    if (!r1)
	      r1 = r2;
	    else
	      r1 = union_web_parts (r1, r2);
	  }
    }
}

/* In case we want to remember the conflict list of a WEB, before adding
   new conflicts, we copy it here to orig_conflict_list.  */

static void
copy_conflict_list (web)
     struct web *web;
{
  struct conflict_link *cl;
  if (web->orig_conflict_list || web->have_orig_conflicts)
    abort ();
  web->have_orig_conflicts = 1;
  for (cl = web->conflict_list; cl; cl = cl->next)
    {
      struct conflict_link *ncl;
      ncl = (struct conflict_link *) ra_alloc (sizeof *ncl);
      ncl->t = cl->t;
      ncl->sub = NULL;
      ncl->next = web->orig_conflict_list;
      web->orig_conflict_list = ncl;
      if (cl->sub)
	{
	  struct sub_conflict *sl, *nsl;
	  for (sl = cl->sub; sl; sl = sl->next)
	    {
	      nsl = (struct sub_conflict *) ra_alloc (sizeof *nsl);
	      nsl->s = sl->s;
	      nsl->t = sl->t;
	      nsl->next = ncl->sub;
	      ncl->sub = nsl;
	    }
	}
    }
}

/* Possibly add an edge from web FROM to TO marking a conflict between
   those two.  This is one half of marking a complete conflict, which notes
   in FROM, that TO is a conflict.  Adding TO to FROM's conflicts might
   make other conflicts superflous, because the current TO overlaps some web
   already being in conflict with FROM.  In this case the smaller webs are
   deleted from the conflict list.  Likewise if TO is overlapped by a web
   already in the list, it isn't added at all.  Note, that this can only
   happen, if SUBREG webs are involved.  */

static void
add_conflict_edge (from, to)
     struct web *from, *to;
{
  if (from->type != PRECOLORED)
    {
      struct web *pfrom = find_web_for_subweb (from);
      struct web *pto = find_web_for_subweb (to);
      struct sub_conflict *sl;
      struct conflict_link *cl = pfrom->conflict_list;
      int may_delete = 1;

      /* This can happen when subwebs of one web conflict with each
	 other.  In live_out_1() we created such conflicts between yet
	 undefined webparts and defs of parts which didn't overlap with the
	 undefined bits.  Then later they nevertheless could have merged into
	 one web, and then we land here.  */
      if (pfrom == pto)
	return;
      if (remember_conflicts && !pfrom->have_orig_conflicts)
	copy_conflict_list (pfrom);
      if (!TEST_BIT (sup_igraph, (pfrom->id * num_webs + pto->id)))
	{
	  cl = (struct conflict_link *) ra_alloc (sizeof (*cl));
	  cl->t = pto;
	  cl->sub = NULL;
	  cl->next = pfrom->conflict_list;
	  pfrom->conflict_list = cl;
	  if (pto->type != SELECT && pto->type != COALESCED)
	    pfrom->num_conflicts += 1 + pto->add_hardregs;
          SET_BIT (sup_igraph, (pfrom->id * num_webs + pto->id));
	  may_delete = 0;
	}
      else
        /* We don't need to test for cl==NULL, because at this point
	   a cl with cl->t==pto is guaranteed to exist.  */
        while (cl->t != pto)
	  cl = cl->next;
      if (pfrom != from || pto != to)
	{
	  /* This is a subconflict which should be added.
	     If we inserted cl in this invocation, we really need to add this
	     subconflict.  If we did _not_ add it here, we only add the
	     subconflict, if cl already had subconflicts, because otherwise
	     this indicated, that the whole webs already conflict, which
	     means we are not interested in this subconflict.  */
	  if (!may_delete || cl->sub != NULL)
	    {
	      sl = (struct sub_conflict *) ra_alloc (sizeof (*sl));
	      sl->s = from;
	      sl->t = to;
	      sl->next = cl->sub;
	      cl->sub = sl;
	    }
	}
      else
	/* pfrom == from && pto == to means, that we are not interested
	   anymore in the subconflict list for this pair, because anyway
	   the whole webs conflict.  */
	cl->sub = NULL;
    }
}

/* Record a conflict between two webs, if we haven't recorded it
   already.  */

void
record_conflict (web1, web2)
     struct web *web1, *web2;
{
  unsigned int id1 = web1->id, id2 = web2->id;
  unsigned int index = igraph_index (id1, id2);
  /* Trivial non-conflict or already recorded conflict.  */
  if (web1 == web2 || TEST_BIT (igraph, index))
    return;
  if (id1 == id2)
    abort ();
  /* As fixed_regs are no targets for allocation, conflicts with them
     are pointless.  */
  if ((web1->regno < FIRST_PSEUDO_REGISTER && fixed_regs[web1->regno])
      || (web2->regno < FIRST_PSEUDO_REGISTER && fixed_regs[web2->regno]))
    return;
  /* Conflicts with hardregs, which are not even a candidate
     for this pseudo are also pointless.  */
  if ((web1->type == PRECOLORED
       && ! TEST_HARD_REG_BIT (web2->usable_regs, web1->regno))
      || (web2->type == PRECOLORED
	  && ! TEST_HARD_REG_BIT (web1->usable_regs, web2->regno)))
    return;
  /* Similar if the set of possible hardregs don't intersect.  This iteration
     those conflicts are useless (and would make num_conflicts wrong, because
     num_freedom is calculated from the set of possible hardregs).
     But in presence of spilling and incremental building of the graph we
     need to note all uses of webs conflicting with the spilled ones.
     Because the set of possible hardregs can change in the next round for
     spilled webs, we possibly have then conflicts with webs which would
     be excluded now (because then hardregs intersect).  But we actually
     need to check those uses, and to get hold of them, we need to remember
     also webs conflicting with this one, although not conflicting in this
     round because of non-intersecting hardregs.  */
  if (web1->type != PRECOLORED && web2->type != PRECOLORED
      && ! hard_regs_intersect_p (&web1->usable_regs, &web2->usable_regs))
    {
      struct web *p1 = find_web_for_subweb (web1);
      struct web *p2 = find_web_for_subweb (web2);
      /* We expect these to be rare enough to justify bitmaps.  And because
         we have only a special use for it, we note only the superwebs.  */
      bitmap_set_bit (p1->useless_conflicts, p2->id);
      bitmap_set_bit (p2->useless_conflicts, p1->id);
      return;
    }
  SET_BIT (igraph, index);
  add_conflict_edge (web1, web2);
  add_conflict_edge (web2, web1);
}

/* For each web W this produces the missing subwebs Wx, such that it's
   possible to exactly specify (W-Wy) for all already existing subwebs Wy.  */

static void
build_inverse_webs (web)
     struct web *web;
{
  struct web *sweb = web->subreg_next;
  unsigned HOST_WIDE_INT undef;

  undef = rtx_to_undefined (web->orig_x);
  for (; sweb; sweb = sweb->subreg_next)
    /* Only create inverses of non-artificial webs.  */
    if (!sweb->artificial)
      {
	unsigned HOST_WIDE_INT bits;
	bits = undef & ~ rtx_to_undefined (sweb->orig_x);
	while (bits)
	  {
	    unsigned int size_word = undef_to_size_word (web->orig_x, &bits);
	    if (!find_subweb_2 (web, size_word))
	      add_subweb_2 (web, size_word);
	  }
      }
}

/* Copies the content of WEB to a new one, and link it into WL.
   Used for consistency checking.  */

static void
copy_web (web, wl)
     struct web *web;
     struct web_link **wl;
{
  struct web *cweb = (struct web *) xmalloc (sizeof *cweb);
  struct web_link *link = (struct web_link *) ra_alloc (sizeof *link);
  link->next = *wl;
  *wl = link;
  link->web = cweb;
  *cweb = *web;
}

/* Given a list of webs LINK, compare the content of the webs therein
   with the global webs of the same ID.  For consistency checking.  */

static void
compare_and_free_webs (link)
     struct web_link **link;
{
  struct web_link *wl;
  for (wl = *link; wl; wl = wl->next)
    {
      struct web *web1 = wl->web;
      struct web *web2 = ID2WEB (web1->id);
      if (web1->regno != web2->regno
	  || web1->crosses_call != web2->crosses_call
	  || web1->live_over_abnormal != web2->live_over_abnormal
	  || web1->mode_changed != web2->mode_changed
	  || !rtx_equal_p (web1->orig_x, web2->orig_x)
	  || web1->type != web2->type
	  /* Only compare num_defs/num_uses with non-hardreg webs.
	     E.g. the number of uses of the framepointer changes due to
	     inserting spill code.  */
	  || (web1->type != PRECOLORED &&
	      (web1->num_uses != web2->num_uses
	       || web1->num_defs != web2->num_defs)))
	abort ();
      if (web1->type != PRECOLORED)
	{
	  unsigned int i;
	  for (i = 0; i < web1->num_defs; i++)
	    if (web1->defs[i] != web2->defs[i])
	      abort ();
	  for (i = 0; i < web1->num_uses; i++)
	    if (web1->uses[i] != web2->uses[i])
	      abort ();
	}
      if (web1->type == PRECOLORED)
	{
	  if (web1->defs)
	    free (web1->defs);
	  if (web1->uses)
	    free (web1->uses);
	}
      free (web1);
    }
  *link = NULL;
}

/* Setup and fill uses[] and defs[] arrays of the webs.  */

static void
init_webs_defs_uses ()
{
  struct dlist *d;
  for (d = WEBS(INITIAL); d; d = d->next)
    {
      struct web *web = DLIST_WEB (d);
      unsigned int def_i, use_i;
      struct df_link *link;
      if (web->old_web)
	continue;
      if (web->type == PRECOLORED)
	{
	  web->num_defs = web->num_uses = 0;
	  continue;
	}
      if (web->num_defs)
        web->defs = (struct ref **) xmalloc (web->num_defs *
					     sizeof (web->defs[0]));
      if (web->num_uses)
        web->uses = (struct ref **) xmalloc (web->num_uses *
					     sizeof (web->uses[0]));
      def_i = use_i = 0;
      for (link = web->temp_refs; link; link = link->next)
	{
	  if (DF_REF_REG_DEF_P (link->ref))
	    web->defs[def_i++] = link->ref;
	  else
	    web->uses[use_i++] = link->ref;
	}
      web->temp_refs = NULL;
      if (def_i != web->num_defs || use_i != web->num_uses)
	abort ();
    }
}

/* Called by parts_to_webs().  This creates (or recreates) the webs (and
   subwebs) from web parts, gives them IDs (only to super webs), and sets
   up use2web and def2web arrays.  */

static unsigned int
parts_to_webs_1 (df, copy_webs, all_refs)
     struct df *df;
     struct web_link **copy_webs;
     struct df_link *all_refs;
{
  unsigned int i;
  unsigned int webnum;
  unsigned int def_id = df->def_id;
  unsigned int use_id = df->use_id;
  struct web_part *wp_first_use = &web_parts[def_id];

  /* For each root web part: create and initialize a new web,
     setup def2web[] and use2web[] for all defs and uses, and
     id2web for all new webs.  */

  webnum = 0;
  for (i = 0; i < def_id + use_id; i++)
    {
      struct web *subweb, *web = 0; /* Initialize web to silence warnings.  */
      struct web_part *wp = &web_parts[i];
      struct ref *ref = wp->ref;
      unsigned int ref_id;
      rtx reg;
      if (!ref)
	continue;
      ref_id = i;
      if (i >= def_id)
	ref_id -= def_id;
      all_refs[i].ref = ref;
      reg = DF_REF_REG (ref);
      if (! wp->uplink)
	{
	  /* If we have a web part root, create a new web.  */
	  unsigned int newid = ~(unsigned)0;
	  unsigned int old_web = 0;

	  /* In the first pass, there are no old webs, so unconditionally
	     allocate a new one.  */
	  if (ra_pass == 1)
	    {
	      web = (struct web *) xmalloc (sizeof (struct web));
	      newid = last_num_webs++;
	      init_one_web (web, GET_CODE (reg) == SUBREG
			         ? SUBREG_REG (reg) : reg);
	    }
	  /* Otherwise, we look for an old web.  */
	  else
	    {
	      /* Remember, that use2web == def2web + def_id.
		 Ergo is def2web[i] == use2web[i - def_id] for i >= def_id.
		 So we only need to look into def2web[] array.
		 Try to look at the web, which formerly belonged to this
		 def (or use).  */
	      web = def2web[i];
	      /* Or which belonged to this hardreg.  */
	      if (!web && DF_REF_REGNO (ref) < FIRST_PSEUDO_REGISTER)
		web = hardreg2web[DF_REF_REGNO (ref)];
	      if (web)
		{
		  /* If we found one, reuse it.  */
		  web = find_web_for_subweb (web);
		  remove_list (web->dlink, &WEBS(INITIAL));
		  old_web = 1;
		  copy_web (web, copy_webs);
		}
	      else
		{
		  /* Otherwise use a new one.  First from the free list.  */
		  if (WEBS(FREE))
		    web = DLIST_WEB (pop_list (&WEBS(FREE)));
		  else
		    {
		      /* Else allocate a new one.  */
		      web = (struct web *) xmalloc (sizeof (struct web));
		      newid = last_num_webs++;
		    }
		}
	      /* The id is zeroed in init_one_web().  */
	      if (newid == ~(unsigned)0)
		newid = web->id;
	      if (old_web)
		reinit_one_web (web, GET_CODE (reg) == SUBREG
				     ? SUBREG_REG (reg) : reg);
	      else
		init_one_web (web, GET_CODE (reg) == SUBREG
				   ? SUBREG_REG (reg) : reg);
	      web->old_web = (old_web && web->type != PRECOLORED) ? 1 : 0;
	    }
	  web->span_deaths = wp->spanned_deaths;
	  web->crosses_call = wp->crosses_call;
	  web->id = newid;
	  web->temp_refs = NULL;
	  webnum++;
	  if (web->regno < FIRST_PSEUDO_REGISTER && !hardreg2web[web->regno])
	    hardreg2web[web->regno] = web;
	  else if (web->regno < FIRST_PSEUDO_REGISTER
		   && hardreg2web[web->regno] != web)
	    abort ();
	}

      /* If this reference already had a web assigned, we are done.
         This test better is equivalent to the web being an old web.
         Otherwise something is screwed.  (This is tested)  */
      if (def2web[i] != NULL)
	{
	  web = def2web[i];
	  web = find_web_for_subweb (web);
	  /* But if this ref includes a mode change, or was a use live
	     over an abnormal call, set appropriate flags in the web.  */
	  if ((DF_REF_FLAGS (ref) & DF_REF_MODE_CHANGE) != 0
	      && web->regno >= FIRST_PSEUDO_REGISTER)
	    web->mode_changed = 1;
	  if (i >= def_id
	      && TEST_BIT (live_over_abnormal, ref_id))
	    web->live_over_abnormal = 1;
	  /* And check, that it's not a newly allocated web.  This would be
	     an inconsistency.  */
	  if (!web->old_web || web->type == PRECOLORED)
	    abort ();
	  continue;
	}
      /* In case this was no web part root, we need to initialize WEB
	 from the ref2web array belonging to the root.  */
      if (wp->uplink)
	{
	  struct web_part *rwp = find_web_part (wp);
	  unsigned int j = DF_REF_ID (rwp->ref);
	  if (rwp < wp_first_use)
	    web = def2web[j];
	  else
	    web = use2web[j];
	  web = find_web_for_subweb (web);
	}

      /* Remember all references for a web in a single linked list.  */
      all_refs[i].next = web->temp_refs;
      web->temp_refs = &all_refs[i];

      /* And the test, that if def2web[i] was NULL above, that we are _not_
	 an old web.  */
      if (web->old_web && web->type != PRECOLORED)
	abort ();

      /* Possible create a subweb, if this ref was a subreg.  */
      if (GET_CODE (reg) == SUBREG)
	{
	  subweb = find_subweb (web, reg);
	  if (!subweb)
	    {
	      subweb = add_subweb (web, reg);
	      if (web->old_web)
		abort ();
	    }
	}
      else
	subweb = web;

      /* And look, if the ref involves an invalid mode change.  */
      if ((DF_REF_FLAGS (ref) & DF_REF_MODE_CHANGE) != 0
	  && web->regno >= FIRST_PSEUDO_REGISTER)
	web->mode_changed = 1;

      /* Setup def2web, or use2web, and increment num_defs or num_uses.  */
      if (i < def_id)
	{
	  /* Some sanity checks.  */
	  if (ra_pass > 1)
	    {
	      struct web *compare = def2web[i];
	      if (i < last_def_id)
		{
		  if (web->old_web && compare != subweb)
		    abort ();
		}
	      if (!web->old_web && compare)
		abort ();
	      if (compare && compare != subweb)
		abort ();
	    }
	  def2web[i] = subweb;
	  web->num_defs++;
	}
      else
	{
	  if (ra_pass > 1)
	    {
	      struct web *compare = use2web[ref_id];
	      if (ref_id < last_use_id)
		{
		  if (web->old_web && compare != subweb)
		    abort ();
		}
	      if (!web->old_web && compare)
		abort ();
	      if (compare && compare != subweb)
		abort ();
	    }
	  use2web[ref_id] = subweb;
	  web->num_uses++;
	  if (TEST_BIT (live_over_abnormal, ref_id))
	    web->live_over_abnormal = 1;
	}
    }

  /* We better now have exactly as many webs as we had web part roots.  */
  if (webnum != num_webs)
    abort ();

  return webnum;
}

/* This builds full webs out of web parts, without relating them to each
   other (i.e. without creating the conflict edges).  */

static void
parts_to_webs (df)
     struct df *df;
{
  unsigned int i;
  unsigned int webnum;
  struct web_link *copy_webs = NULL;
  struct dlist *d;
  struct df_link *all_refs;
  num_subwebs = 0;

  /* First build webs and ordinary subwebs.  */
  all_refs = (struct df_link *) xcalloc (df->def_id + df->use_id,
					 sizeof (all_refs[0]));
  webnum = parts_to_webs_1 (df, &copy_webs, all_refs);

  /* Setup the webs for hardregs which are still missing (weren't
     mentioned in the code).  */
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (!hardreg2web[i])
      {
	struct web *web = (struct web *) xmalloc (sizeof (struct web));
	init_one_web (web, gen_rtx_REG (reg_raw_mode[i], i));
	web->id = last_num_webs++;
	hardreg2web[web->regno] = web;
      }
  num_webs = last_num_webs;

  /* Now create all artificial subwebs, i.e. those, which do
     not correspond to a real subreg in the current function's RTL, but
     which nevertheless is a target of a conflict.
     XXX we need to merge this loop with the one above, which means, we need
     a way to later override the artificiality.  Beware: currently
     add_subweb_2() relies on the existence of normal subwebs for deducing
     a sane mode to use for the artificial subwebs.  */
  for (i = 0; i < df->def_id + df->use_id; i++)
    {
      struct web_part *wp = &web_parts[i];
      struct tagged_conflict *cl;
      struct web *web;
      if (wp->uplink || !wp->ref)
	{
	  if (wp->sub_conflicts)
	    abort ();
	  continue;
	}
      web = def2web[i];
      web = find_web_for_subweb (web);
      for (cl = wp->sub_conflicts; cl; cl = cl->next)
        if (!find_subweb_2 (web, cl->size_word))
	  add_subweb_2 (web, cl->size_word);
    }

  /* And now create artificial subwebs needed for representing the inverse
     of some subwebs.  This also gives IDs to all subwebs.  */
  webnum = last_num_webs;
  for (d = WEBS(INITIAL); d; d = d->next)
    {
      struct web *web = DLIST_WEB (d);
      if (web->subreg_next)
	{
	  struct web *sweb;
          build_inverse_webs (web);
	  for (sweb = web->subreg_next; sweb; sweb = sweb->subreg_next)
	    sweb->id = webnum++;
	}
    }

  /* Now that everyone has an ID, we can setup the id2web array.  */
  id2web = (struct web **) xcalloc (webnum, sizeof (id2web[0]));
  for (d = WEBS(INITIAL); d; d = d->next)
    {
      struct web *web = DLIST_WEB (d);
      ID2WEB (web->id) = web;
      for (web = web->subreg_next; web; web = web->subreg_next)
        ID2WEB (web->id) = web;
    }
  num_subwebs = webnum - last_num_webs;
  num_allwebs = num_webs + num_subwebs;
  num_webs += num_subwebs;

  /* Allocate and clear the conflict graph bitmaps.  */
  igraph = sbitmap_alloc (num_webs * num_webs / 2);
  sup_igraph = sbitmap_alloc (num_webs * num_webs);
  sbitmap_zero (igraph);
  sbitmap_zero (sup_igraph);

  /* Distibute the references to their webs.  */
  init_webs_defs_uses ();
  /* And do some sanity checks if old webs, and those recreated from the
     really are the same.  */
  compare_and_free_webs (&copy_webs);
  free (all_refs);
}

/* This deletes all conflicts to and from webs which need to be renewed
   in this pass of the allocator, i.e. those which were spilled in the
   last pass.  Furthermore it also rebuilds the bitmaps for the remaining
   conflicts.  */

static void
reset_conflicts ()
{
  unsigned int i;
  bitmap newwebs = BITMAP_XMALLOC ();
  for (i = 0; i < num_webs - num_subwebs; i++)
    {
      struct web *web = ID2WEB (i);
      /* Hardreg webs and non-old webs are new webs (which
	 need rebuilding).  */
      if (web->type == PRECOLORED || !web->old_web)
	bitmap_set_bit (newwebs, web->id);
    }

  for (i = 0; i < num_webs - num_subwebs; i++)
    {
      struct web *web = ID2WEB (i);
      struct conflict_link *cl;
      struct conflict_link **pcl;
      pcl = &(web->conflict_list);

      /* First restore the conflict list to be like it was before
	 coalescing.  */
      if (web->have_orig_conflicts)
	{
	  web->conflict_list = web->orig_conflict_list;
	  web->orig_conflict_list = NULL;
	}
      if (web->orig_conflict_list)
	abort ();

      /* New non-precolored webs, have no conflict list.  */
      if (web->type != PRECOLORED && !web->old_web)
	{
	  *pcl = NULL;
	  /* Useless conflicts will be rebuilt completely.  But check
	     for cleanlyness, as the web might have come from the
	     free list.  */
	  if (bitmap_first_set_bit (web->useless_conflicts) >= 0)
	    abort ();
	}
      else
	{
	  /* Useless conflicts with new webs will be rebuilt if they
	     are still there.  */
	  bitmap_operation (web->useless_conflicts, web->useless_conflicts,
			    newwebs, BITMAP_AND_COMPL);
	  /* Go through all conflicts, and retain those to old webs.  */
	  for (cl = web->conflict_list; cl; cl = cl->next)
	    {
	      if (cl->t->old_web || cl->t->type == PRECOLORED)
		{
		  *pcl = cl;
		  pcl = &(cl->next);

		  /* Also restore the entries in the igraph bitmaps.  */
		  web->num_conflicts += 1 + cl->t->add_hardregs;
		  SET_BIT (sup_igraph, (web->id * num_webs + cl->t->id));
		  /* No subconflicts mean full webs conflict.  */
		  if (!cl->sub)
		    SET_BIT (igraph, igraph_index (web->id, cl->t->id));
		  else
		    /* Else only the parts in cl->sub must be in the
		       bitmap.  */
		    {
		      struct sub_conflict *sl;
		      for (sl = cl->sub; sl; sl = sl->next)
			SET_BIT (igraph, igraph_index (sl->s->id, sl->t->id));
		    }
		}
	    }
	  *pcl = NULL;
	}
      web->have_orig_conflicts = 0;
    }
  BITMAP_XFREE (newwebs);
}

/* For each web check it's num_conflicts member against that
   number, as calculated from scratch from all neighbors.  */

#if 0
static void
check_conflict_numbers ()
{
  unsigned int i;
  for (i = 0; i < num_webs; i++)
    {
      struct web *web = ID2WEB (i);
      int new_conf = 0 * web->add_hardregs;
      struct conflict_link *cl;
      for (cl = web->conflict_list; cl; cl = cl->next)
	if (cl->t->type != SELECT && cl->t->type != COALESCED)
	  new_conf += 1 + cl->t->add_hardregs;
      if (web->type != PRECOLORED && new_conf != web->num_conflicts)
	abort ();
    }
}
#endif

/* Convert the conflicts between web parts to conflicts between full webs.

   This can't be done in parts_to_webs(), because for recording conflicts
   between webs we need to know their final usable_regs set, which is used
   to discard non-conflicts (between webs having no hard reg in common).
   But this is set for spill temporaries only after the webs itself are
   built.  Until then the usable_regs set is based on the pseudo regno used
   in this web, which may contain far less registers than later determined.
   This would result in us loosing conflicts (due to record_conflict()
   thinking that a web can only be allocated to the current usable_regs,
   whereas later this is extended) leading to colorings, where some regs which
   in reality conflict get the same color.  */

static void
conflicts_between_webs (df)
     struct df *df;
{
  unsigned int i;
#ifdef STACK_REGS
  struct dlist *d;
#endif
  bitmap ignore_defs = BITMAP_XMALLOC ();
  unsigned int have_ignored;
  unsigned int *pass_cache = (unsigned int *) xcalloc (num_webs, sizeof (int));
  unsigned int pass = 0;

  if (ra_pass > 1)
    reset_conflicts ();

  /* It is possible, that in the conflict bitmaps still some defs I are noted,
     which have web_parts[I].ref being NULL.  This can happen, when from the
     last iteration the conflict bitmap for this part wasn't deleted, but a
     conflicting move insn was removed.  It's DEF is still in the conflict
     bitmap, but it doesn't exist anymore in df->defs.  To not have to check
     it in the tight loop below, we instead remember the ID's of them in a
     bitmap, and loop only over IDs which are not in it.  */
  for (i = 0; i < df->def_id; i++)
    if (web_parts[i].ref == NULL)
      bitmap_set_bit (ignore_defs, i);
  have_ignored = (bitmap_first_set_bit (ignore_defs) >= 0);

  /* Now record all conflicts between webs.  Note that we only check
     the conflict bitmaps of all defs.  Conflict bitmaps are only in
     webpart roots.  If they are in uses, those uses are roots, which
     means, that this is an uninitialized web, whose conflicts
     don't matter.  Nevertheless for hardregs we also need to check uses.
     E.g. hardregs used for argument passing have no DEF in the RTL,
     but if they have uses, they indeed conflict with all DEFs they
     overlap.  */
  for (i = 0; i < df->def_id + df->use_id; i++)
    {
      struct tagged_conflict *cl = web_parts[i].sub_conflicts;
      struct web *supweb1;
      if (!cl
	  || (i >= df->def_id
	      && DF_REF_REGNO (web_parts[i].ref) >= FIRST_PSEUDO_REGISTER))
	continue;
      supweb1 = def2web[i];
      supweb1 = find_web_for_subweb (supweb1);
      for (; cl; cl = cl->next)
        if (cl->conflicts)
	  {
	    int j;
	    struct web *web1 = find_subweb_2 (supweb1, cl->size_word);
	    if (have_ignored)
	      bitmap_operation (cl->conflicts, cl->conflicts, ignore_defs,
			        BITMAP_AND_COMPL);
	    /* We reduce the number of calls to record_conflict() with this
	       pass thing.  record_conflict() itself also has some early-out
	       optimizations, but here we can use the special properties of
	       the loop (constant web1) to reduce that even more.
	       We once used an sbitmap of already handled web indices,
	       but sbitmaps are slow to clear and bitmaps are slow to
	       set/test.  The current approach needs more memory, but
	       locality is large.  */
	    pass++;

	    /* Note, that there are only defs in the conflicts bitset.  */
	    EXECUTE_IF_SET_IN_BITMAP (
	      cl->conflicts, 0, j,
	      {
		struct web *web2 = def2web[j];
		unsigned int id2 = web2->id;
		if (pass_cache[id2] != pass)
		  {
		    pass_cache[id2] = pass;
		    record_conflict (web1, web2);
		  }
	      });
	  }
    }

  free (pass_cache);
  BITMAP_XFREE (ignore_defs);

#ifdef STACK_REGS
  /* Pseudos can't go in stack regs if they are live at the beginning of
     a block that is reached by an abnormal edge.  */
  for (d = WEBS(INITIAL); d; d = d->next)
    {
      struct web *web = DLIST_WEB (d);
      int j;
      if (web->live_over_abnormal)
	for (j = FIRST_STACK_REG; j <= LAST_STACK_REG; j++)
	  record_conflict (web, hardreg2web[j]);
    }
#endif
}

/* Remember that a web was spilled, and change some characteristics
   accordingly.  */

static void
remember_web_was_spilled (web)
     struct web *web;
{
  int i;
  unsigned int found_size = 0;
  int adjust;
  web->spill_temp = 1;

  /* From now on don't use reg_pref/alt_class (regno) anymore for
     this web, but instead  usable_regs.  We can't use spill_temp for
     this, as it might get reset later, when we are coalesced to a
     non-spill-temp.  In that case we still want to use usable_regs.  */
  web->use_my_regs = 1;

  /* We don't constrain spill temporaries in any way for now.
     It's wrong sometimes to have the same constraints or
     preferences as the original pseudo, esp. if they were very narrow.
     (E.g. there once was a reg wanting class AREG (only one register)
     without alternative class.  As long, as also the spill-temps for
     this pseudo had the same constraints it was spilled over and over.
     Ideally we want some constraints also on spill-temps: Because they are
     not only loaded/stored, but also worked with, any constraints from insn
     alternatives needs applying.  Currently this is dealt with by reload, as
     many other things, but at some time we want to integrate that
     functionality into the allocator.  */
  if (web->regno >= max_normal_pseudo)
    {
      COPY_HARD_REG_SET (web->usable_regs,
			reg_class_contents[reg_preferred_class (web->regno)]);
      IOR_HARD_REG_SET (web->usable_regs,
			reg_class_contents[reg_alternate_class (web->regno)]);
    }
  else
    COPY_HARD_REG_SET (web->usable_regs,
		       reg_class_contents[(int) GENERAL_REGS]);
  AND_COMPL_HARD_REG_SET (web->usable_regs, never_use_colors);
  prune_hardregs_for_mode (&web->usable_regs, PSEUDO_REGNO_MODE (web->regno));
#ifdef CLASS_CANNOT_CHANGE_MODE
  if (web->mode_changed)
    AND_COMPL_HARD_REG_SET (web->usable_regs, reg_class_contents[
			      (int) CLASS_CANNOT_CHANGE_MODE]);
#endif
  web->num_freedom = hard_regs_count (web->usable_regs);
  if (!web->num_freedom)
    abort();
  COPY_HARD_REG_SET (web->orig_usable_regs, web->usable_regs);
  /* Now look for a class, which is subset of our constraints, to
     setup add_hardregs, and regclass for debug output.  */
  web->regclass = NO_REGS;
  for (i = (int) ALL_REGS - 1; i > 0; i--)
    {
      unsigned int size;
      HARD_REG_SET test;
      COPY_HARD_REG_SET (test, reg_class_contents[i]);
      AND_COMPL_HARD_REG_SET (test, never_use_colors);
      GO_IF_HARD_REG_SUBSET (test, web->usable_regs, found);
      continue;
    found:
      /* Measure the actual number of bits which really are overlapping
	 the target regset, not just the reg_class_size.  */
      size = hard_regs_count (test);
      if (found_size < size)
	{
          web->regclass = (enum reg_class) i;
	  found_size = size;
	}
    }

  adjust = 0 * web->add_hardregs;
  web->add_hardregs =
    CLASS_MAX_NREGS (web->regclass, PSEUDO_REGNO_MODE (web->regno)) - 1;
  web->num_freedom -= web->add_hardregs;
  if (!web->num_freedom)
    abort();
  adjust -= 0 * web->add_hardregs;
  web->num_conflicts -= adjust;
}

/* Look at each web, if it is used as spill web.  Or better said,
   if it will be spillable in this pass.  */

static void
detect_spill_temps ()
{
  struct dlist *d;
  bitmap already = BITMAP_XMALLOC ();

  /* Detect webs used for spill temporaries.  */
  for (d = WEBS(INITIAL); d; d = d->next)
    {
      struct web *web = DLIST_WEB (d);

      /* Below only the detection of spill temporaries.  We never spill
         precolored webs, so those can't be spill temporaries.  The code above
         (remember_web_was_spilled) can't currently cope with hardregs
         anyway.  */
      if (web->regno < FIRST_PSEUDO_REGISTER)
	continue;
      /* Uninitialized webs can't be spill-temporaries.  */
      if (web->num_defs == 0)
	continue;

      /* A web with only defs and no uses can't be spilled.  Nevertheless
	 it must get a color, as it takes away an register from all webs
	 live at these defs.  So we make it a short web.  */
      if (web->num_uses == 0)
	web->spill_temp = 3;
      /* A web which was spilled last time, but for which no insns were
         emitted (can happen with IR spilling ignoring sometimes
	 all deaths).  */
      else if (web->changed)
	web->spill_temp = 1;
      /* A spill temporary has one def, one or more uses, all uses
	 are in one insn, and either the def or use insn was inserted
	 by the allocator.  */
      /* XXX not correct currently.  There might also be spill temps
	 involving more than one def.  Usually that's an additional
	 clobber in the using instruction.  We might also constrain
	 ourself to that, instead of like currently marking all
	 webs involving any spill insns at all.  */
      else
	{
	  unsigned int i;
	  int spill_involved = 0;
	  for (i = 0; i < web->num_uses && !spill_involved; i++)
	    if (DF_REF_INSN_UID (web->uses[i]) >= orig_max_uid)
	      spill_involved = 1;
	  for (i = 0; i < web->num_defs && !spill_involved; i++)
	    if (DF_REF_INSN_UID (web->defs[i]) >= orig_max_uid)
	      spill_involved = 1;

	  if (spill_involved/* && ra_pass > 2*/)
	    {
	      int num_deaths = web->span_deaths;
	      /* Mark webs involving at least one spill insn as
		 spill temps.  */
	      remember_web_was_spilled (web);
	      /* Search for insns which define and use the web in question
		 at the same time, i.e. look for rmw insns.  If these insns
		 are also deaths of other webs they might have been counted
		 as such into web->span_deaths.  But because of the rmw nature
		 of this insn it is no point where a load/reload could be
		 placed successfully (it would still conflict with the
		 dead web), so reduce the number of spanned deaths by those
		 insns.  Note that sometimes such deaths are _not_ counted,
	         so negative values can result.  */
	      bitmap_zero (already);
	      for (i = 0; i < web->num_defs; i++)
		{
		  rtx insn = web->defs[i]->insn;
		  if (TEST_BIT (insns_with_deaths, INSN_UID (insn))
		      && !bitmap_bit_p (already, INSN_UID (insn)))
		    {
		      unsigned int j;
		      bitmap_set_bit (already, INSN_UID (insn));
		      /* Only decrement it once for each insn.  */
		      for (j = 0; j < web->num_uses; j++)
			if (web->uses[j]->insn == insn)
			  {
			    num_deaths--;
			    break;
			  }
		    }
		}
	      /* But mark them specially if they could possibly be spilled,
		 either because they cross some deaths (without the above
		 mentioned ones) or calls.  */
	      if (web->crosses_call || num_deaths > 0)
		web->spill_temp = 1 * 2;
	    }
	  /* A web spanning no deaths can't be spilled either.  No loads
	     would be created for it, ergo no defs.  So the insns wouldn't
	     change making the graph not easier to color.  Make this also
	     a short web.  Don't do this if it crosses calls, as these are
	     also points of reloads.  */
	  else if (web->span_deaths == 0 && !web->crosses_call)
	    web->spill_temp = 3;
	}
      web->orig_spill_temp = web->spill_temp;
    }
  BITMAP_XFREE (already);
}

/* Returns nonzero if the rtx MEM refers somehow to a stack location.  */

int
memref_is_stack_slot (mem)
     rtx mem;
{
  rtx ad = XEXP (mem, 0);
  rtx x;
  if (GET_CODE (ad) != PLUS || GET_CODE (XEXP (ad, 1)) != CONST_INT)
    return 0;
  x = XEXP (ad, 0);
  if (x == frame_pointer_rtx || x == hard_frame_pointer_rtx
      || (x == arg_pointer_rtx && fixed_regs[ARG_POINTER_REGNUM])
      || x == stack_pointer_rtx)
    return 1;
  return 0;
}

/* Returns nonzero, if rtx X somewhere contains any pseudo register.  */

static int
contains_pseudo (x)
     rtx x;
{
  const char *fmt;
  int i;
  if (GET_CODE (x) == SUBREG)
    x = SUBREG_REG (x);
  if (GET_CODE (x) == REG)
    {
      if (REGNO (x) >= FIRST_PSEUDO_REGISTER)
        return 1;
      else
	return 0;
    }

  fmt = GET_RTX_FORMAT (GET_CODE (x));
  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    if (fmt[i] == 'e')
      {
	if (contains_pseudo (XEXP (x, i)))
	  return 1;
      }
    else if (fmt[i] == 'E')
      {
	int j;
	for (j = 0; j < XVECLEN (x, i); j++)
	  if (contains_pseudo (XVECEXP (x, i, j)))
	    return 1;
      }
  return 0;
}

/* Returns nonzero, if we are able to rematerialize something with
   value X.  If it's not a general operand, we test if we can produce
   a valid insn which set a pseudo to that value, and that insn doesn't
   clobber anything.  */

static GTY(()) rtx remat_test_insn;
static int
want_to_remat (x)
     rtx x;
{
  int num_clobbers = 0;
  int icode;

  /* If this is a valid operand, we are OK.  If it's VOIDmode, we aren't.  */
  if (general_operand (x, GET_MODE (x)))
    return 1;

  /* Otherwise, check if we can make a valid insn from it.  First initialize
     our test insn if we haven't already.  */
  if (remat_test_insn == 0)
    {
      remat_test_insn
	= make_insn_raw (gen_rtx_SET (VOIDmode,
				      gen_rtx_REG (word_mode,
						   FIRST_PSEUDO_REGISTER * 2),
				      const0_rtx));
      NEXT_INSN (remat_test_insn) = PREV_INSN (remat_test_insn) = 0;
    }

  /* Now make an insn like the one we would make when rematerializing
     the value X and see if valid.  */
  PUT_MODE (SET_DEST (PATTERN (remat_test_insn)), GET_MODE (x));
  SET_SRC (PATTERN (remat_test_insn)) = x;
  /* XXX For now we don't allow any clobbers to be added, not just no
     hardreg clobbers.  */
  return ((icode = recog (PATTERN (remat_test_insn), remat_test_insn,
			  &num_clobbers)) >= 0
	  && (num_clobbers == 0
	      /*|| ! added_clobbers_hard_reg_p (icode)*/));
}

/* Look at all webs, if they perhaps are rematerializable.
   They are, if all their defs are simple sets to the same value,
   and that value is simple enough, and want_to_remat() holds for it.  */

static void
detect_remat_webs ()
{
  struct dlist *d;
  for (d = WEBS(INITIAL); d; d = d->next)
    {
      struct web *web = DLIST_WEB (d);
      unsigned int i;
      rtx pat = NULL_RTX;
      /* Hardregs and useless webs aren't spilled -> no remat necessary.
	 Defless webs obviously also can't be rematerialized.  */
      if (web->regno < FIRST_PSEUDO_REGISTER || !web->num_defs
	  || !web->num_uses)
	continue;
      for (i = 0; i < web->num_defs; i++)
	{
	  rtx insn;
	  rtx set = single_set (insn = DF_REF_INSN (web->defs[i]));
	  rtx src;
	  if (!set)
	    break;
	  src = SET_SRC (set);
	  /* When only subregs of the web are set it isn't easily
	     rematerializable.  */
	  if (!rtx_equal_p (SET_DEST (set), web->orig_x))
	    break;
	  /* If we already have a pattern it must be equal to the current.  */
	  if (pat && !rtx_equal_p (pat, src))
	    break;
	  /* Don't do the expensive checks multiple times.  */
	  if (pat)
	    continue;
	  /* For now we allow only constant sources.  */
	  if ((CONSTANT_P (src)
	       /* If the whole thing is stable already, it is a source for
		  remat, no matter how complicated (probably all needed
		  resources for it are live everywhere, and don't take
		  additional register resources).  */
	       /* XXX Currently we can't use patterns which contain
		  pseudos, _even_ if they are stable.  The code simply isn't
		  prepared for that.  All those operands can't be spilled (or
		  the dependent remat webs are not remat anymore), so they
		  would be oldwebs in the next iteration.  But currently
		  oldwebs can't have their references changed.  The
		  incremental machinery barfs on that.  */
	       || (!rtx_unstable_p (src) && !contains_pseudo (src))
	       /* Additionally also memrefs to stack-slots are usefull, when
		  we created them ourself.  They might not have set their
		  unchanging flag set, but nevertheless they are stable across
		  the livetime in question.  */
	       || (GET_CODE (src) == MEM
		   && INSN_UID (insn) >= orig_max_uid
		   && memref_is_stack_slot (src)))
	      /* And we must be able to construct an insn without
		 side-effects to actually load that value into a reg.  */
	      && want_to_remat (src))
	    pat = src;
	  else
	    break;
	}
      if (pat && i == web->num_defs)
	web->pattern = pat;
    }
}

/* Determine the spill costs of all webs.  */

static void
determine_web_costs ()
{
  struct dlist *d;
  for (d = WEBS(INITIAL); d; d = d->next)
    {
      unsigned int i, num_loads;
      int load_cost, store_cost;
      unsigned HOST_WIDE_INT w;
      struct web *web = DLIST_WEB (d);
      if (web->type == PRECOLORED)
	continue;
      /* Get costs for one load/store.  Note that we offset them by 1,
	 because some patterns have a zero rtx_cost(), but we of course
	 still need the actual load/store insns.  With zero all those
	 webs would be the same, no matter how often and where
	 they are used.  */
      if (web->pattern)
	{
	  /* This web is rematerializable.  Beware, we set store_cost to
	     zero optimistically assuming, that we indeed don't emit any
	     stores in the spill-code addition.  This might be wrong if
	     at the point of the load not all needed resources are
	     available, in which case we emit a stack-based load, for
	     which we in turn need the according stores.  */
	  load_cost = 1 + rtx_cost (web->pattern, 0);
	  store_cost = 0;
	}
      else
	{
	  load_cost = 1 + MEMORY_MOVE_COST (GET_MODE (web->orig_x),
					    web->regclass, 1);
	  store_cost = 1 + MEMORY_MOVE_COST (GET_MODE (web->orig_x),
					     web->regclass, 0);
	}
      /* We create only loads at deaths, whose number is in span_deaths.  */
      num_loads = MIN (web->span_deaths, web->num_uses);
      for (w = 0, i = 0; i < web->num_uses; i++)
	w += DF_REF_BB (web->uses[i])->frequency + 1;
      if (num_loads < web->num_uses)
	w = (w * num_loads + web->num_uses - 1) / web->num_uses;
      web->spill_cost = w * load_cost;
      if (store_cost)
	{
	  for (w = 0, i = 0; i < web->num_defs; i++)
	    w += DF_REF_BB (web->defs[i])->frequency + 1;
	  web->spill_cost += w * store_cost;
	}
      web->orig_spill_cost = web->spill_cost;
    }
}

/* Detect webs which are set in a conditional jump insn (possibly a
   decrement-and-branch type of insn), and mark them not to be
   spillable.  The stores for them would need to be placed on edges,
   which destroys the CFG.  (Somewhen we want to deal with that XXX)  */

static void
detect_webs_set_in_cond_jump ()
{
  basic_block bb;
  FOR_EACH_BB (bb)
    if (GET_CODE (bb->end) == JUMP_INSN)
      {
	struct df_link *link;
	for (link = DF_INSN_DEFS (df, bb->end); link; link = link->next)
	  if (link->ref && DF_REF_REGNO (link->ref) >= FIRST_PSEUDO_REGISTER)
	    {
	      struct web *web = def2web[DF_REF_ID (link->ref)];
	      web->orig_spill_temp = web->spill_temp = 3;
	    }
      }
}

/* Second top-level function of this file.
   Converts the connected web parts to full webs.  This means, it allocates
   all webs, and initializes all fields, including detecting spill
   temporaries.  It does not distribute moves to their corresponding webs,
   though.  */

static void
make_webs (df)
     struct df *df;
{
  /* First build all the webs itself.  They are not related with
     others yet.  */
  parts_to_webs (df);
  /* Now detect spill temporaries to initialize their usable_regs set.  */
  detect_spill_temps ();
  detect_webs_set_in_cond_jump ();
  /* And finally relate them to each other, meaning to record all possible
     conflicts between webs (see the comment there).  */
  conflicts_between_webs (df);
  detect_remat_webs ();
  determine_web_costs ();
}

/* Distribute moves to the corresponding webs.  */

static void
moves_to_webs (df)
     struct df *df;
{
  struct df_link *link;
  struct move_list *ml;

  /* Distribute all moves to their corresponding webs, making sure,
     each move is in a web maximally one time (happens on some strange
     insns).  */
  for (ml = wl_moves; ml; ml = ml->next)
    {
      struct move *m = ml->move;
      struct web *web;
      struct move_list *newml;
      if (!m)
	continue;
      m->type = WORKLIST;
      m->dlink = NULL;
      /* Multiple defs/uses can happen in moves involving hard-regs in
	 a wider mode.  For those df.* creates use/def references for each
	 real hard-reg involved.  For coalescing we are interested in
	 the smallest numbered hard-reg.  */
      for (link = DF_INSN_DEFS (df, m->insn); link; link = link->next)
        if (link->ref)
	  {
	    web = def2web[DF_REF_ID (link->ref)];
	    web = find_web_for_subweb (web);
	    if (!m->target_web || web->regno < m->target_web->regno)
	      m->target_web = web;
	  }
      for (link = DF_INSN_USES (df, m->insn); link; link = link->next)
        if (link->ref)
	  {
	    web = use2web[DF_REF_ID (link->ref)];
	    web = find_web_for_subweb (web);
	    if (!m->source_web || web->regno < m->source_web->regno)
	      m->source_web = web;
	  }
      if (m->source_web && m->target_web
	  /* If the usable_regs don't intersect we can't coalesce the two
	     webs anyway, as this is no simple copy insn (it might even
	     need an intermediate stack temp to execute this "copy" insn).  */
	  && hard_regs_intersect_p (&m->source_web->usable_regs,
				    &m->target_web->usable_regs))
	{
	  if (!flag_ra_optimistic_coalescing)
	    {
	      struct move_list *test = m->source_web->moves;
	      for (; test && test->move != m; test = test->next);
	      if (! test)
		{
		  newml = (struct move_list*)
		    ra_alloc (sizeof (struct move_list));
		  newml->move = m;
		  newml->next = m->source_web->moves;
		  m->source_web->moves = newml;
		}
	      test = m->target_web->moves;
	      for (; test && test->move != m; test = test->next);
	      if (! test)
		{
		  newml = (struct move_list*)
		    ra_alloc (sizeof (struct move_list));
		  newml->move = m;
		  newml->next = m->target_web->moves;
		  m->target_web->moves = newml;
		}
	    }
	}
      else
	/* Delete this move.  */
	ml->move = NULL;
    }
}

/* Handle tricky asm insns.
   Supposed to create conflicts to hardregs which aren't allowed in
   the constraints.  Doesn't actually do that, as it might confuse
   and constrain the allocator too much.  */

static void
handle_asm_insn (df, insn)
     struct df *df;
     rtx insn;
{
  const char *constraints[MAX_RECOG_OPERANDS];
  enum machine_mode operand_mode[MAX_RECOG_OPERANDS];
  int i, noperands, in_output;
  HARD_REG_SET clobbered, allowed, conflict;
  rtx pat;
  if (! INSN_P (insn)
      || (noperands = asm_noperands (PATTERN (insn))) < 0)
    return;
  pat = PATTERN (insn);
  CLEAR_HARD_REG_SET (clobbered);

  if (GET_CODE (pat) == PARALLEL)
    for (i = 0; i < XVECLEN (pat, 0); i++)
      {
	rtx t = XVECEXP (pat, 0, i);
	if (GET_CODE (t) == CLOBBER && GET_CODE (XEXP (t, 0)) == REG
	    && REGNO (XEXP (t, 0)) < FIRST_PSEUDO_REGISTER)
	  SET_HARD_REG_BIT (clobbered, REGNO (XEXP (t, 0)));
      }

  decode_asm_operands (pat, recog_data.operand, recog_data.operand_loc,
		       constraints, operand_mode);
  in_output = 1;
  for (i = 0; i < noperands; i++)
    {
      const char *p = constraints[i];
      int cls = (int) NO_REGS;
      struct df_link *link;
      rtx reg;
      struct web *web;
      int nothing_allowed = 1;
      reg = recog_data.operand[i];

      /* Look, if the constraints apply to a pseudo reg, and not to
	 e.g. a mem.  */
      while (GET_CODE (reg) == SUBREG
	     || GET_CODE (reg) == ZERO_EXTRACT
	     || GET_CODE (reg) == SIGN_EXTRACT
	     || GET_CODE (reg) == STRICT_LOW_PART)
	reg = XEXP (reg, 0);
      if (GET_CODE (reg) != REG || REGNO (reg) < FIRST_PSEUDO_REGISTER)
	continue;

      /* Search the web corresponding to this operand.  We depend on
	 that decode_asm_operands() places the output operands
	 before the input operands.  */
      while (1)
	{
	  if (in_output)
	    link = df->insns[INSN_UID (insn)].defs;
	  else
	    link = df->insns[INSN_UID (insn)].uses;
	  while (link && link->ref && DF_REF_REAL_REG (link->ref) != reg)
	    link = link->next;
	  if (!link || !link->ref)
	    {
	      if (in_output)
	        in_output = 0;
	      else
	        abort ();
	    }
	  else
	    break;
	}
      if (in_output)
	web = def2web[DF_REF_ID (link->ref)];
      else
	web = use2web[DF_REF_ID (link->ref)];
      reg = DF_REF_REG (link->ref);

      /* Find the constraints, noting the allowed hardregs in allowed.  */
      CLEAR_HARD_REG_SET (allowed);
      while (1)
	{
	  char c = *p++;

	  if (c == '\0' || c == ',' || c == '#')
	    {
	      /* End of one alternative - mark the regs in the current
	       class, and reset the class.
	       */
	      IOR_HARD_REG_SET (allowed, reg_class_contents[cls]);
	      if (cls != NO_REGS)
		nothing_allowed = 0;
	      cls = NO_REGS;
	      if (c == '#')
		do {
		    c = *p++;
		} while (c != '\0' && c != ',');
	      if (c == '\0')
	        break;
	      continue;
	    }

	  switch (c)
	    {
	      case '=': case '+': case '*': case '%': case '?': case '!':
	      case '0': case '1': case '2': case '3': case '4': case 'm':
	      case '<': case '>': case 'V': case 'o': case '&': case 'E':
	      case 'F': case 's': case 'i': case 'n': case 'X': case 'I':
	      case 'J': case 'K': case 'L': case 'M': case 'N': case 'O':
	      case 'P':
		break;

	      case 'p':
		cls = (int) reg_class_subunion[cls][(int) BASE_REG_CLASS];
		nothing_allowed = 0;
	        break;

	      case 'g':
	      case 'r':
		cls = (int) reg_class_subunion[cls][(int) GENERAL_REGS];
		nothing_allowed = 0;
		break;

	      default:
		cls =
		  (int) reg_class_subunion[cls][(int)
						REG_CLASS_FROM_LETTER (c)];
	    }
	}

      /* Now make conflicts between this web, and all hardregs, which
	 are not allowed by the constraints.  */
      if (nothing_allowed)
	{
	  /* If we had no real constraints nothing was explicitely
	     allowed, so we allow the whole class (i.e. we make no
	     additional conflicts).  */
	  CLEAR_HARD_REG_SET (conflict);
	}
      else
	{
	  COPY_HARD_REG_SET (conflict, usable_regs
			     [reg_preferred_class (web->regno)]);
	  IOR_HARD_REG_SET (conflict, usable_regs
			    [reg_alternate_class (web->regno)]);
	  AND_COMPL_HARD_REG_SET (conflict, allowed);
	  /* We can't yet establish these conflicts.  Reload must go first
	     (or better said, we must implement some functionality of reload).
	     E.g. if some operands must match, and they need the same color
	     we don't see yet, that they do not conflict (because they match).
	     For us it looks like two normal references with different DEFs,
	     so they conflict, and as they both need the same color, the
	     graph becomes uncolorable.  */
#if 0
	  for (c = 0; c < FIRST_PSEUDO_REGISTER; c++)
	    if (TEST_HARD_REG_BIT (conflict, c))
	      record_conflict (web, hardreg2web[c]);
#endif
	}
      if (rtl_dump_file)
	{
	  int c;
	  ra_debug_msg (DUMP_ASM, " ASM constrain Web %d conflicts with:", web->id);
	  for (c = 0; c < FIRST_PSEUDO_REGISTER; c++)
	    if (TEST_HARD_REG_BIT (conflict, c))
	      ra_debug_msg (DUMP_ASM, " %d", c);
	  ra_debug_msg (DUMP_ASM, "\n");
	}
    }
}

/* The real toplevel function in this file.
   Build (or rebuilds) the complete interference graph with webs
   and conflicts.  */

void
build_i_graph (df)
     struct df *df;
{
  rtx insn;

  init_web_parts (df);

  sbitmap_zero (move_handled);
  wl_moves = NULL;

  build_web_parts_and_conflicts (df);

  /* For read-modify-write instructions we may have created two webs.
     Reconnect them here.  (s.a.)  */
  connect_rmw_web_parts (df);

  /* The webs are conceptually complete now, but still scattered around as
     connected web parts.  Collect all information and build the webs
     including all conflicts between webs (instead web parts).  */
  make_webs (df);
  moves_to_webs (df);

  /* Look for additional constraints given by asms.  */
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    handle_asm_insn (df, insn);
}

/* Allocates or reallocates most memory for the interference graph and
   assiciated structures.  If it reallocates memory (meaning, this is not
   the first pass), this also changes some structures to reflect the
   additional entries in various array, and the higher number of
   defs and uses.  */

void
ra_build_realloc (df)
     struct df *df;
{
  struct web_part *last_web_parts = web_parts;
  struct web **last_def2web = def2web;
  struct web **last_use2web = use2web;
  sbitmap last_live_over_abnormal = live_over_abnormal;
  unsigned int i;
  struct dlist *d;
  move_handled = sbitmap_alloc (get_max_uid () );
  web_parts = (struct web_part *) xcalloc (df->def_id + df->use_id,
					   sizeof web_parts[0]);
  def2web = (struct web **) xcalloc (df->def_id + df->use_id,
				     sizeof def2web[0]);
  use2web = &def2web[df->def_id];
  live_over_abnormal = sbitmap_alloc (df->use_id);
  sbitmap_zero (live_over_abnormal);

  /* First go through all old defs and uses.  */
  for (i = 0; i < last_def_id + last_use_id; i++)
    {
      /* And relocate them to the new array.  This is made ugly by the
         fact, that defs and uses are placed consecutive into one array.  */
      struct web_part *dest = &web_parts[i < last_def_id
					 ? i : (df->def_id + i - last_def_id)];
      struct web_part *up;
      *dest = last_web_parts[i];
      up = dest->uplink;
      dest->uplink = NULL;

      /* Also relocate the uplink to point into the new array.  */
      if (up && up->ref)
	{
	  unsigned int id = DF_REF_ID (up->ref);
	  if (up < &last_web_parts[last_def_id])
	    {
	      if (df->defs[id])
	        dest->uplink = &web_parts[DF_REF_ID (up->ref)];
	    }
	  else if (df->uses[id])
	    dest->uplink = &web_parts[df->def_id + DF_REF_ID (up->ref)];
	}
    }

  /* Also set up the def2web and use2web arrays, from the last pass.i
     Remember also the state of live_over_abnormal.  */
  for (i = 0; i < last_def_id; i++)
    {
      struct web *web = last_def2web[i];
      if (web)
	{
	  web = find_web_for_subweb (web);
	  if (web->type != FREE && web->type != PRECOLORED)
	    def2web[i] = last_def2web[i];
	}
    }
  for (i = 0; i < last_use_id; i++)
    {
      struct web *web = last_use2web[i];
      if (web)
	{
	  web = find_web_for_subweb (web);
	  if (web->type != FREE && web->type != PRECOLORED)
	    use2web[i] = last_use2web[i];
	}
      if (TEST_BIT (last_live_over_abnormal, i))
	SET_BIT (live_over_abnormal, i);
    }

  /* We don't have any subwebs for now.  Somewhen we might want to
     remember them too, instead of recreating all of them every time.
     The problem is, that which subwebs we need, depends also on what
     other webs and subwebs exist, and which conflicts are there.
     OTOH it should be no problem, if we had some more subwebs than strictly
     needed.  Later.  */
  for (d = WEBS(FREE); d; d = d->next)
    {
      struct web *web = DLIST_WEB (d);
      struct web *wnext;
      for (web = web->subreg_next; web; web = wnext)
	{
	  wnext = web->subreg_next;
	  free (web);
	}
      DLIST_WEB (d)->subreg_next = NULL;
    }

  /* The uses we anyway are going to check, are not yet live over an abnormal
     edge.  In fact, they might actually not anymore, due to added
     loads.  */
  if (last_check_uses)
    sbitmap_difference (live_over_abnormal, live_over_abnormal,
		        last_check_uses);

  if (last_def_id || last_use_id)
    {
      sbitmap_free (last_live_over_abnormal);
      free (last_web_parts);
      free (last_def2web);
    }
  if (!last_max_uid)
    {
      /* Setup copy cache, for copy_insn_p ().  */
      copy_cache = (struct copy_p_cache *)
	xcalloc (get_max_uid (), sizeof (copy_cache[0]));
      init_bb_info ();
    }
  else
    {
      copy_cache = (struct copy_p_cache *)
	xrealloc (copy_cache, get_max_uid () * sizeof (copy_cache[0]));
      memset (&copy_cache[last_max_uid], 0,
	      (get_max_uid () - last_max_uid) * sizeof (copy_cache[0]));
    }
}

/* Free up/clear some memory, only needed for one pass.  */

void
ra_build_free ()
{
  struct dlist *d;
  unsigned int i;

  /* Clear the moves associated with a web (we also need to look into
     subwebs here).  */
  for (i = 0; i < num_webs; i++)
    {
      struct web *web = ID2WEB (i);
      if (!web)
	abort ();
      if (i >= num_webs - num_subwebs
	  && (web->conflict_list || web->orig_conflict_list))
	abort ();
      web->moves = NULL;
    }
  /* All webs in the free list have no defs or uses anymore.  */
  for (d = WEBS(FREE); d; d = d->next)
    {
      struct web *web = DLIST_WEB (d);
      if (web->defs)
	free (web->defs);
      web->defs = NULL;
      if (web->uses)
	free (web->uses);
      web->uses = NULL;
      /* We can't free the subwebs here, as they are referenced from
	 def2web[], and possibly needed in the next ra_build_realloc().
	 We free them there (or in free_all_mem()).  */
    }

  /* Free all conflict bitmaps from web parts.  Note that we clear
     _all_ these conflicts, and don't rebuild them next time for uses
     which aren't rechecked.  This mean, that those conflict bitmaps
     only contain the incremental information.  The cumulative one
     is still contained in the edges of the I-graph, i.e. in
     conflict_list (or orig_conflict_list) of the webs.  */
  for (i = 0; i < df->def_id + df->use_id; i++)
    {
      struct tagged_conflict *cl;
      for (cl = web_parts[i].sub_conflicts; cl; cl = cl->next)
	{
	  if (cl->conflicts)
	    BITMAP_XFREE (cl->conflicts);
	}
      web_parts[i].sub_conflicts = NULL;
    }

  wl_moves = NULL;

  free (id2web);
  free (move_handled);
  sbitmap_free (sup_igraph);
  sbitmap_free (igraph);
}

/* Free all memory for the interference graph structures.  */

void
ra_build_free_all (df)
     struct df *df;
{
  unsigned int i;

  free_bb_info ();
  free (copy_cache);
  copy_cache = NULL;
  for (i = 0; i < df->def_id + df->use_id; i++)
    {
      struct tagged_conflict *cl;
      for (cl = web_parts[i].sub_conflicts; cl; cl = cl->next)
	{
	  if (cl->conflicts)
	    BITMAP_XFREE (cl->conflicts);
	}
      web_parts[i].sub_conflicts = NULL;
    }
  sbitmap_free (live_over_abnormal);
  free (web_parts);
  web_parts = NULL;
  if (last_check_uses)
    sbitmap_free (last_check_uses);
  last_check_uses = NULL;
  free (def2web);
  use2web = NULL;
  def2web = NULL;
}

#include "gt-ra-build.h"

/*
vim:cinoptions={.5s,g0,p5,t0,(0,^-0.5s,n-0.5s:tw=78:cindent:sw=4:
*/
