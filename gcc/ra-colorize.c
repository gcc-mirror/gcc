/* Graph coloring register allocator
   Copyright (C) 2001, 2002 Free Software Foundation, Inc.
   Contributed by Michael Matz <matz@suse.de>
   and Daniel Berlin <dan@cgsoftware.com>.

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
#include "function.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "df.h"
#include "output.h"
#include "ra.h"

/* This file is part of the graph coloring register allocator.
   It contains the graph colorizer.  Given an interference graph
   as set up in ra-build.c the toplevel function in this file
   (ra_colorize_graph) colorizes the graph, leaving a list
   of colored, coalesced and spilled nodes.

   The algorithm used is a merge of George & Appels iterative coalescing
   and optimistic coalescing, switchable at runtime.  The current default
   is "optimistic coalescing +", which is based on the normal Briggs/Cooper
   framework.  We can also use biased coloring.  Most of the structure
   here follows the different papers.

   Additionally there is a custom step to locally improve the overall
   spill cost of the colored graph (recolor_spills).  */

static void push_list PARAMS ((struct dlist *, struct dlist **));
static void push_list_end PARAMS ((struct dlist *, struct dlist **));
static void free_dlist PARAMS ((struct dlist **));
static void put_web_at_end PARAMS ((struct web *, enum node_type));
static void put_move PARAMS ((struct move *, enum move_type));
static void build_worklists PARAMS ((struct df *));
static void enable_move PARAMS ((struct web *));
static void decrement_degree PARAMS ((struct web *, int));
static void simplify PARAMS ((void));
static void remove_move_1 PARAMS ((struct web *, struct move *));
static void remove_move PARAMS ((struct web *, struct move *));
static void add_worklist PARAMS ((struct web *));
static int ok PARAMS ((struct web *, struct web *));
static int conservative PARAMS ((struct web *, struct web *));
static inline unsigned int simplify_p PARAMS ((enum node_type));
static void combine PARAMS ((struct web *, struct web *));
static void coalesce PARAMS ((void));
static void freeze_moves PARAMS ((struct web *));
static void freeze PARAMS ((void));
static void select_spill PARAMS ((void));
static int color_usable_p PARAMS ((int, HARD_REG_SET, HARD_REG_SET,
				   enum machine_mode));
int get_free_reg PARAMS ((HARD_REG_SET, HARD_REG_SET, enum machine_mode));
static int get_biased_reg PARAMS ((HARD_REG_SET, HARD_REG_SET, HARD_REG_SET,
				   HARD_REG_SET, enum machine_mode));
static int count_long_blocks PARAMS ((HARD_REG_SET, int));
static char * hardregset_to_string PARAMS ((HARD_REG_SET));
static void calculate_dont_begin PARAMS ((struct web *, HARD_REG_SET *));
static void colorize_one_web PARAMS ((struct web *, int));
static void assign_colors PARAMS ((void));
static void try_recolor_web PARAMS ((struct web *));
static void insert_coalesced_conflicts PARAMS ((void));
static int comp_webs_maxcost PARAMS ((const void *, const void *));
static void recolor_spills PARAMS ((void));
static void check_colors PARAMS ((void));
static void restore_conflicts_from_coalesce PARAMS ((struct web *));
static void break_coalesced_spills PARAMS ((void));
static void unalias_web PARAMS ((struct web *));
static void break_aliases_to_web PARAMS ((struct web *));
static void break_precolored_alias PARAMS ((struct web *));
static void init_web_pairs PARAMS ((void));
static void add_web_pair_cost PARAMS ((struct web *, struct web *,
			               unsigned HOST_WIDE_INT, unsigned int));
static int comp_web_pairs PARAMS ((const void *, const void *));
static void sort_and_combine_web_pairs PARAMS ((int));
static void aggressive_coalesce PARAMS ((void));
static void extended_coalesce_2 PARAMS ((void));
static void check_uncoalesced_moves PARAMS ((void));

static struct dlist *mv_worklist, *mv_coalesced, *mv_constrained;
static struct dlist *mv_frozen, *mv_active;

/* Push a node onto the front of the list.  */

static void
push_list (x, list)
     struct dlist *x;
     struct dlist **list;
{
  if (x->next || x->prev)
    abort ();
  x->next = *list;
  if (*list)
    (*list)->prev = x;
  *list = x;
}

static void
push_list_end (x, list)
     struct dlist *x;
     struct dlist **list;
{
  if (x->prev || x->next)
    abort ();
  if (!*list)
    {
      *list = x;
      return;
    }
  while ((*list)->next)
    list = &((*list)->next);
  x->prev = *list;
  (*list)->next = x;
}

/* Remove a node from the list.  */

void
remove_list (x, list)
     struct dlist *x;
     struct dlist **list;
{
  struct dlist *y = x->prev;
  if (y)
    y->next = x->next;
  else
    *list = x->next;
  y = x->next;
  if (y)
    y->prev = x->prev;
  x->next = x->prev = NULL;
}

/* Pop the front of the list.  */

struct dlist *
pop_list (list)
     struct dlist **list;
{
  struct dlist *r = *list;
  if (r)
    remove_list (r, list);
  return r;
}

/* Free the given double linked list.  */

static void
free_dlist (list)
     struct dlist **list;
{
  *list = NULL;
}

/* The web WEB should get the given new TYPE.  Put it onto the
   appropriate list.
   Inline, because it's called with constant TYPE every time.  */

inline void
put_web (web, type)
     struct web *web;
     enum node_type type;
{
  switch (type)
    {
      case INITIAL:
      case FREE:
      case FREEZE:
      case SPILL:
      case SPILLED:
      case COALESCED:
      case COLORED:
      case SELECT:
	push_list (web->dlink, &WEBS(type));
	break;
      case PRECOLORED:
	push_list (web->dlink, &WEBS(INITIAL));
	break;
      case SIMPLIFY:
	if (web->spill_temp)
	  push_list (web->dlink, &WEBS(type = SIMPLIFY_SPILL));
	else if (web->add_hardregs)
	  push_list (web->dlink, &WEBS(type = SIMPLIFY_FAT));
	else
	  push_list (web->dlink, &WEBS(SIMPLIFY));
	break;
      default:
	abort ();
    }
  web->type = type;
}

/* After we are done with the whole pass of coloring/spilling,
   we reset the lists of webs, in preparation of the next pass.
   The spilled webs become free, colored webs go to the initial list,
   coalesced webs become free or initial, according to what type of web
   they are coalesced to.  */

void
reset_lists ()
{
  struct dlist *d;
  unsigned int i;
  if (WEBS(SIMPLIFY) || WEBS(SIMPLIFY_SPILL) || WEBS(SIMPLIFY_FAT)
      || WEBS(FREEZE) || WEBS(SPILL) || WEBS(SELECT))
    abort ();

  while ((d = pop_list (&WEBS(COALESCED))) != NULL)
    {
      struct web *web = DLIST_WEB (d);
      struct web *aweb = alias (web);
      /* Note, how alias() becomes invalid through the two put_web()'s
	 below.  It might set the type of a web to FREE (from COALESCED),
	 which itself is a target of aliasing (i.e. in the middle of
	 an alias chain).  We can handle this by checking also for
	 type == FREE.  Note nevertheless, that alias() is invalid
	 henceforth.  */
      if (aweb->type == SPILLED || aweb->type == FREE)
	put_web (web, FREE);
      else
	put_web (web, INITIAL);
    }
  while ((d = pop_list (&WEBS(SPILLED))) != NULL)
    put_web (DLIST_WEB (d), FREE);
  while ((d = pop_list (&WEBS(COLORED))) != NULL)
    put_web (DLIST_WEB (d), INITIAL);

  /* All free webs have no conflicts anymore.  */
  for (d = WEBS(FREE); d; d = d->next)
    {
      struct web *web = DLIST_WEB (d);
      BITMAP_XFREE (web->useless_conflicts);
      web->useless_conflicts = NULL;
    }

  /* Sanity check, that we only have free, initial or precolored webs.  */
  for (i = 0; i < num_webs; i++)
    {
      struct web *web = ID2WEB (i);
      if (web->type != INITIAL && web->type != FREE && web->type != PRECOLORED)
	abort ();
    }
  free_dlist (&mv_worklist);
  free_dlist (&mv_coalesced);
  free_dlist (&mv_constrained);
  free_dlist (&mv_frozen);
  free_dlist (&mv_active);
}

/* Similar to put_web(), but add the web to the end of the appropriate
   list.  Additionally TYPE may not be SIMPLIFY.  */

static void
put_web_at_end (web, type)
     struct web *web;
     enum node_type type;
{
  if (type == PRECOLORED)
    type = INITIAL;
  else if (type == SIMPLIFY)
    abort ();
  push_list_end (web->dlink, &WEBS(type));
  web->type = type;
}

/* Unlink WEB from the list it's currently on (which corresponds to
   its current type).  */

void
remove_web_from_list (web)
     struct web *web;
{
  if (web->type == PRECOLORED)
    remove_list (web->dlink, &WEBS(INITIAL));
  else
    remove_list (web->dlink, &WEBS(web->type));
}

/* Give MOVE the TYPE, and link it into the correct list.  */

static inline void
put_move (move, type)
     struct move *move;
     enum move_type type;
{
  switch (type)
    {
      case WORKLIST:
	push_list (move->dlink, &mv_worklist);
	break;
      case MV_COALESCED:
	push_list (move->dlink, &mv_coalesced);
	break;
      case CONSTRAINED:
	push_list (move->dlink, &mv_constrained);
	break;
      case FROZEN:
	push_list (move->dlink, &mv_frozen);
	break;
      case ACTIVE:
	push_list (move->dlink, &mv_active);
	break;
      default:
	abort ();
    }
  move->type = type;
}

/* Build the worklists we are going to process.  */

static void
build_worklists (df)
     struct df *df ATTRIBUTE_UNUSED;
{
  struct dlist *d, *d_next;
  struct move_list *ml;

  /* If we are not the first pass, put all stackwebs (which are still
     backed by a new pseudo, but conceptually can stand for a stackslot,
     i.e. it doesn't really matter if they get a color or not), on
     the SELECT stack first, those with lowest cost first.  This way
     they will be colored last, so do not contrain the coloring of the
     normal webs.  But still those with the highest count are colored
     before, i.e. get a color more probable.  The use of stackregs is
     a pure optimization, and all would work, if we used real stackslots
     from the begin.  */
  if (ra_pass > 1)
    {
      unsigned int i, num, max_num;
      struct web **order2web;
      max_num = num_webs - num_subwebs;
      order2web = (struct web **) xmalloc (max_num * sizeof (order2web[0]));
      for (i = 0, num = 0; i < max_num; i++)
	if (id2web[i]->regno >= max_normal_pseudo)
	  order2web[num++] = id2web[i];
      if (num)
	{
	  qsort (order2web, num, sizeof (order2web[0]), comp_webs_maxcost);
	  for (i = num - 1;; i--)
	    {
	      struct web *web = order2web[i];
	      struct conflict_link *wl;
	      remove_list (web->dlink, &WEBS(INITIAL));
	      put_web (web, SELECT);
	      for (wl = web->conflict_list; wl; wl = wl->next)
		{
		  struct web *pweb = wl->t;
		  pweb->num_conflicts -= 1 + web->add_hardregs;
		}
	      if (i == 0)
		break;
	    }
	}
      free (order2web);
    }

  /* For all remaining initial webs, classify them.  */
  for (d = WEBS(INITIAL); d; d = d_next)
    {
      struct web *web = DLIST_WEB (d);
      d_next = d->next;
      if (web->type == PRECOLORED)
        continue;

      remove_list (d, &WEBS(INITIAL));
      if (web->num_conflicts >= NUM_REGS (web))
	put_web (web, SPILL);
      else if (web->moves)
	put_web (web, FREEZE);
      else
	put_web (web, SIMPLIFY);
    }

  /* And put all moves on the worklist for iterated coalescing.
     Note, that if iterated coalescing is off, then wl_moves doesn't
     contain any moves.  */
  for (ml = wl_moves; ml; ml = ml->next)
    if (ml->move)
      {
	struct move *m = ml->move;
        d = (struct dlist *) ra_calloc (sizeof (struct dlist));
        DLIST_MOVE (d) = m;
        m->dlink = d;
	put_move (m, WORKLIST);
      }
}

/* Enable the active moves, in which WEB takes part, to be processed.  */

static void
enable_move (web)
     struct web *web;
{
  struct move_list *ml;
  for (ml = web->moves; ml; ml = ml->next)
    if (ml->move->type == ACTIVE)
      {
	remove_list (ml->move->dlink, &mv_active);
	put_move (ml->move, WORKLIST);
      }
}

/* Decrement the degree of node WEB by the amount DEC.
   Possibly change the type of WEB, if the number of conflicts is
   now smaller than its freedom.  */

static void
decrement_degree (web, dec)
     struct web *web;
     int dec;
{
  int before = web->num_conflicts;
  web->num_conflicts -= dec;
  if (web->num_conflicts < NUM_REGS (web) && before >= NUM_REGS (web))
    {
      struct conflict_link *a;
      enable_move (web);
      for (a = web->conflict_list; a; a = a->next)
	{
	  struct web *aweb = a->t;
	  if (aweb->type != SELECT && aweb->type != COALESCED)
	    enable_move (aweb);
	}
      if (web->type != FREEZE)
	{
	  remove_web_from_list (web);
	  if (web->moves)
	    put_web (web, FREEZE);
	  else
	    put_web (web, SIMPLIFY);
	}
    }
}

/* Repeatedly simplify the nodes on the simplify worklists.  */

static void
simplify ()
{
  struct dlist *d;
  struct web *web;
  struct conflict_link *wl;
  while (1)
    {
      /* We try hard to color all the webs resulting from spills first.
	 Without that on register starved machines (x86 e.g) with some live
	 DImode pseudos, -fPIC, and an asm requiring %edx, it might be, that
	 we do rounds over rounds, because the conflict graph says, we can
	 simplify those short webs, but later due to irregularities we can't
	 color those pseudos.  So we have to spill them, which in later rounds
	 leads to other spills.  */
      d = pop_list (&WEBS(SIMPLIFY));
      if (!d)
	d = pop_list (&WEBS(SIMPLIFY_FAT));
      if (!d)
	d = pop_list (&WEBS(SIMPLIFY_SPILL));
      if (!d)
	break;
      web = DLIST_WEB (d);
      ra_debug_msg (DUMP_PROCESS, " simplifying web %3d, conflicts = %d\n",
		 web->id, web->num_conflicts);
      put_web (web, SELECT);
      for (wl = web->conflict_list; wl; wl = wl->next)
	{
	  struct web *pweb = wl->t;
	  if (pweb->type != SELECT && pweb->type != COALESCED)
	    {
	      decrement_degree (pweb, 1 + web->add_hardregs);
	    }
	}
    }
}

/* Helper function to remove a move from the movelist of the web.  */

static void
remove_move_1 (web, move)
     struct web *web;
     struct move *move;
{
  struct move_list *ml = web->moves;
  if (!ml)
    return;
  if (ml->move == move)
    {
      web->moves = ml->next;
      return;
    }
  for (; ml->next && ml->next->move != move; ml = ml->next) ;
  if (!ml->next)
    return;
  ml->next = ml->next->next;
}

/* Remove a move from the movelist of the web.  Actually this is just a
   wrapper around remove_move_1(), making sure, the removed move really is
   not in the list anymore.  */

static void
remove_move (web, move)
     struct web *web;
     struct move *move;
{
  struct move_list *ml;
  remove_move_1 (web, move);
  for (ml = web->moves; ml; ml = ml->next)
    if (ml->move == move)
      abort ();
}

/* Merge the moves for the two webs into the first web's movelist.  */

void
merge_moves (u, v)
     struct web *u, *v;
{
  regset seen;
  struct move_list *ml, *ml_next;

  seen = BITMAP_XMALLOC ();
  for (ml = u->moves; ml; ml = ml->next)
    bitmap_set_bit (seen, INSN_UID (ml->move->insn));
  for (ml = v->moves; ml; ml = ml_next)
    {
      ml_next = ml->next;
      if (! bitmap_bit_p (seen, INSN_UID (ml->move->insn)))
        {
	  ml->next = u->moves;
	  u->moves = ml;
	}
    }
  BITMAP_XFREE (seen);
  v->moves = NULL;
}

/* Add a web to the simplify worklist, from the freeze worklist.  */

static void
add_worklist (web)
     struct web *web;
{
  if (web->type != PRECOLORED && !web->moves
      && web->num_conflicts < NUM_REGS (web))
    {
      remove_list (web->dlink, &WEBS(FREEZE));
      put_web (web, SIMPLIFY);
    }
}

/* Precolored node coalescing heuristic.  */

static int
ok (target, source)
     struct web *target, *source;
{
  struct conflict_link *wl;
  int i;
  int color = source->color;
  int size;

  /* Normally one would think, the next test wouldn't be needed.
     We try to coalesce S and T, and S has already a color, and we checked
     when processing the insns, that both have the same mode.  So naively
     we could conclude, that of course that mode was valid for this color.
     Hah.  But there is sparc.  Before reload there are copy insns
     (e.g. the ones copying arguments to locals) which happily refer to
     colors in invalid modes.  We can't coalesce those things.  */
  if (! HARD_REGNO_MODE_OK (source->color, GET_MODE (target->orig_x)))
    return 0;

  /* Sanity for funny modes.  */
  size = HARD_REGNO_NREGS (color, GET_MODE (target->orig_x));
  if (!size)
    return 0;

  /* We can't coalesce target with a precolored register which isn't in
     usable_regs.  */
  for (i = size; i--;)
    if (TEST_HARD_REG_BIT (never_use_colors, color + i)
	|| !TEST_HARD_REG_BIT (target->usable_regs, color + i)
	/* Before usually calling ok() at all, we already test, if the
	   candidates conflict in sup_igraph.  But when wide webs are
	   coalesced to hardregs, we only test the hardweb coalesced into.
	   This is only the begin color.  When actually coalescing both,
	   it will also take the following size colors, i.e. their webs.
	   We nowhere checked if the candidate possibly conflicts with
	   one of _those_, which is possible with partial conflicts,
	   so we simply do it here (this does one bit-test more than
	   necessary, the first color).  Note, that if X is precolored
	   bit [X*num_webs + Y] can't be set (see add_conflict_edge()).  */
	|| TEST_BIT (sup_igraph,
		     target->id * num_webs + hardreg2web[color + i]->id))
      return 0;

  for (wl = target->conflict_list; wl; wl = wl->next)
    {
      struct web *pweb = wl->t;
      if (pweb->type == SELECT || pweb->type == COALESCED)
	continue;

      /* Coalescing target (T) and source (S) is o.k, if for
	 all conflicts C of T it is true, that:
	  1) C will be colored, or
	  2) C is a hardreg (precolored), or
	  3) C already conflicts with S too, or
	  4) a web which contains C conflicts already with S.
	 XXX: we handle here only the special case of 4), that C is
	 a subreg, and the containing thing is the reg itself, i.e.
	 we dont handle the situation, were T conflicts with
	 (subreg:SI x 1), and S conflicts with (subreg:DI x 0), which
	 would be allowed also, as the S-conflict overlaps
	 the T-conflict.
         So, we first test the whole web for any of these conditions, and
         continue with the next C, if 1, 2 or 3 is true.  */
      if (pweb->num_conflicts < NUM_REGS (pweb)
	  || pweb->type == PRECOLORED
	  || TEST_BIT (igraph, igraph_index (source->id, pweb->id)) )
	continue;

      /* This is reached, if not one of 1, 2 or 3 was true.  In the case C has
         no subwebs, 4 can't be true either, so we can't coalesce S and T.  */
      if (wl->sub == NULL)
        return 0;
      else
	{
	  /* The main webs do _not_ conflict, only some parts of both.  This
	     means, that 4 is possibly true, so we need to check this too.
	     For this we go thru all sub conflicts between T and C, and see if
	     the target part of C already conflicts with S.  When this is not
	     the case we disallow coalescing.  */
	  struct sub_conflict *sl;
	  for (sl = wl->sub; sl; sl = sl->next)
	    {
              if (!TEST_BIT (igraph, igraph_index (source->id, sl->t->id)))
	        return 0;
	    }
        }
    }
  return 1;
}

/* Non-precolored node coalescing heuristic.  */

static int
conservative (target, source)
     struct web *target, *source;
{
  unsigned int k;
  unsigned int loop;
  regset seen;
  struct conflict_link *wl;
  unsigned int num_regs = NUM_REGS (target); /* XXX */

  /* k counts the resulting conflict weight, if target and source
     would be merged, and all low-degree neighbors would be
     removed.  */
  k = 0 * MAX (target->add_hardregs, source->add_hardregs);
  seen = BITMAP_XMALLOC ();
  for (loop = 0; loop < 2; loop++)
    for (wl = ((loop == 0) ? target : source)->conflict_list;
	 wl; wl = wl->next)
      {
	struct web *pweb = wl->t;
	if (pweb->type != SELECT && pweb->type != COALESCED
	    && pweb->num_conflicts >= NUM_REGS (pweb)
	    && ! REGNO_REG_SET_P (seen, pweb->id))
	  {
	    SET_REGNO_REG_SET (seen, pweb->id);
	    k += 1 + pweb->add_hardregs;
	  }
      }
  BITMAP_XFREE (seen);

  if (k >= num_regs)
    return 0;
  return 1;
}

/* If the web is coalesced, return it's alias.  Otherwise, return what
   was passed in.  */

struct web *
alias (web)
     struct web *web;
{
  while (web->type == COALESCED)
    web = web->alias;
  return web;
}

/* Returns nonzero, if the TYPE belongs to one of those representing
   SIMPLIFY types.  */

static inline unsigned int
simplify_p (type)
     enum node_type type;
{
  return type == SIMPLIFY || type == SIMPLIFY_SPILL || type == SIMPLIFY_FAT;
}

/* Actually combine two webs, that can be coalesced.  */

static void
combine (u, v)
     struct web *u, *v;
{
  int i;
  struct conflict_link *wl;
  if (u == v || v->type == COALESCED)
    abort ();
  if ((u->regno >= max_normal_pseudo) != (v->regno >= max_normal_pseudo))
    abort ();
  remove_web_from_list (v);
  put_web (v, COALESCED);
  v->alias = u;
  u->is_coalesced = 1;
  v->is_coalesced = 1;
  u->num_aliased += 1 + v->num_aliased;
  if (flag_ra_merge_spill_costs && u->type != PRECOLORED)
    u->spill_cost += v->spill_cost;
    /*u->spill_cost = MAX (u->spill_cost, v->spill_cost);*/
  merge_moves (u, v);
  /* combine add_hardregs's of U and V.  */

  for (wl = v->conflict_list; wl; wl = wl->next)
    {
      struct web *pweb = wl->t;
      /* We don't strictly need to move conflicts between webs which are
	 already coalesced or selected, if we do iterated coalescing, or
	 better if we need not to be able to break aliases again.
	 I.e. normally we would use the condition
	 (pweb->type != SELECT && pweb->type != COALESCED).
	 But for now we simply merge all conflicts.  It doesn't take that
         much time.  */
      if (1)
	{
	  struct web *web = u;
	  int nregs = 1 + v->add_hardregs;
	  if (u->type == PRECOLORED)
	    nregs = HARD_REGNO_NREGS (u->color, GET_MODE (v->orig_x));

	  /* For precolored U's we need to make conflicts between V's
	     neighbors and as many hardregs from U as V needed if it gets
	     color U.  For now we approximate this by V->add_hardregs, which
	     could be too much in multi-length classes.  We should really
	     count how many hardregs are needed for V with color U.  When U
	     isn't precolored this loop breaks out after one iteration.  */
	  for (i = 0; i < nregs; i++)
	    {
	      if (u->type == PRECOLORED)
		web = hardreg2web[i + u->color];
	      if (wl->sub == NULL)
		record_conflict (web, pweb);
	      else
		{
		  struct sub_conflict *sl;
		  /* So, between V and PWEB there are sub_conflicts.  We
		     need to relocate those conflicts to be between WEB (==
		     U when it wasn't precolored) and PWEB.  In the case
		     only a part of V conflicted with (part of) PWEB we
		     nevertheless make the new conflict between the whole U
		     and the (part of) PWEB.  Later we might try to find in
		     U the correct subpart corresponding (by size and
		     offset) to the part of V (sl->s) which was the source
		     of the conflict.  */
		  for (sl = wl->sub; sl; sl = sl->next)
		    {
		      /* Beware: sl->s is no subweb of web (== U) but of V.
			 We try to search a corresponding subpart of U.
			 If we found none we let it conflict with the whole U.
			 Note that find_subweb() only looks for mode and
			 subreg_byte of the REG rtx but not for the pseudo
			 reg number (otherwise it would be guaranteed to
			 _not_ find any subpart).  */
		      struct web *sweb = NULL;
		      if (SUBWEB_P (sl->s))
			sweb = find_subweb (web, sl->s->orig_x);
		      if (!sweb)
			sweb = web;
		      record_conflict (sweb, sl->t);
		    }
		}
	      if (u->type != PRECOLORED)
		break;
	    }
	  if (pweb->type != SELECT && pweb->type != COALESCED)
	    decrement_degree (pweb, 1 + v->add_hardregs);
	}
    }

  /* Now merge the usable_regs together.  */
  /* XXX That merging might normally make it necessary to
     adjust add_hardregs, which also means to adjust neighbors.  This can
     result in making some more webs trivially colorable, (or the opposite,
     if this increases our add_hardregs).  Because we intersect the
     usable_regs it should only be possible to decrease add_hardregs.  So a
     conservative solution for now is to simply don't change it.  */
  u->use_my_regs = 1;
  AND_HARD_REG_SET (u->usable_regs, v->usable_regs);
  u->regclass = reg_class_subunion[u->regclass][v->regclass];
  /* Count number of possible hardregs.  This might make U a spillweb,
     but that could also happen, if U and V together had too many
     conflicts.  */
  u->num_freedom = hard_regs_count (u->usable_regs);
  u->num_freedom -= u->add_hardregs;
  /* The next would mean an invalid coalesced move (both webs have no
     possible hardreg in common), so abort.  */
  if (!u->num_freedom)
    abort();

  if (u->num_conflicts >= NUM_REGS (u)
      && (u->type == FREEZE || simplify_p (u->type)))
    {
      remove_web_from_list (u);
      put_web (u, SPILL);
    }

  /* We want the most relaxed combination of spill_temp state.
     I.e. if any was no spilltemp or a spilltemp2, the result is so too,
     otherwise if any is short, the result is too.  It remains, when both
     are normal spilltemps.  */
  if (v->spill_temp == 0)
    u->spill_temp = 0;
  else if (v->spill_temp == 2 && u->spill_temp != 0)
    u->spill_temp = 2;
  else if (v->spill_temp == 3 && u->spill_temp == 1)
    u->spill_temp = 3;
}

/* Attempt to coalesce the first thing on the move worklist.
   This is used only for iterated coalescing.  */

static void
coalesce ()
{
  struct dlist *d = pop_list (&mv_worklist);
  struct move *m = DLIST_MOVE (d);
  struct web *source = alias (m->source_web);
  struct web *target = alias (m->target_web);

  if (target->type == PRECOLORED)
    {
      struct web *h = source;
      source = target;
      target = h;
    }
  if (source == target)
    {
      remove_move (source, m);
      put_move (m, MV_COALESCED);
      add_worklist (source);
    }
  else if (target->type == PRECOLORED
	   || TEST_BIT (sup_igraph, source->id * num_webs + target->id)
	   || TEST_BIT (sup_igraph, target->id * num_webs + source->id))
    {
      remove_move (source, m);
      remove_move (target, m);
      put_move (m, CONSTRAINED);
      add_worklist (source);
      add_worklist (target);
    }
  else if ((source->type == PRECOLORED && ok (target, source))
	   || (source->type != PRECOLORED
	       && conservative (target, source)))
    {
      remove_move (source, m);
      remove_move (target, m);
      put_move (m, MV_COALESCED);
      combine (source, target);
      add_worklist (source);
    }
  else
    put_move (m, ACTIVE);
}

/* Freeze the moves associated with the web.  Used for iterated coalescing.  */

static void
freeze_moves (web)
     struct web *web;
{
  struct move_list *ml, *ml_next;
  for (ml = web->moves; ml; ml = ml_next)
    {
      struct move *m = ml->move;
      struct web *src, *dest;
      ml_next = ml->next;
      if (m->type == ACTIVE)
	remove_list (m->dlink, &mv_active);
      else
	remove_list (m->dlink, &mv_worklist);
      put_move (m, FROZEN);
      remove_move (web, m);
      src = alias (m->source_web);
      dest = alias (m->target_web);
      src = (src == web) ? dest : src;
      remove_move (src, m);
      /* XXX GA use the original v, instead of alias(v) */
      if (!src->moves && src->num_conflicts < NUM_REGS (src))
	{
	  remove_list (src->dlink, &WEBS(FREEZE));
	  put_web (src, SIMPLIFY);
	}
    }
}

/* Freeze the first thing on the freeze worklist (only for iterated
   coalescing).  */

static void
freeze ()
{
  struct dlist *d = pop_list (&WEBS(FREEZE));
  put_web (DLIST_WEB (d), SIMPLIFY);
  freeze_moves (DLIST_WEB (d));
}

/* The current spill heuristic.  Returns a number for a WEB.
   Webs with higher numbers are selected later.  */

static unsigned HOST_WIDE_INT (*spill_heuristic) PARAMS ((struct web *));

static unsigned HOST_WIDE_INT default_spill_heuristic PARAMS ((struct web *));

/* Our default heuristic is similar to spill_cost / num_conflicts.
   Just scaled for integer arithmetic, and it favors coalesced webs,
   and webs which span more insns with deaths.  */

static unsigned HOST_WIDE_INT
default_spill_heuristic (web)
     struct web *web;
{
  unsigned HOST_WIDE_INT ret;
  unsigned int divisor = 1;
  /* Make coalesce targets cheaper to spill, because they will be broken
     up again into smaller parts.  */
  if (flag_ra_break_aliases)
    divisor += web->num_aliased;
  divisor += web->num_conflicts;
  ret = ((web->spill_cost << 8) + divisor - 1) / divisor;
  /* It is better to spill webs that span more insns (deaths in our
     case) than other webs with the otherwise same spill_cost.  So make
     them a little bit cheaper.  Remember that spill_cost is unsigned.  */
  if (web->span_deaths < ret)
    ret -= web->span_deaths;
  return ret;
}

/* Select the cheapest spill to be potentially spilled (we don't
   *actually* spill until we need to).  */

static void
select_spill ()
{
  unsigned HOST_WIDE_INT best = (unsigned HOST_WIDE_INT) -1;
  struct dlist *bestd = NULL;
  unsigned HOST_WIDE_INT best2 = (unsigned HOST_WIDE_INT) -1;
  struct dlist *bestd2 = NULL;
  struct dlist *d;
  for (d = WEBS(SPILL); d; d = d->next)
    {
      struct web *w = DLIST_WEB (d);
      unsigned HOST_WIDE_INT cost = spill_heuristic (w);
      if ((!w->spill_temp) && cost < best)
	{
	  best = cost;
	  bestd = d;
	}
      /* Specially marked spill temps can be spilled.  Also coalesce
	 targets can.  Eventually they will be broken up later in the
	 colorizing process, so if we have nothing better take that.  */
      else if ((w->spill_temp == 2 || w->is_coalesced) && cost < best2)
	{
	  best2 = cost;
	  bestd2 = d;
	}
    }
  if (!bestd)
    {
      bestd = bestd2;
      best = best2;
    }
  if (!bestd)
    abort ();

  /* Note the potential spill.  */
  DLIST_WEB (bestd)->was_spilled = 1;
  remove_list (bestd, &WEBS(SPILL));
  put_web (DLIST_WEB (bestd), SIMPLIFY);
  freeze_moves (DLIST_WEB (bestd));
  ra_debug_msg (DUMP_PROCESS, " potential spill web %3d, conflicts = %d\n",
	     DLIST_WEB (bestd)->id, DLIST_WEB (bestd)->num_conflicts);
}

/* Given a set of forbidden colors to begin at, and a set of still
   free colors, and MODE, returns nonzero of color C is still usable.  */

static int
color_usable_p (c, dont_begin_colors, free_colors, mode)
     int c;
     HARD_REG_SET dont_begin_colors, free_colors;
     enum machine_mode mode;
{
  if (!TEST_HARD_REG_BIT (dont_begin_colors, c)
      && TEST_HARD_REG_BIT (free_colors, c)
      && HARD_REGNO_MODE_OK (c, mode))
    {
      int i, size;
      size = HARD_REGNO_NREGS (c, mode);
      for (i = 1; i < size && TEST_HARD_REG_BIT (free_colors, c + i); i++);
      if (i == size)
	return 1;
    }
  return 0;
}

/* I don't want to clutter up the actual code with ifdef's.  */
#ifdef REG_ALLOC_ORDER
#define INV_REG_ALLOC_ORDER(c) inv_reg_alloc_order[c]
#else
#define INV_REG_ALLOC_ORDER(c) c
#endif

/* Searches in FREE_COLORS for a block of hardregs of the right length
   for MODE, which doesn't begin at a hardreg mentioned in DONT_BEGIN_COLORS.
   If it needs more than one hardreg it prefers blocks beginning
   at an even hardreg, and only gives an odd begin reg if no other
   block could be found.  */

int
get_free_reg (dont_begin_colors, free_colors, mode)
     HARD_REG_SET dont_begin_colors, free_colors;
     enum machine_mode mode;
{
  int c;
  int last_resort_reg = -1;
  int pref_reg = -1;
  int pref_reg_order = INT_MAX;
  int last_resort_reg_order = INT_MAX;

  for (c = 0; c < FIRST_PSEUDO_REGISTER; c++)
    if (!TEST_HARD_REG_BIT (dont_begin_colors, c)
	&& TEST_HARD_REG_BIT (free_colors, c)
	&& HARD_REGNO_MODE_OK (c, mode))
      {
	int i, size;
	size = HARD_REGNO_NREGS (c, mode);
	for (i = 1; i < size && TEST_HARD_REG_BIT (free_colors, c + i); i++);
	if (i != size)
	  {
	    c += i;
	    continue;
	  }
	if (i == size)
	  {
	    if (size < 2 || (c & 1) == 0)
	      {
		if (INV_REG_ALLOC_ORDER (c) < pref_reg_order)
		  {
		    pref_reg = c;
		    pref_reg_order = INV_REG_ALLOC_ORDER (c);
		  }
	      }
	    else if (INV_REG_ALLOC_ORDER (c) < last_resort_reg_order)
	      {
		last_resort_reg = c;
		last_resort_reg_order = INV_REG_ALLOC_ORDER (c);
	      }
	  }
	else
	  c += i;
      }
  return pref_reg >= 0 ? pref_reg : last_resort_reg;
}

/* Similar to get_free_reg(), but first search in colors provided
   by BIAS _and_ PREFER_COLORS, then in BIAS alone, then in PREFER_COLORS
   alone, and only then for any free color.  If flag_ra_biased is zero
   only do the last two steps.  */

static int
get_biased_reg (dont_begin_colors, bias, prefer_colors, free_colors, mode)
     HARD_REG_SET dont_begin_colors, bias, prefer_colors, free_colors;
     enum machine_mode mode;
{
  int c = -1;
  HARD_REG_SET s;
  if (flag_ra_biased)
    {
      COPY_HARD_REG_SET (s, dont_begin_colors);
      IOR_COMPL_HARD_REG_SET (s, bias);
      IOR_COMPL_HARD_REG_SET (s, prefer_colors);
      c = get_free_reg (s, free_colors, mode);
      if (c >= 0)
	return c;
      COPY_HARD_REG_SET (s, dont_begin_colors);
      IOR_COMPL_HARD_REG_SET (s, bias);
      c = get_free_reg (s, free_colors, mode);
      if (c >= 0)
	return c;
    }
  COPY_HARD_REG_SET (s, dont_begin_colors);
  IOR_COMPL_HARD_REG_SET (s, prefer_colors);
  c = get_free_reg (s, free_colors, mode);
  if (c >= 0)
      return c;
  c = get_free_reg (dont_begin_colors, free_colors, mode);
  return c;
}

/* Counts the number of non-overlapping bitblocks of length LEN
   in FREE_COLORS.  */

static int
count_long_blocks (free_colors, len)
     HARD_REG_SET free_colors;
     int len;
{
  int i, j;
  int count = 0;
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (!TEST_HARD_REG_BIT (free_colors, i))
	continue;
      for (j = 1; j < len; j++)
	if (!TEST_HARD_REG_BIT (free_colors, i + j))
	  break;
      /* Bits [i .. i+j-1] are free.  */
      if (j == len)
	count++;
      i += j - 1;
    }
  return count;
}

/* Given a hardreg set S, return a string representing it.
   Either as 0/1 string, or as hex value depending on the implementation
   of hardreg sets.  Note that this string is statically allocated.  */

static char *
hardregset_to_string (s)
     HARD_REG_SET s;
{
  static char string[/*FIRST_PSEUDO_REGISTER + 30*/1024];
#if FIRST_PSEUDO_REGISTER <= HOST_BITS_PER_WIDE_INT
  sprintf (string, HOST_WIDE_INT_PRINT_HEX, s);
#else
  char *c = string;
  int i,j;
  c += sprintf (c, "{ ");
  for (i = 0;i < HARD_REG_SET_LONGS; i++)
    {
      for (j = 0; j < HOST_BITS_PER_WIDE_INT; j++)
	  c += sprintf (c, "%s", ( 1 << j) & s[i] ? "1" : "0");
      c += sprintf (c, "%s", i ? ", " : "");
    }
  c += sprintf (c, " }");
#endif
  return string;
}

/* For WEB, look at its already colored neighbors, and calculate
   the set of hardregs which is not allowed as color for WEB.  Place
   that set int *RESULT.  Note that the set of forbidden begin colors
   is not the same as all colors taken up by neighbors.  E.g. suppose
   two DImode webs, but only the lo-part from one conflicts with the
   hipart from the other, and suppose the other gets colors 2 and 3
   (it needs two SImode hardregs).  Now the first can take also color
   1 or 2, although in those cases there's a partial overlap.  Only
   3 can't be used as begin color.  */

static void
calculate_dont_begin (web, result)
     struct web *web;
     HARD_REG_SET *result;
{
  struct conflict_link *wl;
  HARD_REG_SET dont_begin;
  /* The bits set in dont_begin correspond to the hardregs, at which
     WEB may not begin.  This differs from the set of _all_ hardregs which
     are taken by WEB's conflicts in the presence of wide webs, where only
     some parts conflict with others.  */
  CLEAR_HARD_REG_SET (dont_begin);
  for (wl = web->conflict_list; wl; wl = wl->next)
    {
      struct web *w;
      struct web *ptarget = alias (wl->t);
      struct sub_conflict *sl = wl->sub;
      w = sl ? sl->t : wl->t;
      while (w)
	{
	  if (ptarget->type == COLORED || ptarget->type == PRECOLORED)
	    {
	      struct web *source = (sl) ? sl->s : web;
	      unsigned int tsize = HARD_REGNO_NREGS (ptarget->color,
						     GET_MODE (w->orig_x));
	      /* ssize is only a first guess for the size.  */
	      unsigned int ssize = HARD_REGNO_NREGS (ptarget->color, GET_MODE
					             (source->orig_x));
	      unsigned int tofs = 0;
	      unsigned int sofs = 0;
	      /* C1 and C2 can become negative, so unsigned
		 would be wrong.  */
	      int c1, c2;

	      if (SUBWEB_P (w)
		  && GET_MODE_SIZE (GET_MODE (w->orig_x)) >= UNITS_PER_WORD)
		tofs = (SUBREG_BYTE (w->orig_x) / UNITS_PER_WORD);
	      if (SUBWEB_P (source)
		  && GET_MODE_SIZE (GET_MODE (source->orig_x))
		     >= UNITS_PER_WORD)
		sofs = (SUBREG_BYTE (source->orig_x) / UNITS_PER_WORD);
	      c1 = ptarget->color + tofs - sofs - ssize + 1;
	      c2 = ptarget->color + tofs + tsize - 1 - sofs;
	      if (c2 >= 0)
		{
		  if (c1 < 0)
		    c1 = 0;
		  /* Because ssize was only guessed above, which influenced our
		     begin color (c1), we need adjustment, if for that color
		     another size would be needed.  This is done by moving
		     c1 to a place, where the last of sources hardregs does not
		     overlap the first of targets colors.  */
		  while (c1 + sofs
			 + HARD_REGNO_NREGS (c1, GET_MODE (source->orig_x)) - 1
			 < ptarget->color + tofs)
		    c1++;
		  while (c1 > 0 && c1 + sofs
			 + HARD_REGNO_NREGS (c1, GET_MODE (source->orig_x)) - 1
			 > ptarget->color + tofs)
		    c1--;
		  for (; c1 <= c2; c1++)
		    SET_HARD_REG_BIT (dont_begin, c1);
		}
	    }
	  /* The next if() only gets true, if there was no wl->sub at all, in
	     which case we are only making one go thru this loop with W being
	     a whole web.  */
	  if (!sl)
	    break;
	  sl = sl->next;
	  w = sl ? sl->t : NULL;
	}
    }
  COPY_HARD_REG_SET (*result, dont_begin);
}

/* Try to assign a color to WEB.  If HARD if nonzero, we try many
   tricks to get it one color, including respilling already colored
   neighbors.

   We also trie very hard, to not constrain the uncolored non-spill
   neighbors, which need more hardregs than we.  Consider a situation, 2
   hardregs free for us (0 and 1), and one of our neighbors needs 2
   hardregs, and only conflicts with us.  There are 3 hardregs at all.  Now
   a simple minded method might choose 1 as color for us.  Then our neighbor
   has two free colors (0 and 2) as it should, but they are not consecutive,
   so coloring it later would fail.  This leads to nasty problems on
   register starved machines, so we try to avoid this.  */

static void
colorize_one_web (web, hard)
     struct web *web;
     int hard;
{
  struct conflict_link *wl;
  HARD_REG_SET colors, dont_begin;
  int c = -1;
  int bestc = -1;
  int neighbor_needs= 0;
  struct web *fat_neighbor = NULL;
  struct web *fats_parent = NULL;
  int num_fat = 0;
  int long_blocks = 0;
  int best_long_blocks = -1;
  HARD_REG_SET fat_colors;
  HARD_REG_SET bias;

  if (web->regno >= max_normal_pseudo)
    hard = 0;

  /* First we want to know the colors at which we can't begin.  */
  calculate_dont_begin (web, &dont_begin);
  CLEAR_HARD_REG_SET (bias);

  /* Now setup the set of colors used by our neighbors neighbors,
     and search the biggest noncolored neighbor.  */
  neighbor_needs = web->add_hardregs + 1;
  for (wl = web->conflict_list; wl; wl = wl->next)
    {
      struct web *w;
      struct web *ptarget = alias (wl->t);
      struct sub_conflict *sl = wl->sub;
      IOR_HARD_REG_SET (bias, ptarget->bias_colors);
      w = sl ? sl->t : wl->t;
      if (ptarget->type != COLORED && ptarget->type != PRECOLORED
	  && !ptarget->was_spilled)
        while (w)
	  {
	    if (find_web_for_subweb (w)->type != COALESCED
		&& w->add_hardregs >= neighbor_needs)
	      {
		neighbor_needs = w->add_hardregs;
		fat_neighbor = w;
		fats_parent = ptarget;
		num_fat++;
	      }
	    if (!sl)
	      break;
	    sl = sl->next;
	    w = sl ? sl->t : NULL;
	  }
    }

  ra_debug_msg (DUMP_COLORIZE, "colorize web %d [don't begin at %s]", web->id,
             hardregset_to_string (dont_begin));

  /* If there are some fat neighbors, remember their usable regs,
     and how many blocks are free in it for that neighbor.  */
  if (num_fat)
    {
      COPY_HARD_REG_SET (fat_colors, fats_parent->usable_regs);
      long_blocks = count_long_blocks (fat_colors, neighbor_needs + 1);
    }

  /* We break out, if we found a color which doesn't constrain
     neighbors, or if we can't find any colors.  */
  while (1)
    {
      HARD_REG_SET call_clobbered;

      /* Here we choose a hard-reg for the current web.  For non spill
         temporaries we first search in the hardregs for it's prefered
	 class, then, if we found nothing appropriate, in those of the
	 alternate class.  For spill temporaries we only search in
	 usable_regs of this web (which is probably larger than that of
	 the preferred or alternate class).  All searches first try to
	 find a non-call-clobbered hard-reg.
         XXX this should be more finegraned... First look into preferred
         non-callclobbered hardregs, then _if_ the web crosses calls, in
         alternate non-cc hardregs, and only _then_ also in preferred cc
         hardregs (and alternate ones).  Currently we don't track the number
         of calls crossed for webs.  We should.  */
      if (web->use_my_regs)
	{
	  COPY_HARD_REG_SET (colors, web->usable_regs);
	  AND_HARD_REG_SET (colors,
			    usable_regs[reg_preferred_class (web->regno)]);
	}
      else
	COPY_HARD_REG_SET (colors,
			   usable_regs[reg_preferred_class (web->regno)]);
#ifdef CLASS_CANNOT_CHANGE_MODE
      if (web->mode_changed)
        AND_COMPL_HARD_REG_SET (colors, reg_class_contents[
			          (int) CLASS_CANNOT_CHANGE_MODE]);
#endif
      COPY_HARD_REG_SET (call_clobbered, colors);
      AND_HARD_REG_SET (call_clobbered, call_used_reg_set);

      /* If this web got a color in the last pass, try to give it the
	 same color again.  This will to much better colorization
	 down the line, as we spilled for a certain coloring last time.  */
      if (web->old_color)
	{
	  c = web->old_color - 1;
	  if (!color_usable_p (c, dont_begin, colors,
			       PSEUDO_REGNO_MODE (web->regno)))
	    c = -1;
	}
      else
	c = -1;
      if (c < 0)
	c = get_biased_reg (dont_begin, bias, web->prefer_colors,
			    call_clobbered, PSEUDO_REGNO_MODE (web->regno));
      if (c < 0)
	c = get_biased_reg (dont_begin, bias, web->prefer_colors,
			  colors, PSEUDO_REGNO_MODE (web->regno));

      if (c < 0)
	{
	  if (web->use_my_regs)
	    IOR_HARD_REG_SET (colors, web->usable_regs);
	  else
	    IOR_HARD_REG_SET (colors, usable_regs
			      [reg_alternate_class (web->regno)]);
#ifdef CLASS_CANNOT_CHANGE_MODE
	  if (web->mode_changed)
	    AND_COMPL_HARD_REG_SET (colors, reg_class_contents[
				      (int) CLASS_CANNOT_CHANGE_MODE]);
#endif
	  COPY_HARD_REG_SET (call_clobbered, colors);
	  AND_HARD_REG_SET (call_clobbered, call_used_reg_set);

	  c = get_biased_reg (dont_begin, bias, web->prefer_colors,
			    call_clobbered, PSEUDO_REGNO_MODE (web->regno));
	  if (c < 0)
	    c = get_biased_reg (dont_begin, bias, web->prefer_colors,
			      colors, PSEUDO_REGNO_MODE (web->regno));
	}
      if (c < 0)
	break;
      if (bestc < 0)
        bestc = c;
      /* If one of the yet uncolored neighbors, which is not a potential
	 spill needs a block of hardregs be sure, not to destroy such a block
	 by coloring one reg in the middle.  */
      if (num_fat)
	{
	  int i;
	  int new_long;
	  HARD_REG_SET colors1;
	  COPY_HARD_REG_SET (colors1, fat_colors);
	  for (i = 0; i < 1 + web->add_hardregs; i++)
	    CLEAR_HARD_REG_BIT (colors1, c + i);
	  new_long = count_long_blocks (colors1, neighbor_needs + 1);
	  /* If we changed the number of long blocks, and it's now smaller
	     than needed, we try to avoid this color.  */
	  if (long_blocks != new_long && new_long < num_fat)
	    {
	      if (new_long > best_long_blocks)
		{
		  best_long_blocks = new_long;
		  bestc = c;
		}
	      SET_HARD_REG_BIT (dont_begin, c);
	      ra_debug_msg (DUMP_COLORIZE, " avoid %d", c);
	    }
	  else
	    /* We found a color which doesn't destroy a block.  */
	    break;
	}
      /* If we havee no fat neighbors, the current color won't become
	 "better", so we've found it.  */
      else
	break;
    }
  ra_debug_msg (DUMP_COLORIZE, " --> got %d", c < 0 ? bestc : c);
  if (bestc >= 0 && c < 0 && !web->was_spilled)
    {
      /* This is a non-potential-spill web, which got a color, which did
	 destroy a hardreg block for one of it's neighbors.  We color
	 this web anyway and hope for the best for the neighbor, if we are
	 a spill temp.  */
      if (1 || web->spill_temp)
        c = bestc;
      ra_debug_msg (DUMP_COLORIZE, " [constrains neighbors]");
    }
  ra_debug_msg (DUMP_COLORIZE, "\n");

  if (c < 0)
    {
      /* Guard against a simplified node being spilled.  */
      /* Don't abort.  This can happen, when e.g. enough registers
	 are available in colors, but they are not consecutive.  This is a
	 very serious issue if this web is a short live one, because
	 even if we spill this one here, the situation won't become better
	 in the next iteration.  It probably will have the same conflicts,
	 those will have the same colors, and we would come here again, for
	 all parts, in which this one gets splitted by the spill.  This
	 can result in endless iteration spilling the same register again and
	 again.  That's why we try to find a neighbor, which spans more
	 instructions that ourself, and got a color, and try to spill _that_.

	 if (DLIST_WEB (d)->was_spilled < 0)
	 abort (); */
      if (hard && (!web->was_spilled || web->spill_temp))
	{
	  unsigned int loop;
	  struct web *try = NULL;
	  struct web *candidates[8];

	  ra_debug_msg (DUMP_COLORIZE, "  *** %d spilled, although %s ***\n",
		     web->id, web->spill_temp ? "spilltemp" : "non-spill");
	  /* We make multiple passes over our conflicts, first trying to
	     spill those webs, which only got a color by chance, but
	     were potential spill ones, and if that isn't enough, in a second
	     pass also to spill normal colored webs.  If we still didn't find
	     a candidate, but we are a spill-temp, we make a third pass
	     and include also webs, which were targets for coalescing, and
	     spill those.  */
	  memset (candidates, 0, sizeof candidates);
#define set_cand(i, w) \
	  do { \
	      if (!candidates[(i)] \
		  || (candidates[(i)]->spill_cost < (w)->spill_cost)) \
		candidates[(i)] = (w); \
	  } while (0)
	  for (wl = web->conflict_list; wl; wl = wl->next)
	    {
	      struct web *w = wl->t;
	      struct web *aw = alias (w);
	      /* If we are a spill-temp, we also look at webs coalesced
		 to precolored ones.  Otherwise we only look at webs which
		 themself were colored, or coalesced to one.  */
	      if (aw->type == PRECOLORED && w != aw && web->spill_temp
		  && flag_ra_optimistic_coalescing)
		{
		  if (!w->spill_temp)
		    set_cand (4, w);
		  else if (web->spill_temp == 2
			   && w->spill_temp == 2
			   && w->spill_cost < web->spill_cost)
		    set_cand (5, w);
		  else if (web->spill_temp != 2
			   && (w->spill_temp == 2
			       || w->spill_cost < web->spill_cost))
		    set_cand (6, w);
		  continue;
		}
	      if (aw->type != COLORED)
		continue;
	      if (w->type == COLORED && !w->spill_temp && !w->is_coalesced
		  && w->was_spilled)
		{
		  if (w->spill_cost < web->spill_cost)
		    set_cand (0, w);
		  else if (web->spill_temp)
		    set_cand (1, w);
		}
	      if (w->type == COLORED && !w->spill_temp && !w->is_coalesced
		  && !w->was_spilled)
		{
		  if (w->spill_cost < web->spill_cost)
		    set_cand (2, w);
		  else if (web->spill_temp && web->spill_temp != 2)
		    set_cand (3, w);
		}
	      if (web->spill_temp)
		{
		  if (w->type == COLORED && w->spill_temp == 2
		      && !w->is_coalesced
		      && (w->spill_cost < web->spill_cost
			  || web->spill_temp != 2))
		    set_cand (4, w);
		  if (!aw->spill_temp)
		    set_cand (5, aw);
		  if (aw->spill_temp == 2
		      && (aw->spill_cost < web->spill_cost
			  || web->spill_temp != 2))
		    set_cand (6, aw);
		  /* For boehm-gc/misc.c.  If we are a difficult spilltemp,
		     also coalesced neighbors are a chance, _even_ if they
		     too are spilltemps.  At least their coalscing can be
		     broken up, which may be reset usable_regs, and makes
		     it easier colorable.  */
		  if (web->spill_temp != 2 && aw->is_coalesced
		      && flag_ra_optimistic_coalescing)
		    set_cand (7, aw);
		}
	    }
	  for (loop = 0; try == NULL && loop < 8; loop++)
	    if (candidates[loop])
	      try = candidates[loop];
#undef set_cand
	  if (try)
	    {
	      int old_c = try->color;
	      if (try->type == COALESCED)
		{
		  if (alias (try)->type != PRECOLORED)
		    abort ();
		  ra_debug_msg (DUMP_COLORIZE, "  breaking alias %d -> %d\n",
			     try->id, alias (try)->id);
		  break_precolored_alias (try);
		  colorize_one_web (web, hard);
		}
	      else
		{
		  remove_list (try->dlink, &WEBS(COLORED));
		  put_web (try, SPILLED);
		  /* Now try to colorize us again.  Can recursively make other
		     webs also spill, until there are no more unspilled
		     neighbors.  */
		  ra_debug_msg (DUMP_COLORIZE, "  trying to spill %d\n", try->id);
		  colorize_one_web (web, hard);
		  if (web->type != COLORED)
		    {
		      /* We tried recursively to spill all already colored
			 neighbors, but we are still uncolorable.  So it made
			 no sense to spill those neighbors.  Recolor them.  */
		      remove_list (try->dlink, &WEBS(SPILLED));
		      put_web (try, COLORED);
		      try->color = old_c;
		      ra_debug_msg (DUMP_COLORIZE,
				    "  spilling %d was useless\n", try->id);
		    }
		  else
		    {
		      ra_debug_msg (DUMP_COLORIZE,
				    "  to spill %d was a good idea\n",
				    try->id);
		      remove_list (try->dlink, &WEBS(SPILLED));
		      if (try->was_spilled)
			colorize_one_web (try, 0);
		      else
			colorize_one_web (try, hard - 1);
		    }
		}
	    }
	  else
	    /* No more chances to get a color, so give up hope and
	       spill us.  */
	    put_web (web, SPILLED);
	}
      else
        put_web (web, SPILLED);
    }
  else
    {
      put_web (web, COLORED);
      web->color = c;
      if (flag_ra_biased)
	{
	  int nregs = HARD_REGNO_NREGS (c, GET_MODE (web->orig_x));
	  for (wl = web->conflict_list; wl; wl = wl->next)
	    {
	      struct web *ptarget = alias (wl->t);
	      int i;
	      for (i = 0; i < nregs; i++)
		SET_HARD_REG_BIT (ptarget->bias_colors, c + i);
	    }
	}
    }
  if (web->regno >= max_normal_pseudo && web->type == SPILLED)
    {
      web->color = an_unusable_color;
      remove_list (web->dlink, &WEBS(SPILLED));
      put_web (web, COLORED);
    }
  if (web->type == SPILLED && flag_ra_optimistic_coalescing
      && web->is_coalesced)
    {
      ra_debug_msg (DUMP_COLORIZE, "breaking aliases to web %d:", web->id);
      restore_conflicts_from_coalesce (web);
      break_aliases_to_web (web);
      insert_coalesced_conflicts ();
      ra_debug_msg (DUMP_COLORIZE, "\n");
      remove_list (web->dlink, &WEBS(SPILLED));
      put_web (web, SELECT);
      web->color = -1;
    }
}

/* Assign the colors to all nodes on the select stack.  And update the
   colors of coalesced webs.  */

static void
assign_colors ()
{
  struct dlist *d;

  while (WEBS(SELECT))
    {
      struct web *web;
      d = pop_list (&WEBS(SELECT));
      web = DLIST_WEB (d);
      colorize_one_web (DLIST_WEB (d), 1);
    }

  for (d = WEBS(COALESCED); d; d = d->next)
    {
      struct web *a = alias (DLIST_WEB (d));
      DLIST_WEB (d)->color = a->color;
    }
}

/* WEB is a spilled web.  Look if we can improve the cost of the graph,
   by coloring WEB, even if we then need to spill some of it's neighbors.
   For this we calculate the cost for each color C, that results when we
   _would_ give WEB color C (i.e. the cost of the then spilled neighbors).
   If the lowest cost among them is smaller than the spillcost of WEB, we
   do that recoloring, and instead spill the neighbors.

   This can sometime help, when due to irregularities in register file,
   and due to multi word pseudos, the colorization is suboptimal.  But
   be aware, that currently this pass is quite slow.  */

static void
try_recolor_web (web)
     struct web *web;
{
  struct conflict_link *wl;
  unsigned HOST_WIDE_INT *cost_neighbors;
  unsigned int *min_color;
  int newcol, c;
  HARD_REG_SET precolored_neighbors, spill_temps;
  HARD_REG_SET possible_begin, wide_seen;
  cost_neighbors = (unsigned HOST_WIDE_INT *)
    xcalloc (FIRST_PSEUDO_REGISTER, sizeof (cost_neighbors[0]));
  /* For each hard-regs count the number of preceding hardregs, which
     would overlap this color, if used in WEB's mode.  */
  min_color = (unsigned int *) xcalloc (FIRST_PSEUDO_REGISTER, sizeof (int));
  CLEAR_HARD_REG_SET (possible_begin);
  for (c = 0; c < FIRST_PSEUDO_REGISTER; c++)
    {
      int i, nregs;
      if (!HARD_REGNO_MODE_OK (c, GET_MODE (web->orig_x)))
	continue;
      nregs = HARD_REGNO_NREGS (c, GET_MODE (web->orig_x));
      for (i = 0; i < nregs; i++)
	if (!TEST_HARD_REG_BIT (web->usable_regs, c + i))
	  break;
      if (i < nregs || nregs == 0)
	continue;
      SET_HARD_REG_BIT (possible_begin, c);
      for (; nregs--;)
	if (!min_color[c + nregs])
	  min_color[c + nregs] = 1 + c;
    }
  CLEAR_HARD_REG_SET (precolored_neighbors);
  CLEAR_HARD_REG_SET (spill_temps);
  CLEAR_HARD_REG_SET (wide_seen);
  for (wl = web->conflict_list; wl; wl = wl->next)
    {
      HARD_REG_SET dont_begin;
      struct web *web2 = alias (wl->t);
      struct conflict_link *nn;
      int c1, c2;
      int wide_p = 0;
      if (wl->t->type == COALESCED || web2->type != COLORED)
	{
	  if (web2->type == PRECOLORED)
	    {
	      c1 = min_color[web2->color];
	      c1 = (c1 == 0) ? web2->color : (c1 - 1);
	      c2 = web2->color;
	      for (; c1 <= c2; c1++)
	        SET_HARD_REG_BIT (precolored_neighbors, c1);
	    }
	  continue;
	}
      /* Mark colors for which some wide webs are involved.  For
	 those the independent sets are not simply one-node graphs, so
	 they can't be recolored independ from their neighborhood.  This
	 means, that our cost calculation can be incorrect (assuming it
	 can avoid spilling a web because it thinks some colors are available,
	 although it's neighbors which itself need recoloring might take
	 away exactly those colors).  */
      if (web2->add_hardregs)
	wide_p = 1;
      for (nn = web2->conflict_list; nn && !wide_p; nn = nn->next)
	if (alias (nn->t)->add_hardregs)
	  wide_p = 1;
      calculate_dont_begin (web2, &dont_begin);
      c1 = min_color[web2->color];
      /* Note that min_color[] contains 1-based values (zero means
	 undef).  */
      c1 = c1 == 0 ? web2->color : (c1 - 1);
      c2 = web2->color + HARD_REGNO_NREGS (web2->color, GET_MODE
					   (web2->orig_x)) - 1;
      for (; c1 <= c2; c1++)
	if (TEST_HARD_REG_BIT (possible_begin, c1))
	  {
	    int nregs;
	    HARD_REG_SET colors;
	    nregs = HARD_REGNO_NREGS (c1, GET_MODE (web->orig_x));
	    COPY_HARD_REG_SET (colors, web2->usable_regs);
	    for (; nregs--;)
	      CLEAR_HARD_REG_BIT (colors, c1 + nregs);
	    if (wide_p)
	      SET_HARD_REG_BIT (wide_seen, c1);
	    if (get_free_reg (dont_begin, colors,
			      GET_MODE (web2->orig_x)) < 0)
	      {
		if (web2->spill_temp)
		  SET_HARD_REG_BIT (spill_temps, c1);
		else
		  cost_neighbors[c1] += web2->spill_cost;
	      }
	  }
    }
  newcol = -1;
  for (c = 0; c < FIRST_PSEUDO_REGISTER; c++)
    if (TEST_HARD_REG_BIT (possible_begin, c)
	&& !TEST_HARD_REG_BIT (precolored_neighbors, c)
	&& !TEST_HARD_REG_BIT (spill_temps, c)
	&& (newcol == -1
	    || cost_neighbors[c] < cost_neighbors[newcol]))
      newcol = c;
  if (newcol >= 0 && cost_neighbors[newcol] < web->spill_cost)
    {
      int nregs = HARD_REGNO_NREGS (newcol, GET_MODE (web->orig_x));
      unsigned HOST_WIDE_INT cost = 0;
      int *old_colors;
      struct conflict_link *wl_next;
      ra_debug_msg (DUMP_COLORIZE, "try to set web %d to color %d\n", web->id,
		 newcol);
      remove_list (web->dlink, &WEBS(SPILLED));
      put_web (web, COLORED);
      web->color = newcol;
      old_colors = (int *) xcalloc (num_webs, sizeof (int));
      for (wl = web->conflict_list; wl; wl = wl_next)
	{
	  struct web *web2 = alias (wl->t);
	  /* If web2 is a coalesce-target, and will become spilled
	     below in colorize_one_web(), and the current conflict wl
	     between web and web2 was only the result of that coalescing
	     this conflict will be deleted, making wl invalid.  So save
	     the next conflict right now.  Note that if web2 has indeed
	     such state, then wl->next can not be deleted in this
	     iteration.  */
	  wl_next = wl->next;
	  if (web2->type == COLORED)
	    {
	      int nregs2 = HARD_REGNO_NREGS (web2->color, GET_MODE
					     (web2->orig_x));
	      if (web->color >= web2->color + nregs2
		  || web2->color >= web->color + nregs)
		continue;
	      old_colors[web2->id] = web2->color + 1;
	      web2->color = -1;
	      remove_list (web2->dlink, &WEBS(COLORED));
	      web2->type = SELECT;
	      /* Allow webs to be spilled.  */
	      if (web2->spill_temp == 0 || web2->spill_temp == 2)
		web2->was_spilled = 1;
	      colorize_one_web (web2, 1);
	      if (web2->type == SPILLED)
		cost += web2->spill_cost;
	    }
	}
      /* The actual cost may be smaller than the guessed one, because
	 partial conflicts could result in some conflicting webs getting
	 a color, where we assumed it must be spilled.  See the comment
         above what happens, when wide webs are involved, and why in that
         case there might actually be some webs spilled although thought to
         be colorable.  */
      if (cost > cost_neighbors[newcol]
	  && nregs == 1 && !TEST_HARD_REG_BIT (wide_seen, newcol))
	abort ();
      /* But if the new spill-cost is higher than our own, then really loose.
	 Respill us and recolor neighbors as before.  */
      if (cost > web->spill_cost)
	{
	  ra_debug_msg (DUMP_COLORIZE,
		     "reset coloring of web %d, too expensive\n", web->id);
	  remove_list (web->dlink, &WEBS(COLORED));
	  web->color = -1;
	  put_web (web, SPILLED);
	  for (wl = web->conflict_list; wl; wl = wl->next)
	    {
	      struct web *web2 = alias (wl->t);
	      if (old_colors[web2->id])
		{
		  if (web2->type == SPILLED)
		    {
		      remove_list (web2->dlink, &WEBS(SPILLED));
		      web2->color = old_colors[web2->id] - 1;
		      put_web (web2, COLORED);
		    }
		  else if (web2->type == COLORED)
		    web2->color = old_colors[web2->id] - 1;
		  else if (web2->type == SELECT)
		    /* This means, that WEB2 once was a part of a coalesced
		       web, which got spilled in the above colorize_one_web()
		       call, and whose parts then got splitted and put back
		       onto the SELECT stack.  As the cause for that splitting
		       (the coloring of WEB) was worthless, we should again
		       coalesce the parts, as they were before.  For now we
		       simply leave them SELECTed, for our caller to take
		       care.  */
		    ;
		  else
		    abort ();
		}
	    }
	}
      free (old_colors);
    }
  free (min_color);
  free (cost_neighbors);
}

/* This ensures that all conflicts of coalesced webs are seen from
   the webs coalesced into.  combine() only adds the conflicts which
   at the time of combining were not already SELECTed or COALESCED
   to not destroy num_conflicts.  Here we add all remaining conflicts
   and thereby destroy num_conflicts.  This should be used when num_conflicts
   isn't used anymore, e.g. on a completely colored graph.  */

static void
insert_coalesced_conflicts ()
{
  struct dlist *d;
  for (d = WEBS(COALESCED); 0 && d; d = d->next)
    {
      struct web *web = DLIST_WEB (d);
      struct web *aweb = alias (web);
      struct conflict_link *wl;
      for (wl = web->conflict_list; wl; wl = wl->next)
	{
	  struct web *tweb = aweb;
	  int i;
	  int nregs = 1 + web->add_hardregs;
	  if (aweb->type == PRECOLORED)
	    nregs = HARD_REGNO_NREGS (aweb->color, GET_MODE (web->orig_x));
	  for (i = 0; i < nregs; i++)
	    {
	      if (aweb->type == PRECOLORED)
		tweb = hardreg2web[i + aweb->color];
	      /* There might be some conflict edges laying around
		 where the usable_regs don't intersect.  This can happen
		 when first some webs were coalesced and conflicts
		 propagated, then some combining narrowed usable_regs and
		 further coalescing ignored those conflicts.  Now there are
		 some edges to COALESCED webs but not to it's alias.
		 So abort only when they really should conflict.  */
	      if ((!(tweb->type == PRECOLORED
		     || TEST_BIT (sup_igraph, tweb->id * num_webs + wl->t->id))
		   || !(wl->t->type == PRECOLORED
		        || TEST_BIT (sup_igraph,
				     wl->t->id * num_webs + tweb->id)))
		  && hard_regs_intersect_p (&tweb->usable_regs,
					    &wl->t->usable_regs))
		abort ();
	      /*if (wl->sub == NULL)
		record_conflict (tweb, wl->t);
	      else
		{
		  struct sub_conflict *sl;
		  for (sl = wl->sub; sl; sl = sl->next)
		    record_conflict (tweb, sl->t);
		}*/
	      if (aweb->type != PRECOLORED)
		break;
	    }
	}
    }
}

/* A function suitable to pass to qsort().  Compare the spill costs
   of webs W1 and W2.  When used by qsort, this would order webs with
   largest cost first.  */

static int
comp_webs_maxcost (w1, w2)
     const void *w1, *w2;
{
  struct web *web1 = *(struct web **)w1;
  struct web *web2 = *(struct web **)w2;
  if (web1->spill_cost > web2->spill_cost)
    return -1;
  else if (web1->spill_cost < web2->spill_cost)
    return 1;
  else
    return 0;
}

/* This tries to recolor all spilled webs.  See try_recolor_web()
   how this is done.  This just calls it for each spilled web.  */

static void
recolor_spills ()
{
  unsigned int i, num;
  struct web **order2web;
  num = num_webs - num_subwebs;
  order2web = (struct web **) xmalloc (num * sizeof (order2web[0]));
  for (i = 0; i < num; i++)
    {
      order2web[i] = id2web[i];
      /* If we aren't breaking aliases, combine() wasn't merging the
         spill_costs.  So do that here to have sane measures.  */
      if (!flag_ra_merge_spill_costs && id2web[i]->type == COALESCED)
	alias (id2web[i])->spill_cost += id2web[i]->spill_cost;
    }
  qsort (order2web, num, sizeof (order2web[0]), comp_webs_maxcost);
  insert_coalesced_conflicts ();
  dump_graph_cost (DUMP_COSTS, "before spill-recolor");
  for (i = 0; i < num; i++)
    {
      struct web *web = order2web[i];
      if (web->type == SPILLED)
	try_recolor_web (web);
    }
  /* It might have been decided in try_recolor_web() (in colorize_one_web())
     that a coalesced web should be spilled, so it was put on the
     select stack.  Those webs need recoloring again, and all remaining
     coalesced webs might need their color updated, so simply call
     assign_colors() again.  */
  assign_colors ();
  free (order2web);
}

/* This checks the current color assignment for obvious errors,
   like two conflicting webs overlapping in colors, or the used colors
   not being in usable regs.  */

static void
check_colors ()
{
  unsigned int i;
  for (i = 0; i < num_webs - num_subwebs; i++)
    {
      struct web *web = id2web[i];
      struct web *aweb = alias (web);
      struct conflict_link *wl;
      int nregs, c;
      if (aweb->type == SPILLED || web->regno >= max_normal_pseudo)
	continue;
      else if (aweb->type == COLORED)
	nregs = HARD_REGNO_NREGS (aweb->color, GET_MODE (web->orig_x));
      else if (aweb->type == PRECOLORED)
	nregs = 1;
      else
	abort ();
      /* The color must be valid for the original usable_regs.  */
      for (c = 0; c < nregs; c++)
	if (!TEST_HARD_REG_BIT (web->usable_regs, aweb->color + c))
	  abort ();
      /* Search the original (pre-coalesce) conflict list.  In the current
	 one some inprecise conflicts may be noted (due to combine() or
	 insert_coalesced_conflicts() relocating partial conflicts) making
	 it look like some wide webs are in conflict and having the same
	 color.  */
      wl = (web->have_orig_conflicts ? web->orig_conflict_list
	    : web->conflict_list);
      for (; wl; wl = wl->next)
	if (wl->t->regno >= max_normal_pseudo)
	  continue;
	else if (!wl->sub)
	  {
	    struct web *web2 = alias (wl->t);
	    int nregs2;
	    if (web2->type == COLORED)
	      nregs2 = HARD_REGNO_NREGS (web2->color, GET_MODE (web2->orig_x));
	    else if (web2->type == PRECOLORED)
	      nregs2 = 1;
	    else
	      continue;
	    if (aweb->color >= web2->color + nregs2
	        || web2->color >= aweb->color + nregs)
	      continue;
	    abort ();
	  }
	else
	  {
	    struct sub_conflict *sl;
	    int scol = aweb->color;
	    int tcol = alias (wl->t)->color;
	    if (alias (wl->t)->type == SPILLED)
	      continue;
	    for (sl = wl->sub; sl; sl = sl->next)
	      {
		int ssize = HARD_REGNO_NREGS (scol, GET_MODE (sl->s->orig_x));
		int tsize = HARD_REGNO_NREGS (tcol, GET_MODE (sl->t->orig_x));
		int sofs = 0, tofs = 0;
	        if (SUBWEB_P (sl->t)
		    && GET_MODE_SIZE (GET_MODE (sl->t->orig_x)) >= UNITS_PER_WORD)
		  tofs = (SUBREG_BYTE (sl->t->orig_x) / UNITS_PER_WORD);
	        if (SUBWEB_P (sl->s)
		    && GET_MODE_SIZE (GET_MODE (sl->s->orig_x))
		       >= UNITS_PER_WORD)
		  sofs = (SUBREG_BYTE (sl->s->orig_x) / UNITS_PER_WORD);
		if ((tcol + tofs >= scol + sofs + ssize)
		    || (scol + sofs >= tcol + tofs + tsize))
		  continue;
		abort ();
	      }
	  }
    }
}

/* WEB was a coalesced web.  Make it unaliased again, and put it
   back onto SELECT stack.  */

static void
unalias_web (web)
     struct web *web;
{
  web->alias = NULL;
  web->is_coalesced = 0;
  web->color = -1;
  /* Well, initially everything was spilled, so it isn't incorrect,
     that also the individual parts can be spilled.
     XXX this isn't entirely correct, as we also relaxed the
     spill_temp flag in combine(), which might have made components
     spill, although they were a short or spilltemp web.  */
  web->was_spilled = 1;
  remove_list (web->dlink, &WEBS(COALESCED));
  /* Spilltemps must be colored right now (i.e. as early as possible),
     other webs can be deferred to the end (the code building the
     stack assumed that in this stage only one web was colored).  */
  if (web->spill_temp && web->spill_temp != 2)
    put_web (web, SELECT);
  else
    put_web_at_end (web, SELECT);
}

/* WEB is a _target_ for coalescing which got spilled.
   Break all aliases to WEB, and restore some of its member to the state
   they were before coalescing.  Due to the suboptimal structure of
   the interference graph we need to go through all coalesced webs.
   Somewhen we'll change this to be more sane.  */

static void
break_aliases_to_web (web)
     struct web *web;
{
  struct dlist *d, *d_next;
  if (web->type != SPILLED)
    abort ();
  for (d = WEBS(COALESCED); d; d = d_next)
    {
      struct web *other = DLIST_WEB (d);
      d_next = d->next;
      /* Beware: Don't use alias() here.  We really want to check only
	 one level of aliasing, i.e. only break up webs directly
	 aliased to WEB, not also those aliased through other webs.  */
      if (other->alias == web)
	{
	  unalias_web (other);
	  ra_debug_msg (DUMP_COLORIZE, " %d", other->id);
	}
    }
  web->spill_temp = web->orig_spill_temp;
  web->spill_cost = web->orig_spill_cost;
  /* Beware: The following possibly widens usable_regs again.  While
     it was narrower there might have been some conflicts added which got
     ignored because of non-intersecting hardregsets.  All those conflicts
     would now matter again.  Fortunately we only add conflicts when
     coalescing, which is also the time of narrowing.  And we remove all
     those added conflicts again now that we unalias this web.
     Therefore this is safe to do.  */
  COPY_HARD_REG_SET (web->usable_regs, web->orig_usable_regs);
  web->is_coalesced = 0;
  web->num_aliased = 0;
  web->was_spilled = 1;
  /* Reset is_coalesced flag for webs which itself are target of coalescing.
     It was cleared above if it was coalesced to WEB.  */
  for (d = WEBS(COALESCED); d; d = d->next)
    DLIST_WEB (d)->alias->is_coalesced = 1;
}

/* WEB is a web coalesced into a precolored one.  Break that alias,
   making WEB SELECTed again.  Also restores the conflicts which resulted
   from initially coalescing both.  */

static void
break_precolored_alias (web)
     struct web *web;
{
  struct web *pre = web->alias;
  struct conflict_link *wl;
  unsigned int c = pre->color;
  unsigned int nregs = HARD_REGNO_NREGS (c, GET_MODE (web->orig_x));
  if (pre->type != PRECOLORED)
    abort ();
  unalias_web (web);
  /* Now we need to look at each conflict X of WEB, if it conflicts
     with [PRE, PRE+nregs), and remove such conflicts, of X has not other
     conflicts, which are coalesced into those precolored webs.  */
  for (wl = web->conflict_list; wl; wl = wl->next)
    {
      struct web *x = wl->t;
      struct web *y;
      unsigned int i;
      struct conflict_link *wl2;
      struct conflict_link **pcl;
      HARD_REG_SET regs;
      if (!x->have_orig_conflicts)
	continue;
      /* First look at which colors can not go away, due to other coalesces
	 still existing.  */
      CLEAR_HARD_REG_SET (regs);
      for (i = 0; i < nregs; i++)
	SET_HARD_REG_BIT (regs, c + i);
      for (wl2 = x->conflict_list; wl2; wl2 = wl2->next)
	if (wl2->t->type == COALESCED && alias (wl2->t)->type == PRECOLORED)
	  CLEAR_HARD_REG_BIT (regs, alias (wl2->t)->color);
      /* Now also remove the colors of those conflicts which already
	 were there before coalescing at all.  */
      for (wl2 = x->orig_conflict_list; wl2; wl2 = wl2->next)
	if (wl2->t->type == PRECOLORED)
	  CLEAR_HARD_REG_BIT (regs, wl2->t->color);
      /* The colors now still set are those for which WEB was the last
	 cause, i.e. those which can be removed.  */
      y = NULL;
      for (i = 0; i < nregs; i++)
	if (TEST_HARD_REG_BIT (regs, c + i))
	  {
	    struct web *sub;
	    y = hardreg2web[c + i];
	    RESET_BIT (sup_igraph, x->id * num_webs + y->id);
	    RESET_BIT (sup_igraph, y->id * num_webs + x->id);
	    RESET_BIT (igraph, igraph_index (x->id, y->id));
	    for (sub = x->subreg_next; sub; sub = sub->subreg_next)
	      RESET_BIT (igraph, igraph_index (sub->id, y->id));
	  }
      if (!y)
	continue;
      pcl = &(x->conflict_list);
      while (*pcl)
	{
	  struct web *y = (*pcl)->t;
	  if (y->type != PRECOLORED || !TEST_HARD_REG_BIT (regs, y->color))
	    pcl = &((*pcl)->next);
	  else
	    *pcl = (*pcl)->next;
	}
    }
}

/* WEB is a spilled web which was target for coalescing.
   Delete all interference edges which were added due to that coalescing,
   and break up the coalescing.  */

static void
restore_conflicts_from_coalesce (web)
     struct web *web;
{
  struct conflict_link **pcl;
  struct conflict_link *wl;
  pcl = &(web->conflict_list);
  /* No original conflict list means no conflict was added at all
     after building the graph.  So neither we nor any neighbors have
     conflicts due to this coalescing.  */
  if (!web->have_orig_conflicts)
    return;
  while (*pcl)
    {
      struct web *other = (*pcl)->t;
      for (wl = web->orig_conflict_list; wl; wl = wl->next)
	if (wl->t == other)
	  break;
      if (wl)
	{
	  /* We found this conflict also in the original list, so this
	     was no new conflict.  */
	  pcl = &((*pcl)->next);
	}
      else
	{
	  /* This is a new conflict, so delete it from us and
	     the neighbor.  */
	  struct conflict_link **opcl;
	  struct conflict_link *owl;
	  struct sub_conflict *sl;
	  wl = *pcl;
	  *pcl = wl->next;
	  if (!other->have_orig_conflicts && other->type != PRECOLORED)
	    abort ();
	  for (owl = other->orig_conflict_list; owl; owl = owl->next)
	    if (owl->t == web)
	      break;
	  if (owl)
	    abort ();
	  opcl = &(other->conflict_list);
	  while (*opcl)
	    {
	      if ((*opcl)->t == web)
		{
		  owl = *opcl;
		  *opcl = owl->next;
		  break;
		}
	      else
		{
		  opcl = &((*opcl)->next);
		}
	    }
	  if (!owl && other->type != PRECOLORED)
	    abort ();
	  /* wl and owl contain the edge data to be deleted.  */
	  RESET_BIT (sup_igraph, web->id * num_webs + other->id);
	  RESET_BIT (sup_igraph, other->id * num_webs + web->id);
	  RESET_BIT (igraph, igraph_index (web->id, other->id));
	  for (sl = wl->sub; sl; sl = sl->next)
	    RESET_BIT (igraph, igraph_index (sl->s->id, sl->t->id));
	  if (other->type != PRECOLORED)
	    {
	      for (sl = owl->sub; sl; sl = sl->next)
		RESET_BIT (igraph, igraph_index (sl->s->id, sl->t->id));
	    }
	}
    }

  /* We must restore usable_regs because record_conflict will use it.  */
  COPY_HARD_REG_SET (web->usable_regs, web->orig_usable_regs);
  /* We might have deleted some conflicts above, which really are still
     there (diamond pattern coalescing).  This is because we don't reference
     count interference edges but some of them were the result of different
     coalesces.  */
  for (wl = web->conflict_list; wl; wl = wl->next)
    if (wl->t->type == COALESCED)
      {
	struct web *tweb;
	for (tweb = wl->t->alias; tweb; tweb = tweb->alias)
	  {
	    if (wl->sub == NULL)
	      record_conflict (web, tweb);
	    else
	      {
		struct sub_conflict *sl;
		for (sl = wl->sub; sl; sl = sl->next)
		  {
		    struct web *sweb = NULL;
		    if (SUBWEB_P (sl->t))
		      sweb = find_subweb (tweb, sl->t->orig_x);
		    if (!sweb)
		      sweb = tweb;
		    record_conflict (sl->s, sweb);
		  }
	      }
	    if (tweb->type != COALESCED)
	      break;
	  }
      }
}

/* Repeatedly break aliases for spilled webs, which were target for
   coalescing, and recolorize the resulting parts.  Do this as long as
   there are any spilled coalesce targets.  */

static void
break_coalesced_spills ()
{
  int changed = 0;
  while (1)
    {
      struct dlist *d;
      struct web *web;
      for (d = WEBS(SPILLED); d; d = d->next)
	if (DLIST_WEB (d)->is_coalesced)
	  break;
      if (!d)
	break;
      changed = 1;
      web = DLIST_WEB (d);
      ra_debug_msg (DUMP_COLORIZE, "breaking aliases to web %d:", web->id);
      restore_conflicts_from_coalesce (web);
      break_aliases_to_web (web);
      /* WEB was a spilled web and isn't anymore.  Everything coalesced
	 to WEB is now SELECTed and might potentially get a color.
	 If those other webs were itself targets of coalescing it might be
	 that there are still some conflicts from aliased webs missing,
	 because they were added in combine() right into the now
	 SELECTed web.  So we need to add those missing conflicts here.  */
      insert_coalesced_conflicts ();
      ra_debug_msg (DUMP_COLORIZE, "\n");
      remove_list (d, &WEBS(SPILLED));
      put_web (web, SELECT);
      web->color = -1;
      while (WEBS(SELECT))
	{
	  d = pop_list (&WEBS(SELECT));
	  colorize_one_web (DLIST_WEB (d), 1);
	}
    }
  if (changed)
    {
      struct dlist *d;
      for (d = WEBS(COALESCED); d; d = d->next)
	{
	  struct web *a = alias (DLIST_WEB (d));
	  DLIST_WEB (d)->color = a->color;
	}
    }
  dump_graph_cost (DUMP_COSTS, "after alias-breaking");
}

/* A structure for fast hashing of a pair of webs.
   Used to cumulate savings (from removing copy insns) for coalesced webs.
   All the pairs are also put into a single linked list.  */
struct web_pair
{
  struct web_pair *next_hash;
  struct web_pair *next_list;
  struct web *smaller;
  struct web *larger;
  unsigned int conflicts;
  unsigned HOST_WIDE_INT cost;
};

/* The actual hash table.  */
#define WEB_PAIR_HASH_SIZE 8192
static struct web_pair *web_pair_hash[WEB_PAIR_HASH_SIZE];
static struct web_pair *web_pair_list;
static unsigned int num_web_pairs;

/* Clear the hash table of web pairs.  */

static void
init_web_pairs ()
{
  memset (web_pair_hash, 0, sizeof web_pair_hash);
  num_web_pairs = 0;
  web_pair_list = NULL;
}

/* Given two webs connected by a move with cost COST which together
   have CONFLICTS conflicts, add that pair to the hash table, or if
   already in, cumulate the costs and conflict number.  */

static void
add_web_pair_cost (web1, web2, cost, conflicts)
     struct web *web1, *web2;
     unsigned HOST_WIDE_INT cost;
     unsigned int conflicts;
{
  unsigned int hash;
  struct web_pair *p;
  if (web1->id > web2->id)
    {
      struct web *h = web1;
      web1 = web2;
      web2 = h;
    }
  hash = (web1->id * num_webs + web2->id) % WEB_PAIR_HASH_SIZE;
  for (p = web_pair_hash[hash]; p; p = p->next_hash)
    if (p->smaller == web1 && p->larger == web2)
      {
	p->cost += cost;
	p->conflicts += conflicts;
	return;
      }
  p = (struct web_pair *) ra_alloc (sizeof *p);
  p->next_hash = web_pair_hash[hash];
  p->next_list = web_pair_list;
  p->smaller = web1;
  p->larger = web2;
  p->conflicts = conflicts;
  p->cost = cost;
  web_pair_hash[hash] = p;
  web_pair_list = p;
  num_web_pairs++;
}

/* Suitable to be passed to qsort().  Sort web pairs so, that those
   with more conflicts and higher cost (which actually is a saving
   when the moves are removed) come first.  */

static int
comp_web_pairs (w1, w2)
     const void *w1, *w2;
{
  struct web_pair *p1 = *(struct web_pair **)w1;
  struct web_pair *p2 = *(struct web_pair **)w2;
  if (p1->conflicts > p2->conflicts)
    return -1;
  else if (p1->conflicts < p2->conflicts)
    return 1;
  else if (p1->cost > p2->cost)
    return -1;
  else if (p1->cost < p2->cost)
    return 1;
  else
    return 0;
}

/* Given the list of web pairs, begin to combine them from the one
   with the most savings.  */

static void
sort_and_combine_web_pairs (for_move)
     int for_move;
{
  unsigned int i;
  struct web_pair **sorted;
  struct web_pair *p;
  if (!num_web_pairs)
    return;
  sorted = (struct web_pair **) xmalloc (num_web_pairs * sizeof (sorted[0]));
  for (p = web_pair_list, i = 0; p; p = p->next_list)
    sorted[i++] = p;
  if (i != num_web_pairs)
    abort ();
  qsort (sorted, num_web_pairs, sizeof (sorted[0]), comp_web_pairs);

  /* After combining one pair, we actually should adjust the savings
     of the other pairs, if they are connected to one of the just coalesced
     pair.  Later.  */
  for (i = 0; i < num_web_pairs; i++)
    {
      struct web *w1, *w2;
      p = sorted[i];
      w1 = alias (p->smaller);
      w2 = alias (p->larger);
      if (!for_move && (w1->type == PRECOLORED || w2->type == PRECOLORED))
	continue;
      else if (w2->type == PRECOLORED)
	{
	  struct web *h = w1;
	  w1 = w2;
	  w2 = h;
	}
      if (w1 != w2
	  && !TEST_BIT (sup_igraph, w1->id * num_webs + w2->id)
	  && !TEST_BIT (sup_igraph, w2->id * num_webs + w1->id)
	  && w2->type != PRECOLORED
	  && hard_regs_intersect_p (&w1->usable_regs, &w2->usable_regs))
	  {
	    if (w1->type != PRECOLORED
		|| (w1->type == PRECOLORED && ok (w2, w1)))
	      combine (w1, w2);
	    else if (w1->type == PRECOLORED)
	      SET_HARD_REG_BIT (w2->prefer_colors, w1->color);
	  }
    }
  free (sorted);
}

/* Greedily coalesce all moves possible.  Begin with the web pair
   giving the most saving if coalesced.  */

static void
aggressive_coalesce ()
{
  struct dlist *d;
  struct move *m;
  init_web_pairs ();
  while ((d = pop_list (&mv_worklist)) != NULL)
    if ((m = DLIST_MOVE (d)))
      {
	struct web *s = alias (m->source_web);
	struct web *t = alias (m->target_web);
	if (t->type == PRECOLORED)
	  {
	    struct web *h = s;
	    s = t;
	    t = h;
	  }
	if (s != t
	    && t->type != PRECOLORED
	    && !TEST_BIT (sup_igraph, s->id * num_webs + t->id)
	    && !TEST_BIT (sup_igraph, t->id * num_webs + s->id))
	  {
	    if ((s->type == PRECOLORED && ok (t, s))
		|| s->type != PRECOLORED)
	      {
	        put_move (m, MV_COALESCED);
		add_web_pair_cost (s, t, BLOCK_FOR_INSN (m->insn)->frequency,
				   0);
	      }
	    else if (s->type == PRECOLORED)
	      /* It is !ok(t, s).  But later when coloring the graph it might
		 be possible to take that color.  So we remember the preferred
		 color to try that first.  */
	      {
		put_move (m, CONSTRAINED);
		SET_HARD_REG_BIT (t->prefer_colors, s->color);
	      }
	  }
	else
	  {
	    put_move (m, CONSTRAINED);
	  }
      }
  sort_and_combine_web_pairs (1);
}

/* This is the difference between optimistic coalescing and
   optimistic coalescing+.  Extended coalesce tries to coalesce also
   non-conflicting nodes, not related by a move.  The criteria here is,
   the one web must be a source, the other a destination of the same insn.
   This actually makes sense, as (because they are in the same insn) they
   share many of their neighbors, and if they are coalesced, reduce the
   number of conflicts of those neighbors by one.  For this we sort the
   candidate pairs again according to savings (and this time also conflict
   number).

   This is also a comparatively slow operation, as we need to go through
   all insns, and for each insn, through all defs and uses.  */

static void
extended_coalesce_2 ()
{
  rtx insn;
  struct ra_insn_info info;
  unsigned int n;
  init_web_pairs ();
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    if (INSN_P (insn) && (info = insn_df[INSN_UID (insn)]).num_defs)
      for (n = 0; n < info.num_defs; n++)
	{
	  struct web *dest = def2web[DF_REF_ID (info.defs[n])];
	  dest = alias (find_web_for_subweb (dest));
	  if (dest->type != PRECOLORED && dest->regno < max_normal_pseudo)
	    {
	      unsigned int n2;
	      for (n2 = 0; n2 < info.num_uses; n2++)
		{
		  struct web *source = use2web[DF_REF_ID (info.uses[n2])];
		  source = alias (find_web_for_subweb (source));
		  if (source->type != PRECOLORED
		      && source != dest
		      && source->regno < max_normal_pseudo
		      /* Coalesced webs end up using the same REG rtx in
			 emit_colors().  So we can only coalesce something
			 of equal modes.  */
		      && GET_MODE (source->orig_x) == GET_MODE (dest->orig_x)
		      && !TEST_BIT (sup_igraph,
				    dest->id * num_webs + source->id)
		      && !TEST_BIT (sup_igraph,
				    source->id * num_webs + dest->id)
		      && hard_regs_intersect_p (&source->usable_regs,
						&dest->usable_regs))
		    add_web_pair_cost (dest, source,
				       BLOCK_FOR_INSN (insn)->frequency,
				       dest->num_conflicts
				       + source->num_conflicts);
		}
	    }
	}
  sort_and_combine_web_pairs (0);
}

/* Check if we forgot to coalesce some moves.  */

static void
check_uncoalesced_moves ()
{
  struct move_list *ml;
  struct move *m;
  for (ml = wl_moves; ml; ml = ml->next)
    if ((m = ml->move))
      {
	struct web *s = alias (m->source_web);
	struct web *t = alias (m->target_web);
	if (t->type == PRECOLORED)
	  {
	    struct web *h = s;
	    s = t;
	    t = h;
	  }
	if (s != t
	    && m->type != CONSTRAINED
	    /* Following can happen when a move was coalesced, but later
	       broken up again.  Then s!=t, but m is still MV_COALESCED.  */
	    && m->type != MV_COALESCED
	    && t->type != PRECOLORED
	    && ((s->type == PRECOLORED && ok (t, s))
		|| s->type != PRECOLORED)
	    && !TEST_BIT (sup_igraph, s->id * num_webs + t->id)
	    && !TEST_BIT (sup_igraph, t->id * num_webs + s->id))
	  abort ();
      }
}

/* The toplevel function in this file.  Precondition is, that
   the interference graph is built completely by ra-build.c.  This
   produces a list of spilled, colored and coalesced nodes.  */

void
ra_colorize_graph (df)
     struct df *df;
{
  if (rtl_dump_file)
    dump_igraph (df);
  build_worklists (df);

  /* With optimistic coalescing we coalesce everything we can.  */
  if (flag_ra_optimistic_coalescing)
    {
      aggressive_coalesce ();
      extended_coalesce_2 ();
    }

  /* Now build the select stack.  */
  do
    {
      simplify ();
      if (mv_worklist)
	coalesce ();
      else if (WEBS(FREEZE))
	freeze ();
      else if (WEBS(SPILL))
	select_spill ();
    }
  while (WEBS(SIMPLIFY) || WEBS(SIMPLIFY_FAT) || WEBS(SIMPLIFY_SPILL)
	 || mv_worklist || WEBS(FREEZE) || WEBS(SPILL));
  if (flag_ra_optimistic_coalescing)
    check_uncoalesced_moves ();

  /* Actually colorize the webs from the select stack.  */
  assign_colors ();
  check_colors ();
  dump_graph_cost (DUMP_COSTS, "initially");
  if (flag_ra_break_aliases)
    break_coalesced_spills ();
  check_colors ();

  /* And try to improve the cost by recoloring spilled webs.  */
  recolor_spills ();
  dump_graph_cost (DUMP_COSTS, "after spill-recolor");
  check_colors ();
}

/* Initialize this module.  */

void ra_colorize_init ()
{
  /* FIXME: Choose spill heuristic for platform if we have one */
  spill_heuristic = default_spill_heuristic;
}

/* Free all memory.  (Note that we don't need to free any per pass
   memory).  */

void
ra_colorize_free_all ()
{
  struct dlist *d;
  while ((d = pop_list (&WEBS(FREE))) != NULL)
    put_web (DLIST_WEB (d), INITIAL);
  while ((d = pop_list (&WEBS(INITIAL))) != NULL)
    {
      struct web *web =DLIST_WEB (d);
      struct web *wnext;
      web->orig_conflict_list = NULL;
      web->conflict_list = NULL;
      for (web = web->subreg_next; web; web = wnext)
	{
	  wnext = web->subreg_next;
	  free (web);
	}
      free (DLIST_WEB (d));
    }
}

/*
vim:cinoptions={.5s,g0,p5,t0,(0,^-0.5s,n-0.5s:tw=78:cindent:sw=4:
*/
