/* Graph coloring register allocator
   Copyright (C) 2001, 2002, 2003 Free Software Foundation, Inc.
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
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tm_p.h"
#include "insn-config.h"
#include "recog.h"
#include "reload.h"
#include "integrate.h"
#include "function.h"
#include "regs.h"
#include "obstack.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "df.h"
#include "expr.h"
#include "output.h"
#include "toplev.h"
#include "flags.h"
#include "ra.h"

/* This is the toplevel file of a graph coloring register allocator.
   It is able to act like a George & Appel allocator, i.e. with iterative
   coalescing plus spill coalescing/propagation.
   And it can act as a traditional Briggs allocator, although with
   optimistic coalescing.  Additionally it has a custom pass, which
   tries to reduce the overall cost of the colored graph.

   We support two modes of spilling: spill-everywhere, which is extremely
   fast, and interference region spilling, which reduces spill code to a
   large extent, but is slower.

   Helpful documents:

   Briggs, P., Cooper, K. D., and Torczon, L. 1994. Improvements to graph
   coloring register allocation. ACM Trans. Program. Lang. Syst. 16, 3 (May),
   428-455.

   Bergner, P., Dahl, P., Engebretsen, D., and O'Keefe, M. 1997. Spill code
   minimization via interference region spilling. In Proc. ACM SIGPLAN '97
   Conf. on Prog. Language Design and Implementation. ACM, 287-295.

   George, L., Appel, A.W. 1996.  Iterated register coalescing.
   ACM Trans. Program. Lang. Syst. 18, 3 (May), 300-324.

*/

/* This file contains the main entry point (reg_alloc), some helper routines
   used by more than one file of the register allocator, and the toplevel
   driver procedure (one_pass).  */

/* Things, one might do somewhen:

   * Lattice based rematerialization
   * create definitions of ever-life regs at the beginning of
     the insn chain
   * insert loads as soon, stores as late as possible
   * insert spill insns as outward as possible (either looptree, or LCM)
   * reuse stack-slots
   * delete coalesced insns.  Partly done.  The rest can only go, when we get
     rid of reload.
   * don't destroy coalescing information completely when spilling
   * use the constraints from asms
  */

static struct obstack ra_obstack;
static void create_insn_info (struct df *);
static void free_insn_info (void);
static void alloc_mem (struct df *);
static void free_mem (struct df *);
static void free_all_mem (struct df *df);
static int one_pass (struct df *, int);
static void check_df (struct df *);
static void init_ra (void);

void reg_alloc (void);

/* These global variables are "internal" to the register allocator.
   They are all documented at their declarations in ra.h.  */

/* Somewhen we want to get rid of one of those sbitmaps.
   (for now I need the sup_igraph to note if there is any conflict between
   parts of webs at all.  I can't use igraph for this, as there only the real
   conflicts are noted.)  This is only used to prevent coalescing two
   conflicting webs, were only parts of them are in conflict.  */
sbitmap igraph;
sbitmap sup_igraph;

/* Note the insns not inserted by the allocator, where we detected any
   deaths of pseudos.  It is used to detect closeness of defs and uses.
   In the first pass this is empty (we could initialize it from REG_DEAD
   notes), in the other passes it is left from the pass before.  */
sbitmap insns_with_deaths;
int death_insns_max_uid;

struct web_part *web_parts;

unsigned int num_webs;
unsigned int num_subwebs;
unsigned int num_allwebs;
struct web **id2web;
struct web *hardreg2web[FIRST_PSEUDO_REGISTER];
struct web **def2web;
struct web **use2web;
struct move_list *wl_moves;
int ra_max_regno;
short *ra_reg_renumber;
struct df *df;
bitmap *live_at_end;
int ra_pass;
unsigned int max_normal_pseudo;
int an_unusable_color;

/* The different lists on which a web can be (based on the type).  */
struct dlist *web_lists[(int) LAST_NODE_TYPE];

unsigned int last_def_id;
unsigned int last_use_id;
unsigned int last_num_webs;
int last_max_uid;
sbitmap last_check_uses;
unsigned int remember_conflicts;

int orig_max_uid;

HARD_REG_SET never_use_colors;
HARD_REG_SET usable_regs[N_REG_CLASSES];
unsigned int num_free_regs[N_REG_CLASSES];
HARD_REG_SET hardregs_for_mode[NUM_MACHINE_MODES];
HARD_REG_SET invalid_mode_change_regs;
unsigned char byte2bitcount[256];

unsigned int debug_new_regalloc = -1;
int flag_ra_biased = 0;
int flag_ra_improved_spilling = 0;
int flag_ra_ir_spilling = 0;
int flag_ra_optimistic_coalescing = 0;
int flag_ra_break_aliases = 0;
int flag_ra_merge_spill_costs = 0;
int flag_ra_spill_every_use = 0;
int flag_ra_dump_notes = 0;

/* Fast allocation of small objects, which live until the allocator
   is done.  Allocate an object of SIZE bytes.  */

void *
ra_alloc (size_t size)
{
  return obstack_alloc (&ra_obstack, size);
}

/* Like ra_alloc(), but clear the returned memory.  */

void *
ra_calloc (size_t size)
{
  void *p = obstack_alloc (&ra_obstack, size);
  memset (p, 0, size);
  return p;
}

/* Returns the number of hardregs in HARD_REG_SET RS.  */

int
hard_regs_count (HARD_REG_SET rs)
{
  int count = 0;
#ifdef HARD_REG_SET
  while (rs)
    {
      unsigned char byte = rs & 0xFF;
      rs >>= 8;
      /* Avoid memory access, if nothing is set.  */
      if (byte)
        count += byte2bitcount[byte];
    }
#else
  unsigned int ofs;
  for (ofs = 0; ofs < HARD_REG_SET_LONGS; ofs++)
    {
      HARD_REG_ELT_TYPE elt = rs[ofs];
      while (elt)
	{
	  unsigned char byte = elt & 0xFF;
	  elt >>= 8;
	  if (byte)
	    count += byte2bitcount[byte];
	}
    }
#endif
  return count;
}

/* Basically like emit_move_insn (i.e. validifies constants and such),
   but also handle MODE_CC moves (but then the operands must already
   be basically valid.  */

rtx
ra_emit_move_insn (rtx x, rtx y)
{
  enum machine_mode mode = GET_MODE (x);
  if (GET_MODE_CLASS (mode) == MODE_CC)
    return emit_insn (gen_move_insn (x, y));
  else
    return emit_move_insn (x, y);
}

int insn_df_max_uid;
struct ra_insn_info *insn_df;
static struct ref **refs_for_insn_df;

/* Create the insn_df structure for each insn to have fast access to
   all valid defs and uses in an insn.  */

static void
create_insn_info (struct df *df)
{
  rtx insn;
  struct ref **act_refs;
  insn_df_max_uid = get_max_uid ();
  insn_df = xcalloc (insn_df_max_uid, sizeof (insn_df[0]));
  refs_for_insn_df = xcalloc (df->def_id + df->use_id, sizeof (struct ref *));
  act_refs = refs_for_insn_df;
  /* We create those things backwards to mimic the order in which
     the insns are visited in rewrite_program2() and live_in().  */
  for (insn = get_last_insn (); insn; insn = PREV_INSN (insn))
    {
      int uid = INSN_UID (insn);
      unsigned int n;
      struct df_link *link;
      if (!INSN_P (insn))
	continue;
      for (n = 0, link = DF_INSN_DEFS (df, insn); link; link = link->next)
        if (link->ref
	    && (DF_REF_REGNO (link->ref) >= FIRST_PSEUDO_REGISTER
		|| !TEST_HARD_REG_BIT (never_use_colors,
				       DF_REF_REGNO (link->ref))))
	  {
	    if (n == 0)
	      insn_df[uid].defs = act_refs;
	    insn_df[uid].defs[n++] = link->ref;
	  }
      act_refs += n;
      insn_df[uid].num_defs = n;
      for (n = 0, link = DF_INSN_USES (df, insn); link; link = link->next)
        if (link->ref
	    && (DF_REF_REGNO (link->ref) >= FIRST_PSEUDO_REGISTER
		|| !TEST_HARD_REG_BIT (never_use_colors,
				       DF_REF_REGNO (link->ref))))
	  {
	    if (n == 0)
	      insn_df[uid].uses = act_refs;
	    insn_df[uid].uses[n++] = link->ref;
	  }
      act_refs += n;
      insn_df[uid].num_uses = n;
    }
  if (refs_for_insn_df + (df->def_id + df->use_id) < act_refs)
    abort ();
}

/* Free the insn_df structures.  */

static void
free_insn_info (void)
{
  free (refs_for_insn_df);
  refs_for_insn_df = NULL;
  free (insn_df);
  insn_df = NULL;
  insn_df_max_uid = 0;
}

/* Search WEB for a subweb, which represents REG.  REG needs to
   be a SUBREG, and the inner reg of it needs to be the one which is
   represented by WEB.  Returns the matching subweb or NULL.  */

struct web *
find_subweb (struct web *web, rtx reg)
{
  struct web *w;
  if (GET_CODE (reg) != SUBREG)
    abort ();
  for (w = web->subreg_next; w; w = w->subreg_next)
    if (GET_MODE (w->orig_x) == GET_MODE (reg)
	&& SUBREG_BYTE (w->orig_x) == SUBREG_BYTE (reg))
      return w;
  return NULL;
}

/* Similar to find_subweb(), but matches according to SIZE_WORD,
   a collection of the needed size and offset (in bytes).  */

struct web *
find_subweb_2 (struct web *web, unsigned int size_word)
{
  struct web *w = web;
  if (size_word == GET_MODE_SIZE (GET_MODE (web->orig_x)))
    /* size_word == size means BYTE_BEGIN(size_word) == 0.  */
    return web;
  for (w = web->subreg_next; w; w = w->subreg_next)
    {
      unsigned int bl = rtx_to_bits (w->orig_x);
      if (size_word == bl)
        return w;
    }
  return NULL;
}

/* Returns the superweb for SUBWEB.  */

struct web *
find_web_for_subweb_1 (struct web *subweb)
{
  while (subweb->parent_web)
    subweb = subweb->parent_web;
  return subweb;
}

/* Determine if two hard register sets intersect.
   Return 1 if they do.  */

int
hard_regs_intersect_p (HARD_REG_SET *a, HARD_REG_SET *b)
{
  HARD_REG_SET c;
  COPY_HARD_REG_SET (c, *a);
  AND_HARD_REG_SET (c, *b);
  GO_IF_HARD_REG_SUBSET (c, reg_class_contents[(int) NO_REGS], lose);
  return 1;
lose:
  return 0;
}

/* Allocate and initialize the memory necessary for one pass of the
   register allocator.  */

static void
alloc_mem (struct df *df)
{
  int i;
  ra_build_realloc (df);
  if (!live_at_end)
    {
      live_at_end = xmalloc ((last_basic_block + 2) * sizeof (bitmap));
      for (i = 0; i < last_basic_block + 2; i++)
	live_at_end[i] = BITMAP_XMALLOC ();
      live_at_end += 2;
    }
  create_insn_info (df);
}

/* Free the memory which isn't necessary for the next pass.  */

static void
free_mem (struct df *df ATTRIBUTE_UNUSED)
{
  free_insn_info ();
  ra_build_free ();
}

/* Free all memory allocated for the register allocator.  Used, when
   it's done.  */

static void
free_all_mem (struct df *df)
{
  unsigned int i;
  live_at_end -= 2;
  for (i = 0; i < (unsigned)last_basic_block + 2; i++)
    BITMAP_XFREE (live_at_end[i]);
  free (live_at_end);

  ra_colorize_free_all ();
  ra_build_free_all (df);
  obstack_free (&ra_obstack, NULL);
}

static long ticks_build;
static long ticks_rebuild;

/* Perform one pass of allocation.  Returns nonzero, if some spill code
   was added, i.e. if the allocator needs to rerun.  */

static int
one_pass (struct df *df, int rebuild)
{
  long ticks = clock ();
  int something_spilled;
  remember_conflicts = 0;

  /* Build the complete interference graph, or if this is not the first
     pass, rebuild it incrementally.  */
  build_i_graph (df);

  /* From now on, if we create new conflicts, we need to remember the
     initial list of conflicts per web.  */
  remember_conflicts = 1;
  if (!rebuild)
    dump_igraph_machine ();

  /* Colorize the I-graph.  This results in either a list of
     spilled_webs, in which case we need to run the spill phase, and
     rerun the allocator, or that list is empty, meaning we are done.  */
  ra_colorize_graph (df);

  last_max_uid = get_max_uid ();
  /* actual_spill() might change WEBS(SPILLED) and even empty it,
     so we need to remember it's state.  */
  something_spilled = !!WEBS(SPILLED);

  /* Add spill code if necessary.  */
  if (something_spilled)
    actual_spill ();

  ticks = clock () - ticks;
  if (rebuild)
    ticks_rebuild += ticks;
  else
    ticks_build += ticks;
  return something_spilled;
}

/* Initialize various arrays for the register allocator.  */

static void
init_ra (void)
{
  int i;
  HARD_REG_SET rs;
#ifdef ELIMINABLE_REGS
  static const struct {const int from, to; } eliminables[] = ELIMINABLE_REGS;
  unsigned int j;
#endif
  int need_fp
    = (! flag_omit_frame_pointer
       || (current_function_calls_alloca && EXIT_IGNORE_STACK)
       || FRAME_POINTER_REQUIRED);

  ra_colorize_init ();

  /* We can't ever use any of the fixed regs.  */
  COPY_HARD_REG_SET (never_use_colors, fixed_reg_set);

  /* Additionally don't even try to use hardregs, which we already
     know are not eliminable.  This includes also either the
     hard framepointer or all regs which are eliminable into the
     stack pointer, if need_fp is set.  */
#ifdef ELIMINABLE_REGS
  for (j = 0; j < ARRAY_SIZE (eliminables); j++)
    {
      if (! CAN_ELIMINATE (eliminables[j].from, eliminables[j].to)
	  || (eliminables[j].to == STACK_POINTER_REGNUM && need_fp))
	for (i = HARD_REGNO_NREGS (eliminables[j].from, Pmode); i--;)
	  SET_HARD_REG_BIT (never_use_colors, eliminables[j].from + i);
    }
#if FRAME_POINTER_REGNUM != HARD_FRAME_POINTER_REGNUM
  if (need_fp)
    for (i = HARD_REGNO_NREGS (HARD_FRAME_POINTER_REGNUM, Pmode); i--;)
      SET_HARD_REG_BIT (never_use_colors, HARD_FRAME_POINTER_REGNUM + i);
#endif

#else
  if (need_fp)
    for (i = HARD_REGNO_NREGS (FRAME_POINTER_REGNUM, Pmode); i--;)
      SET_HARD_REG_BIT (never_use_colors, FRAME_POINTER_REGNUM + i);
#endif

  /* Stack and argument pointer are also rather useless to us.  */
  for (i = HARD_REGNO_NREGS (STACK_POINTER_REGNUM, Pmode); i--;)
    SET_HARD_REG_BIT (never_use_colors, STACK_POINTER_REGNUM + i);

  for (i = HARD_REGNO_NREGS (ARG_POINTER_REGNUM, Pmode); i--;)
    SET_HARD_REG_BIT (never_use_colors, ARG_POINTER_REGNUM + i);

  for (i = 0; i < 256; i++)
    {
      unsigned char byte = ((unsigned) i) & 0xFF;
      unsigned char count = 0;
      while (byte)
	{
	  if (byte & 1)
	    count++;
	  byte >>= 1;
	}
      byte2bitcount[i] = count;
    }

  for (i = 0; i < N_REG_CLASSES; i++)
    {
      int size;
      COPY_HARD_REG_SET (rs, reg_class_contents[i]);
      AND_COMPL_HARD_REG_SET (rs, never_use_colors);
      size = hard_regs_count (rs);
      num_free_regs[i] = size;
      COPY_HARD_REG_SET (usable_regs[i], rs);
    }

  /* Setup hardregs_for_mode[].
     We are not interested only in the beginning of a multi-reg, but in
     all the hardregs involved.  Maybe HARD_REGNO_MODE_OK() only ok's
     for beginnings.  */
  for (i = 0; i < NUM_MACHINE_MODES; i++)
    {
      int reg, size;
      CLEAR_HARD_REG_SET (rs);
      for (reg = 0; reg < FIRST_PSEUDO_REGISTER; reg++)
	if (HARD_REGNO_MODE_OK (reg, i)
	    /* Ignore VOIDmode and similar things.  */
	    && (size = HARD_REGNO_NREGS (reg, i)) != 0
	    && (reg + size) <= FIRST_PSEUDO_REGISTER)
	  {
	    while (size--)
	      SET_HARD_REG_BIT (rs, reg + size);
	  }
      COPY_HARD_REG_SET (hardregs_for_mode[i], rs);
    }

  CLEAR_HARD_REG_SET (invalid_mode_change_regs);
#ifdef CANNOT_CHANGE_MODE_CLASS
  if (0)
  for (i = 0; i < NUM_MACHINE_MODES; i++)
    {
      enum machine_mode from = (enum machine_mode) i;
      enum machine_mode to;
      for (to = VOIDmode; to < MAX_MACHINE_MODE; ++to)
	{
	  int r;
	  for (r = 0; r < FIRST_PSEUDO_REGISTER; r++)
	    if (REG_CANNOT_CHANGE_MODE_P (from, to, r))
	      SET_HARD_REG_BIT (invalid_mode_change_regs, r);
	}
    }
#endif

  for (an_unusable_color = 0; an_unusable_color < FIRST_PSEUDO_REGISTER;
       an_unusable_color++)
    if (TEST_HARD_REG_BIT (never_use_colors, an_unusable_color))
      break;
  if (an_unusable_color == FIRST_PSEUDO_REGISTER)
    abort ();

  orig_max_uid = get_max_uid ();
  compute_bb_for_insn ();
  ra_reg_renumber = NULL;
  insns_with_deaths = sbitmap_alloc (orig_max_uid);
  death_insns_max_uid = orig_max_uid;
  sbitmap_ones (insns_with_deaths);
  gcc_obstack_init (&ra_obstack);
}

/* Check the consistency of DF.  This aborts if it violates some
   invariances we expect.  */

static void
check_df (struct df *df)
{
  struct df_link *link;
  rtx insn;
  int regno;
  unsigned int ui;
  bitmap b = BITMAP_XMALLOC ();
  bitmap empty_defs = BITMAP_XMALLOC ();
  bitmap empty_uses = BITMAP_XMALLOC ();

  /* Collect all the IDs of NULL references in the ID->REF arrays,
     as df.c leaves them when updating the df structure.  */
  for (ui = 0; ui < df->def_id; ui++)
    if (!df->defs[ui])
      bitmap_set_bit (empty_defs, ui);
  for (ui = 0; ui < df->use_id; ui++)
    if (!df->uses[ui])
      bitmap_set_bit (empty_uses, ui);

  /* For each insn we check if the chain of references contain each
     ref only once, doesn't contain NULL refs, or refs whose ID is invalid
     (it df->refs[id] element is NULL).  */
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    if (INSN_P (insn))
      {
	bitmap_clear (b);
	for (link = DF_INSN_DEFS (df, insn); link; link = link->next)
	  if (!link->ref || bitmap_bit_p (empty_defs, DF_REF_ID (link->ref))
	      || bitmap_bit_p (b, DF_REF_ID (link->ref)))
	    abort ();
	  else
	    bitmap_set_bit (b, DF_REF_ID (link->ref));

	bitmap_clear (b);
	for (link = DF_INSN_USES (df, insn); link; link = link->next)
	  if (!link->ref || bitmap_bit_p (empty_uses, DF_REF_ID (link->ref))
	      || bitmap_bit_p (b, DF_REF_ID (link->ref)))
	    abort ();
	  else
	    bitmap_set_bit (b, DF_REF_ID (link->ref));
      }

  /* Now the same for the chains per register number.  */
  for (regno = 0; regno < max_reg_num (); regno++)
    {
      bitmap_clear (b);
      for (link = df->regs[regno].defs; link; link = link->next)
	if (!link->ref || bitmap_bit_p (empty_defs, DF_REF_ID (link->ref))
	    || bitmap_bit_p (b, DF_REF_ID (link->ref)))
	  abort ();
	else
	  bitmap_set_bit (b, DF_REF_ID (link->ref));

      bitmap_clear (b);
      for (link = df->regs[regno].uses; link; link = link->next)
	if (!link->ref || bitmap_bit_p (empty_uses, DF_REF_ID (link->ref))
	    || bitmap_bit_p (b, DF_REF_ID (link->ref)))
	  abort ();
	else
	  bitmap_set_bit (b, DF_REF_ID (link->ref));
    }

  BITMAP_XFREE (empty_uses);
  BITMAP_XFREE (empty_defs);
  BITMAP_XFREE (b);
}

/* Main register allocator entry point.  */

void
reg_alloc (void)
{
  int changed;
  FILE *ra_dump_file = rtl_dump_file;
  rtx last = get_last_insn ();

  if (! INSN_P (last))
    last = prev_real_insn (last);
  /* If this is an empty function we shouldn't do all the following,
     but instead just setup what's necessary, and return.  */

  /* We currently rely on the existence of the return value USE as
     one of the last insns.  Add it if it's not there anymore.  */
  if (last)
    {
      edge e;
      for (e = EXIT_BLOCK_PTR->pred; e; e = e->pred_next)
	{
	  basic_block bb = e->src;
	  last = BB_END (bb);
	  if (!INSN_P (last) || GET_CODE (PATTERN (last)) != USE)
	    {
	      rtx insns;
	      start_sequence ();
	      use_return_register ();
	      insns = get_insns ();
	      end_sequence ();
	      emit_insn_after (insns, last);
	    }
	}
    }

  /* Setup debugging levels.  */
  switch (0)
    {
      /* Some useful presets of the debug level, I often use.  */
      case 0: debug_new_regalloc = DUMP_EVER; break;
      case 1: debug_new_regalloc = DUMP_COSTS; break;
      case 2: debug_new_regalloc = DUMP_IGRAPH_M; break;
      case 3: debug_new_regalloc = DUMP_COLORIZE + DUMP_COSTS; break;
      case 4: debug_new_regalloc = DUMP_COLORIZE + DUMP_COSTS + DUMP_WEBS;
	      break;
      case 5: debug_new_regalloc = DUMP_FINAL_RTL + DUMP_COSTS +
	      DUMP_CONSTRAINTS;
	      break;
      case 6: debug_new_regalloc = DUMP_VALIDIFY; break;
    }
  if (!rtl_dump_file)
    debug_new_regalloc = 0;

  /* Run regclass first, so we know the preferred and alternate classes
     for each pseudo.  Deactivate emitting of debug info, if it's not
     explicitly requested.  */
  if ((debug_new_regalloc & DUMP_REGCLASS) == 0)
    rtl_dump_file = NULL;
  regclass (get_insns (), max_reg_num (), rtl_dump_file);
  rtl_dump_file = ra_dump_file;

  /* We don't use those NOTEs, and as we anyway change all registers,
     they only make problems later.  */
  count_or_remove_death_notes (NULL, 1);

  /* Initialize the different global arrays and regsets.  */
  init_ra ();

  /* And some global variables.  */
  ra_pass = 0;
  no_new_pseudos = 0;
  max_normal_pseudo = (unsigned) max_reg_num ();
  ra_rewrite_init ();
  last_def_id = 0;
  last_use_id = 0;
  last_num_webs = 0;
  last_max_uid = 0;
  last_check_uses = NULL;
  live_at_end = NULL;
  WEBS(INITIAL) = NULL;
  WEBS(FREE) = NULL;
  memset (hardreg2web, 0, sizeof (hardreg2web));
  ticks_build = ticks_rebuild = 0;

  /* The default is to use optimistic coalescing with interference
     region spilling, without biased coloring.  */
  flag_ra_biased = 0;
  flag_ra_spill_every_use = 0;
  flag_ra_improved_spilling = 1;
  flag_ra_ir_spilling = 1;
  flag_ra_break_aliases = 0;
  flag_ra_optimistic_coalescing = 1;
  flag_ra_merge_spill_costs = 1;
  if (flag_ra_optimistic_coalescing)
    flag_ra_break_aliases = 1;
  flag_ra_dump_notes = 0;

  /* Allocate the global df structure.  */
  df = df_init ();

  /* This is the main loop, calling one_pass as long as there are still
     some spilled webs.  */
  do
    {
      ra_debug_msg (DUMP_NEARLY_EVER, "RegAlloc Pass %d\n\n", ra_pass);
      if (ra_pass++ > 40)
	internal_error ("Didn't find a coloring.\n");

      /* First collect all the register refs and put them into
	 chains per insn, and per regno.  In later passes only update
         that info from the new and modified insns.  */
      df_analyse (df, (ra_pass == 1) ? 0 : (bitmap) -1,
		  DF_HARD_REGS | DF_RD_CHAIN | DF_RU_CHAIN | DF_FOR_REGALLOC);

      if ((debug_new_regalloc & DUMP_DF) != 0)
	{
	  rtx insn;
	  df_dump (df, DF_HARD_REGS, rtl_dump_file);
	  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
            if (INSN_P (insn))
	      df_insn_debug_regno (df, insn, rtl_dump_file);
	}
      check_df (df);

      /* Now allocate the memory needed for this pass, or (if it's not the
	 first pass), reallocate only additional memory.  */
      alloc_mem (df);

      /* Build and colorize the interference graph, and possibly emit
	 spill insns.  This also might delete certain move insns.  */
      changed = one_pass (df, ra_pass > 1);

      /* If that produced no changes, the graph was colorizable.  */
      if (!changed)
	{
	  /* Change the insns to refer to the new pseudos (one per web).  */
          emit_colors (df);
	  /* Already setup a preliminary reg_renumber[] array, but don't
	     free our own version.  reg_renumber[] will again be destroyed
	     later.  We right now need it in dump_constraints() for
	     constrain_operands(1) whose subproc sometimes reference
	     it (because we are checking strictly, i.e. as if
	     after reload).  */
	  setup_renumber (0);
	  /* Delete some more of the coalesced moves.  */
	  delete_moves ();
	  dump_constraints ();
	}
      else
	{
	  /* If there were changes, this means spill code was added,
	     therefore repeat some things, including some initialization
	     of global data structures.  */
	  if ((debug_new_regalloc & DUMP_REGCLASS) == 0)
	    rtl_dump_file = NULL;
	  /* We have new pseudos (the stackwebs).  */
	  allocate_reg_info (max_reg_num (), FALSE, FALSE);
	  /* And new insns.  */
	  compute_bb_for_insn ();
	  /* Some of them might be dead.  */
	  delete_trivially_dead_insns (get_insns (), max_reg_num ());
	  /* Those new pseudos need to have their REFS count set.  */
	  reg_scan_update (get_insns (), NULL, max_regno);
	  max_regno = max_reg_num ();
	  /* And they need useful classes too.  */
	  regclass (get_insns (), max_reg_num (), rtl_dump_file);
	  rtl_dump_file = ra_dump_file;

	  /* Remember the number of defs and uses, so we can distinguish
	     new from old refs in the next pass.  */
	  last_def_id = df->def_id;
	  last_use_id = df->use_id;
	}

      /* Output the graph, and possibly the current insn sequence.  */
      dump_ra (df);
      if (changed && (debug_new_regalloc & DUMP_RTL) != 0)
	{
	  ra_print_rtl_with_bb (rtl_dump_file, get_insns ());
	  fflush (rtl_dump_file);
	}

      /* Reset the web lists.  */
      reset_lists ();
      free_mem (df);
    }
  while (changed);

  /* We are done with allocation, free all memory and output some
     debug info.  */
  free_all_mem (df);
  df_finish (df);
  if ((debug_new_regalloc & DUMP_RESULTS) == 0)
    dump_cost (DUMP_COSTS);
  ra_debug_msg (DUMP_COSTS, "ticks for build-phase: %ld\n", ticks_build);
  ra_debug_msg (DUMP_COSTS, "ticks for rebuild-phase: %ld\n", ticks_rebuild);
  if ((debug_new_regalloc & (DUMP_FINAL_RTL | DUMP_RTL)) != 0)
    ra_print_rtl_with_bb (rtl_dump_file, get_insns ());

  /* We might have new pseudos, so allocate the info arrays for them.  */
  if ((debug_new_regalloc & DUMP_SM) == 0)
    rtl_dump_file = NULL;
  no_new_pseudos = 0;
  allocate_reg_info (max_reg_num (), FALSE, FALSE);
  no_new_pseudos = 1;
  rtl_dump_file = ra_dump_file;

  /* Some spill insns could've been inserted after trapping calls, i.e.
     at the end of a basic block, which really ends at that call.
     Fixup that breakages by adjusting basic block boundaries.  */
  fixup_abnormal_edges ();

  /* Cleanup the flow graph.  */
  if ((debug_new_regalloc & DUMP_LAST_FLOW) == 0)
    rtl_dump_file = NULL;
  life_analysis (get_insns (), rtl_dump_file,
		 PROP_DEATH_NOTES | PROP_LOG_LINKS  | PROP_REG_INFO);
  cleanup_cfg (CLEANUP_EXPENSIVE);
  recompute_reg_usage (get_insns (), TRUE);
  if (rtl_dump_file)
    dump_flow_info (rtl_dump_file);
  rtl_dump_file = ra_dump_file;

  /* update_equiv_regs() can't be called after register allocation.
     It might delete some pseudos, and insert other insns setting
     up those pseudos in different places.  This of course screws up
     the allocation because that may destroy a hardreg for another
     pseudo.
     XXX we probably should do something like that on our own.  I.e.
     creating REG_EQUIV notes.  */
  /*update_equiv_regs ();*/

  /* Setup the reg_renumber[] array for reload.  */
  setup_renumber (1);
  sbitmap_free (insns_with_deaths);

  /* Remove REG_DEAD notes which are incorrectly set.  See the docu
     of that function.  */
  remove_suspicious_death_notes ();

  if ((debug_new_regalloc & DUMP_LAST_RTL) != 0)
    ra_print_rtl_with_bb (rtl_dump_file, get_insns ());
  dump_static_insn_cost (rtl_dump_file,
			 "after allocation/spilling, before reload", NULL);

  /* Allocate the reg_equiv_memory_loc array for reload.  */
  reg_equiv_memory_loc = xcalloc (max_regno, sizeof (rtx));
  /* And possibly initialize it.  */
  allocate_initial_values (reg_equiv_memory_loc);
  /* And one last regclass pass just before reload.  */
  regclass (get_insns (), max_reg_num (), rtl_dump_file);
}

/*
vim:cinoptions={.5s,g0,p5,t0,(0,^-0.5s,n-0.5s:tw=78:cindent:sw=4:
*/
