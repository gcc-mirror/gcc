/* Graph coloring register allocator
   Copyright (C) 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
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

/* Double linked list to implement the per-type lists of webs
   and moves.  */
struct dlist
{
  struct dlist *prev;
  struct dlist *next;
  union
    {
      struct web *web;
      struct move *move;
    } value;
};
/* Simple helper macros for ease of misuse.  */
#define DLIST_WEB(l) ((l)->value.web)
#define DLIST_MOVE(l) ((l)->value.move)

/* Classification of a given node (i.e. what state it's in).  */
enum node_type
{
  INITIAL = 0, FREE,
  PRECOLORED,
  SIMPLIFY, SIMPLIFY_SPILL, SIMPLIFY_FAT, FREEZE, SPILL,
  SELECT,
  SPILLED, COALESCED, COLORED,
  LAST_NODE_TYPE
};

/* A list of conflict bitmaps, factorized on the exact part of
   the source, which conflicts with the DEFs, whose ID are noted in
   the bitmap.  This is used while building web-parts with conflicts.  */
struct tagged_conflict
{
  struct tagged_conflict *next;
  bitmap conflicts;

  /* If the part of source identified by size S, byteoffset O conflicts,
     then size_word == S | (O << 16).  */
  unsigned int size_word;
};

/* Such a structure is allocated initially for each def and use.
   In the process of building the interference graph web parts are
   connected together, if they have common instructions and reference the
   same register.  That way live ranges are build (by connecting defs and
   uses) and implicitly complete webs (by connecting web parts in common
   uses).  */
struct web_part
{
  /* The def or use for this web part.  */
  struct ref *ref;
  /* The uplink implementing the disjoint set.  */
  struct web_part *uplink;

  /* Here dynamic information associated with each def/use is saved.
     This all is only valid for root web parts (uplink==NULL).
     That's the information we need to merge, if web parts are unioned.  */

  /* Number of spanned insns containing any deaths.  */
  unsigned int spanned_deaths;
  /* The list of bitmaps of DEF ID's with which this part conflicts.  */
  struct tagged_conflict *sub_conflicts;
  /* If there's any call_insn, while this part is live.  */
  unsigned int crosses_call : 1;
};

/* Web structure used to store info about connected live ranges.
   This represents the nodes of the interference graph, which gets
   colored.  It can also hold subwebs, which are contained in webs
   and represent subregs.  */
struct web
{
  /* Unique web ID.  */
  unsigned int id;

  /* Register number of the live range's variable.  */
  unsigned int regno;

  /* How many insns containing deaths do we span?  */
  unsigned int span_deaths;

  /* Spill_temp indicates if this web was part of a web spilled in the
     last iteration, or or reasons why this shouldn't be spilled again.
     1 spill web, can't be spilled.
     2 big spill web (live over some deaths).  Discouraged, but not
       impossible to spill again.
     3 short web (spans no deaths), can't be spilled.  */
  unsigned int spill_temp;

  /* When coalescing we might change spill_temp.  If breaking aliases we
     need to restore it.  */
  unsigned int orig_spill_temp;

  /* Cost of spilling.  */
  unsigned HOST_WIDE_INT spill_cost;
  unsigned HOST_WIDE_INT orig_spill_cost;

  /* How many webs are aliased to us?  */
  unsigned int num_aliased;

  /* The color we got.  This is a hardreg number.  */
  int color;
  /* 1 + the color this web got in the last pass.  If it hadn't got a color,
     or we are in the first pass, or this web is a new one, this is zero.  */
  int old_color;

  /* Now follow some flags characterizing the web.  */

  /* Nonzero, if we should use usable_regs for this web, instead of
     preferred_class() or alternate_class().  */
  unsigned int use_my_regs:1;

  /* Nonzero if we selected this web as possible spill candidate in
     select_spill().  */
  unsigned int was_spilled:1;

  /* We need to distinguish also webs which are targets of coalescing
     (all x with some y, so that x==alias(y)), but the alias field is
     only set for sources of coalescing.  This flag is set for all webs
     involved in coalescing in some way.  */
  unsigned int is_coalesced:1;

  /* Nonzero, if this web (or subweb) doesn't correspond with any of
     the current functions actual use of reg rtx.  Happens e.g. with
     conflicts to a web, of which only a part was still undefined at the
     point of that conflict.  In this case we construct a subweb
     representing these yet undefined bits to have a target for the
     conflict.  Suppose e.g. this sequence:
     (set (reg:DI x) ...)
     (set (reg:SI y) ...)
     (set (subreg:SI (reg:DI x) 0) ...)
     (use (reg:DI x))
     Here x only partly conflicts with y.  Namely only (subreg:SI (reg:DI x)
     1) conflicts with it, but this rtx doesn't show up in the program.  For
     such things an "artificial" subweb is built, and this flag is true for
     them.  */
  unsigned int artificial:1;

  /* Nonzero if we span a call_insn.  */
  unsigned int crosses_call:1;

  /* Wether the web is involved in a move insn.  */
  unsigned int move_related:1;

  /* 1 when this web (or parts thereof) are live over an abnormal edge.  */
  unsigned int live_over_abnormal:1;

  /* Nonzero if this web is used in subregs where the mode change
     was illegal for hardregs in CLASS_CANNOT_CHANGE_MODE.  */
  unsigned int mode_changed:1;

  /* Nonzero if some references of this web, where in subreg context,
     but the actual subreg is already stripped (i.e. we don't know the
     outer mode of the actual reference).  */
  unsigned int subreg_stripped:1;

  /* Nonzero, when this web stems from the last pass of the allocator,
     and all info is still valid (i.e. it wasn't spilled).  */
  unsigned int old_web:1;

  /* Used in rewrite_program2() to remember webs, which
     are already marked for (re)loading.  */
  unsigned int in_load:1;

  /* If in_load is != 0, then this is nonzero, if only one use was seen
     since insertion in loadlist.  Zero if more uses currently need a
     reload.  Used to differentiate between inserting register loads or
     directly substituting the stackref.  */
  unsigned int one_load:1;

  /* When using rewrite_program2() this flag gets set if some insns
     were inserted on behalf of this web.  IR spilling might ignore some
     deaths up to the def, so no code might be emitted and we need not to
     spill such a web again.  */
  unsigned int changed:1;

  /* With interference region spilling it's sometimes the case, that a
     bb border is also an IR border for webs, which were targets of moves,
     which are already removed due to coalescing.  All webs, which are
     a destination of such a removed move, have this flag set.  */
  unsigned int target_of_spilled_move:1;

  /* For optimistic coalescing we need to be able to break aliases, which
     includes restoring conflicts to those before coalescing.  This flag
     is set, if we have a list of conflicts before coalescing.  It's needed
     because that list is lazily constructed only when actually needed.  */
  unsigned int have_orig_conflicts:1;

  /* Current state of the node.  */
  ENUM_BITFIELD(node_type) type:5;

  /* A regclass, combined from preferred and alternate class, or calculated
     from usable_regs.  Used only for debugging, and to determine
     add_hardregs.  */
  ENUM_BITFIELD(reg_class) regclass:10;

  /* Additional consecutive hardregs needed for this web.  */
  int add_hardregs;

  /* Number of conflicts currently.  */
  int num_conflicts;

  /* Numbers of uses and defs, which belong to this web.  */
  unsigned int num_uses;
  unsigned int num_defs;

  /* The (reg:M a) or (subreg:M1 (reg:M2 a) x) rtx which this
     web is based on.  This is used to distinguish subreg webs
     from it's reg parents, and to get hold of the mode.  */
  rtx orig_x;

  /* If this web is a subweb, this point to the super web.  Otherwise
     it's NULL.  */
  struct web *parent_web;

  /* If this web is a subweb, but not the last one, this points to the
     next subweb of the same super web.  Otherwise it's NULL.  */
  struct web *subreg_next;

  /* The set of webs (or subwebs), this web conflicts with.  */
  struct conflict_link *conflict_list;

  /* If have_orig_conflicts is set this contains a copy of conflict_list,
     as it was right after building the interference graph.
     It's used for incremental i-graph building and for breaking
     coalescings again.  */
  struct conflict_link *orig_conflict_list;

  /* Bitmap of all conflicts which don't count this pass, because of
     non-intersecting hardregs of the conflicting webs.  See also
     record_conflict().  */
  bitmap useless_conflicts;

  /* Different sets of hard registers, for all usable registers, ...  */
  HARD_REG_SET usable_regs;
  /* ... the same before coalescing, ...  */
  HARD_REG_SET orig_usable_regs;
  /* ... colors of all already colored neighbors (used when biased coloring
     is active), and ...  */
  HARD_REG_SET bias_colors;
  /* ... colors of PRECOLORED webs this web is connected to by a move.  */
  HARD_REG_SET prefer_colors;

  /* Number of usable colors in usable_regs.  */
  int num_freedom;

  /* After successful coloring the graph each web gets a new reg rtx,
     with which the original uses and defs are replaced.  This is it.  */
  rtx reg_rtx;

  /* While spilling this is the rtx of the home of spilled webs.
     It can be a mem ref (a stack slot), or a pseudo register.  */
  rtx stack_slot;

  /* Used in rewrite_program2() to remember the using
     insn last seen for webs needing (re)loads.  */
  rtx last_use_insn;

  /* If this web is rematerializable, this contains the RTL pattern
     usable as source for that.  Otherwise it's NULL.  */
  rtx pattern;

  /* All the defs and uses.  There are num_defs, resp.
     num_uses elements.  */
  struct ref **defs; /* [0..num_defs-1] */
  struct ref **uses; /* [0..num_uses-1] */

  /* The web to which this web is aliased (coalesced).  If NULL, this
     web is not coalesced into some other (but might still be a target
     for other webs).  */
  struct web *alias;

  /* With iterated coalescing this is a list of active moves this web
     is involved in.  */
  struct move_list *moves;

  /* The list implementation.  */
  struct dlist *dlink;

  /* While building webs, out of web-parts, this holds a (partial)
     list of all refs for this web seen so far.  */
  struct df_link *temp_refs;
};

/* For implementing a single linked list.  */
struct web_link
{
  struct web_link *next;
  struct web *web;
};

/* A subconflict is part of a conflict edge to track precisely,
   which parts of two webs conflict, in case not all of both webs do.  */
struct sub_conflict
{
  /* The next partial conflict.  For one such list the parent-web of
     all the S webs, resp. the parent of all the T webs are constant.  */
  struct sub_conflict *next;
  struct web *s;
  struct web *t;
};

/* This represents an edge in the conflict graph.  */
struct conflict_link
{
  struct conflict_link *next;

  /* The web we conflict with (the Target of the edge).  */
  struct web *t;

  /* If not the complete source web and T conflict, this points to
     the list of parts which really conflict.  */
  struct sub_conflict *sub;
};

/* For iterated coalescing the moves can be in these states.  */
enum move_type
{
  WORKLIST, MV_COALESCED, CONSTRAINED, FROZEN, ACTIVE,
  LAST_MOVE_TYPE
};

/* Structure of a move we are considering coalescing.  */
struct move
{
  rtx insn;
  struct web *source_web;
  struct web *target_web;
  enum move_type type;
  struct dlist *dlink;
};

/* List of moves.  */
struct move_list
{
  struct move_list *next;
  struct move *move;
};

/* To have fast access to the defs and uses per insn, we have one such
   structure per insn.  The difference to the normal df.c structures is,
   that it doesn't contain any NULL refs, which df.c produces in case
   an insn was modified and it only contains refs to pseudo regs, or to
   hardregs which matter for allocation, i.e. those not in
   never_use_colors.  */
struct ra_insn_info
{
  unsigned int num_defs, num_uses;
  struct ref **defs, **uses;
};

/* The above structures are stored in this array, indexed by UID...  */
extern struct ra_insn_info *insn_df;
/* ... and the size of that array, as we add insn after setting it up.  */
extern int insn_df_max_uid;

/* The interference graph.  */
extern sbitmap igraph;
/* And how to access it.  I and J are web indices.  If the bit
   igraph_index(I, J) is set, then they conflict.  Note, that
   if only parts of webs conflict, then also only those parts
   are noted in the I-graph (i.e. the parent webs not).  */
#define igraph_index(i, j) ((i) < (j) ? ((j)*((j)-1)/2)+(i) : ((i)*((i)-1)/2)+(j))
/* This is the bitmap of all (even partly) conflicting super webs.
   If bit I*num_webs+J or J*num_webs+I is set, then I and J (both being
   super web indices) conflict, maybe only partially.  Note the
   asymmetry.  */
extern sbitmap sup_igraph;

/* After the first pass, and when interference region spilling is
   activated, bit I is set, when the insn with UID I contains some
   refs to pseudos which die at the insn.  */
extern sbitmap insns_with_deaths;
/* The size of that sbitmap.  */
extern int death_insns_max_uid;

/* All the web-parts.  There are exactly as many web-parts as there
   are register refs in the insn stream.  */
extern struct web_part *web_parts;

/* The number of all webs, including subwebs.  */
extern unsigned int num_webs;
/* The number of just the subwebs.  */
extern unsigned int num_subwebs;
/* The number of all webs, including subwebs.  */
extern unsigned int num_allwebs;

/* For easy access when given a web ID: id2web[W->id] == W.  */
extern struct web **id2web;
/* For each hardreg, the web which represents it.  */
extern struct web *hardreg2web[FIRST_PSEUDO_REGISTER];

/* Given the ID of a df_ref, which represent a DEF, def2web[ID] is
   the web, to which this def belongs.  */
extern struct web **def2web;
/* The same as def2web, just for uses.  */
extern struct web **use2web;

/* The list of all recognized and coalescable move insns.  */
extern struct move_list *wl_moves;


/* Some parts of the compiler which we run after colorizing
   clean reg_renumber[], so we need another place for the colors.
   This is copied to reg_renumber[] just before returning to toplev.  */
extern short *ra_reg_renumber;
/* The size of that array.  Some passes after coloring might have created
   new pseudos, which will get no color.  */
extern int ra_max_regno;

/* The dataflow structure of the current function, while regalloc
   runs.  */
extern struct df *df;

/* For each basic block B we have a bitmap of DF_REF_ID's of uses,
   which backward reach the end of B.  */
extern bitmap *live_at_end;

/* One pass is: collecting registers refs, building I-graph, spilling.
   And this is how often we already ran that for the current function.  */
extern int ra_pass;

/* The maximum pseudo regno, just before register allocation starts.
   While regalloc runs all pseudos with a larger number represent
   potentially stack slots or hardregs.  I call them stackwebs or
   stackpseudos.  */
extern unsigned int max_normal_pseudo;

/* One of the fixed colors.  It must be < FIRST_PSEUDO_REGISTER, because
   we sometimes want to check the color against a HARD_REG_SET.  It must
   be >= 0, because negative values mean "no color".
   This color is used for the above stackwebs, when they can't be colored.
   I.e. normally they would be spilled, but they already represent
   stackslots.  So they are colored with an invalid color.  It has
   the property that even webs which conflict can have that color at the
   same time.  I.e. a stackweb with that color really represents a
   stackslot.  */
extern int an_unusable_color;

/* While building the I-graph, every time insn UID is looked at,
   number_seen[UID] is incremented.  For debugging.  */
extern int *number_seen;

/* The different lists on which a web can be (based on the type).  */
extern struct dlist *web_lists[(int) LAST_NODE_TYPE];
#define WEBS(type) (web_lists[(int)(type)])

/* The largest DF_REF_ID of defs resp. uses, as it was in the
   last pass.  In the first pass this is zero.  Used to distinguish new
   from old references.  */
extern unsigned int last_def_id;
extern unsigned int last_use_id;

/* Similar for UIDs and number of webs.  */
extern int last_max_uid;
extern unsigned int last_num_webs;

/* If I is the ID of an old use, and last_check_uses[I] is set,
   then we must reevaluate it's flow while building the new I-graph.  */
extern sbitmap last_check_uses;

/* If nonzero, record_conflict() saves the current conflict list of
   webs in orig_conflict_list, when not already done so, and the conflict
   list is going to be changed.  It is set, after initially building the
   I-graph.  I.e. new conflicts due to coalescing trigger that copying.  */
extern unsigned int remember_conflicts;

/* The maximum UID right before calling regalloc().
   Used to detect any instructions inserted by the allocator.  */
extern int orig_max_uid;

/* A HARD_REG_SET of those color, which can't be used for coalescing.
   Includes e.g. fixed_regs.  */
extern HARD_REG_SET never_use_colors;
/* For each class C this is reg_class_contents[C] \ never_use_colors.  */
extern HARD_REG_SET usable_regs[N_REG_CLASSES];
/* For each class C the count of hardregs in usable_regs[C].  */
extern unsigned int num_free_regs[N_REG_CLASSES];
/* For each mode M the hardregs, which are MODE_OK for M, and have
   enough space behind them to hold an M value.  Additionally
   if reg R is OK for mode M, but it needs two hardregs, then R+1 will
   also be set here, even if R+1 itself is not OK for M.  I.e. this
   represent the possible resources which could be taken away be a value
   in mode M.  */
extern HARD_REG_SET hardregs_for_mode[NUM_MACHINE_MODES];
/* The set of hardregs, for which _any_ mode change is invalid.  */
extern HARD_REG_SET invalid_mode_change_regs;
/* For 0 <= I <= 255, the number of bits set in I.  Used to calculate
   the number of set bits in a HARD_REG_SET.  */
extern unsigned char byte2bitcount[256];

/* Expressive helper macros.  */
#define ID2WEB(I) id2web[I]
#define NUM_REGS(W) (((W)->type == PRECOLORED) ? 1 : (W)->num_freedom)
#define SUBWEB_P(W) (GET_CODE ((W)->orig_x) == SUBREG)

/* Constant usable as debug area to ra_debug_msg.  */
#define DUMP_COSTS		0x0001
#define DUMP_WEBS		0x0002
#define DUMP_IGRAPH		0x0004
#define DUMP_PROCESS		0x0008
#define DUMP_COLORIZE		0x0010
#define DUMP_ASM		0x0020
#define DUMP_CONSTRAINTS	0x0040
#define DUMP_RESULTS		0x0080
#define DUMP_DF			0x0100
#define DUMP_RTL		0x0200
#define DUMP_FINAL_RTL		0x0400
#define DUMP_REGCLASS		0x0800
#define DUMP_SM			0x1000
#define DUMP_LAST_FLOW		0x2000
#define DUMP_LAST_RTL		0x4000
#define DUMP_REBUILD		0x8000
#define DUMP_IGRAPH_M		0x10000
#define DUMP_VALIDIFY		0x20000
#define DUMP_EVER		((unsigned int)-1)
#define DUMP_NEARLY_EVER	(DUMP_EVER - DUMP_COSTS - DUMP_IGRAPH_M)

/* All the wanted debug levels as ORing of the various DUMP_xxx
   constants.  */
extern unsigned int debug_new_regalloc;

/* Nonzero means we want biased coloring.  */
extern int flag_ra_biased;

/* Nonzero if we want to use improved (and slow) spilling.  This
   includes also interference region spilling (see below).  */
extern int flag_ra_improved_spilling;

/* Nonzero for using interference region spilling.  Zero for improved
   Chaintin style spilling (only at deaths).  */
extern int flag_ra_ir_spilling;

/* Nonzero if we use optimistic coalescing, zero for iterated
   coalescing.  */
extern int flag_ra_optimistic_coalescing;

/* Nonzero if we want to break aliases of spilled webs.  Forced to
   nonzero, when flag_ra_optimistic_coalescing is.  */
extern int flag_ra_break_aliases;

/* Nonzero if we want to merge the spill costs of webs which
   are coalesced.  */
extern int flag_ra_merge_spill_costs;

/* Nonzero if we want to spill at every use, instead of at deaths,
   or interference region borders.  */
extern int flag_ra_spill_every_use;

/* Nonzero to output all notes in the debug dumps.  */
extern int flag_ra_dump_notes;

extern void * ra_alloc (size_t);
extern void * ra_calloc (size_t);
extern int hard_regs_count (HARD_REG_SET);
extern rtx ra_emit_move_insn (rtx, rtx);
extern void ra_debug_msg (unsigned int, const char *, ...) ATTRIBUTE_PRINTF_2;
extern int hard_regs_intersect_p (HARD_REG_SET *, HARD_REG_SET *);
extern unsigned int rtx_to_bits (rtx);
extern struct web * find_subweb (struct web *, rtx);
extern struct web * find_subweb_2 (struct web *, unsigned int);
extern struct web * find_web_for_subweb_1 (struct web *);

#define find_web_for_subweb(w) (((w)->parent_web) \
				? find_web_for_subweb_1 ((w)->parent_web) \
				: (w))

extern void ra_build_realloc (struct df *);
extern void ra_build_free (void);
extern void ra_build_free_all (struct df *);
extern void ra_colorize_init (void);
extern void ra_colorize_free_all (void);
extern void ra_rewrite_init (void);

extern void ra_print_rtx (FILE *, rtx, int);
extern void ra_print_rtx_top (FILE *, rtx, int);
extern void ra_debug_rtx (rtx);
extern void ra_debug_insns (rtx, int);
extern void ra_debug_bbi (int);
extern void ra_print_rtl_with_bb (FILE *, rtx);
extern void dump_igraph (struct df *);
extern void dump_igraph_machine (void);
extern void dump_constraints (void);
extern void dump_cost (unsigned int);
extern void dump_graph_cost (unsigned int, const char *);
extern void dump_ra (struct df *);
extern void dump_number_seen (void);
extern void dump_static_insn_cost (FILE *, const char *, const char *);
extern void dump_web_conflicts (struct web *);
extern void dump_web_insns (struct web*);
extern int web_conflicts_p (struct web *, struct web *);
extern void debug_hard_reg_set (HARD_REG_SET);

extern void remove_list (struct dlist *, struct dlist **);
extern struct dlist * pop_list (struct dlist **);
extern void record_conflict (struct web *, struct web *);
extern int memref_is_stack_slot (rtx);
extern void build_i_graph (struct df *);
extern void put_web (struct web *, enum node_type);
extern void remove_web_from_list (struct web *);
extern void reset_lists (void);
extern struct web * alias (struct web *);
extern void merge_moves (struct web *, struct web *);
extern void ra_colorize_graph (struct df *);

extern void actual_spill (void);
extern void emit_colors (struct df *);
extern void delete_moves (void);
extern void setup_renumber (int);
extern void remove_suspicious_death_notes (void);
