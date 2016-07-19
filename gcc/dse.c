/* RTL dead store elimination.
   Copyright (C) 2005-2016 Free Software Foundation, Inc.

   Contributed by Richard Sandiford <rsandifor@codesourcery.com>
   and Kenneth Zadeck <zadeck@naturalbridge.com>

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

#undef BASELINE

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "predict.h"
#include "df.h"
#include "tm_p.h"
#include "gimple-ssa.h"
#include "expmed.h"
#include "optabs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "alias.h"
#include "stor-layout.h"
#include "cfgrtl.h"
#include "cselib.h"
#include "tree-pass.h"
#include "explow.h"
#include "expr.h"
#include "dbgcnt.h"
#include "params.h"
#include "rtl-iter.h"
#include "cfgcleanup.h"

/* This file contains three techniques for performing Dead Store
   Elimination (dse).

   * The first technique performs dse locally on any base address.  It
   is based on the cselib which is a local value numbering technique.
   This technique is local to a basic block but deals with a fairly
   general addresses.

   * The second technique performs dse globally but is restricted to
   base addresses that are either constant or are relative to the
   frame_pointer.

   * The third technique, (which is only done after register allocation)
   processes the spill slots.  This differs from the second
   technique because it takes advantage of the fact that spilling is
   completely free from the effects of aliasing.

   Logically, dse is a backwards dataflow problem.  A store can be
   deleted if it if cannot be reached in the backward direction by any
   use of the value being stored.  However, the local technique uses a
   forwards scan of the basic block because cselib requires that the
   block be processed in that order.

   The pass is logically broken into 7 steps:

   0) Initialization.

   1) The local algorithm, as well as scanning the insns for the two
   global algorithms.

   2) Analysis to see if the global algs are necessary.  In the case
   of stores base on a constant address, there must be at least two
   stores to that address, to make it possible to delete some of the
   stores.  In the case of stores off of the frame or spill related
   stores, only one store to an address is necessary because those
   stores die at the end of the function.

   3) Set up the global dataflow equations based on processing the
   info parsed in the first step.

   4) Solve the dataflow equations.

   5) Delete the insns that the global analysis has indicated are
   unnecessary.

   6) Delete insns that store the same value as preceding store
   where the earlier store couldn't be eliminated.

   7) Cleanup.

   This step uses cselib and canon_rtx to build the largest expression
   possible for each address.  This pass is a forwards pass through
   each basic block.  From the point of view of the global technique,
   the first pass could examine a block in either direction.  The
   forwards ordering is to accommodate cselib.

   We make a simplifying assumption: addresses fall into four broad
   categories:

   1) base has rtx_varies_p == false, offset is constant.
   2) base has rtx_varies_p == false, offset variable.
   3) base has rtx_varies_p == true, offset constant.
   4) base has rtx_varies_p == true, offset variable.

   The local passes are able to process all 4 kinds of addresses.  The
   global pass only handles 1).

   The global problem is formulated as follows:

     A store, S1, to address A, where A is not relative to the stack
     frame, can be eliminated if all paths from S1 to the end of the
     function contain another store to A before a read to A.

     If the address A is relative to the stack frame, a store S2 to A
     can be eliminated if there are no paths from S2 that reach the
     end of the function that read A before another store to A.  In
     this case S2 can be deleted if there are paths from S2 to the
     end of the function that have no reads or writes to A.  This
     second case allows stores to the stack frame to be deleted that
     would otherwise die when the function returns.  This cannot be
     done if stores_off_frame_dead_at_return is not true.  See the doc
     for that variable for when this variable is false.

     The global problem is formulated as a backwards set union
     dataflow problem where the stores are the gens and reads are the
     kills.  Set union problems are rare and require some special
     handling given our representation of bitmaps.  A straightforward
     implementation requires a lot of bitmaps filled with 1s.
     These are expensive and cumbersome in our bitmap formulation so
     care has been taken to avoid large vectors filled with 1s.  See
     the comments in bb_info and in the dataflow confluence functions
     for details.

   There are two places for further enhancements to this algorithm:

   1) The original dse which was embedded in a pass called flow also
   did local address forwarding.  For example in

   A <- r100
   ... <- A

   flow would replace the right hand side of the second insn with a
   reference to r100.  Most of the information is available to add this
   to this pass.  It has not done it because it is a lot of work in
   the case that either r100 is assigned to between the first and
   second insn and/or the second insn is a load of part of the value
   stored by the first insn.

   insn 5 in gcc.c-torture/compile/990203-1.c simple case.
   insn 15 in gcc.c-torture/execute/20001017-2.c simple case.
   insn 25 in gcc.c-torture/execute/20001026-1.c simple case.
   insn 44 in gcc.c-torture/execute/20010910-1.c simple case.

   2) The cleaning up of spill code is quite profitable.  It currently
   depends on reading tea leaves and chicken entrails left by reload.
   This pass depends on reload creating a singleton alias set for each
   spill slot and telling the next dse pass which of these alias sets
   are the singletons.  Rather than analyze the addresses of the
   spills, dse's spill processing just does analysis of the loads and
   stores that use those alias sets.  There are three cases where this
   falls short:

     a) Reload sometimes creates the slot for one mode of access, and
     then inserts loads and/or stores for a smaller mode.  In this
     case, the current code just punts on the slot.  The proper thing
     to do is to back out and use one bit vector position for each
     byte of the entity associated with the slot.  This depends on
     KNOWING that reload always generates the accesses for each of the
     bytes in some canonical (read that easy to understand several
     passes after reload happens) way.

     b) Reload sometimes decides that spill slot it allocated was not
     large enough for the mode and goes back and allocates more slots
     with the same mode and alias set.  The backout in this case is a
     little more graceful than (a).  In this case the slot is unmarked
     as being a spill slot and if final address comes out to be based
     off the frame pointer, the global algorithm handles this slot.

     c) For any pass that may prespill, there is currently no
     mechanism to tell the dse pass that the slot being used has the
     special properties that reload uses.  It may be that all that is
     required is to have those passes make the same calls that reload
     does, assuming that the alias sets can be manipulated in the same
     way.  */

/* There are limits to the size of constant offsets we model for the
   global problem.  There are certainly test cases, that exceed this
   limit, however, it is unlikely that there are important programs
   that really have constant offsets this size.  */
#define MAX_OFFSET (64 * 1024)

/* Obstack for the DSE dataflow bitmaps.  We don't want to put these
   on the default obstack because these bitmaps can grow quite large
   (~2GB for the small (!) test case of PR54146) and we'll hold on to
   all that memory until the end of the compiler run.
   As a bonus, delete_tree_live_info can destroy all the bitmaps by just
   releasing the whole obstack.  */
static bitmap_obstack dse_bitmap_obstack;

/* Obstack for other data.  As for above: Kinda nice to be able to
   throw it all away at the end in one big sweep.  */
static struct obstack dse_obstack;

/* Scratch bitmap for cselib's cselib_expand_value_rtx.  */
static bitmap scratch = NULL;

struct insn_info_type;

/* This structure holds information about a candidate store.  */
struct store_info
{

  /* False means this is a clobber.  */
  bool is_set;

  /* False if a single HOST_WIDE_INT bitmap is used for positions_needed.  */
  bool is_large;

  /* The id of the mem group of the base address.  If rtx_varies_p is
     true, this is -1.  Otherwise, it is the index into the group
     table.  */
  int group_id;

  /* This is the cselib value.  */
  cselib_val *cse_base;

  /* This canonized mem.  */
  rtx mem;

  /* Canonized MEM address for use by canon_true_dependence.  */
  rtx mem_addr;

  /* The offset of the first and byte before the last byte associated
     with the operation.  */
  HOST_WIDE_INT begin, end;

  union
    {
      /* A bitmask as wide as the number of bytes in the word that
	 contains a 1 if the byte may be needed.  The store is unused if
	 all of the bits are 0.  This is used if IS_LARGE is false.  */
      unsigned HOST_WIDE_INT small_bitmask;

      struct
	{
	  /* A bitmap with one bit per byte.  Cleared bit means the position
	     is needed.  Used if IS_LARGE is false.  */
	  bitmap bmap;

	  /* Number of set bits (i.e. unneeded bytes) in BITMAP.  If it is
	     equal to END - BEGIN, the whole store is unused.  */
	  int count;
	} large;
    } positions_needed;

  /* The next store info for this insn.  */
  struct store_info *next;

  /* The right hand side of the store.  This is used if there is a
     subsequent reload of the mems address somewhere later in the
     basic block.  */
  rtx rhs;

  /* If rhs is or holds a constant, this contains that constant,
     otherwise NULL.  */
  rtx const_rhs;

  /* Set if this store stores the same constant value as REDUNDANT_REASON
     insn stored.  These aren't eliminated early, because doing that
     might prevent the earlier larger store to be eliminated.  */
  struct insn_info_type *redundant_reason;
};

/* Return a bitmask with the first N low bits set.  */

static unsigned HOST_WIDE_INT
lowpart_bitmask (int n)
{
  unsigned HOST_WIDE_INT mask = ~(unsigned HOST_WIDE_INT) 0;
  return mask >> (HOST_BITS_PER_WIDE_INT - n);
}

static object_allocator<store_info> cse_store_info_pool ("cse_store_info_pool");

static object_allocator<store_info> rtx_store_info_pool ("rtx_store_info_pool");

/* This structure holds information about a load.  These are only
   built for rtx bases.  */
struct read_info_type
{
  /* The id of the mem group of the base address.  */
  int group_id;

  /* The offset of the first and byte after the last byte associated
     with the operation.  If begin == end == 0, the read did not have
     a constant offset.  */
  int begin, end;

  /* The mem being read.  */
  rtx mem;

  /* The next read_info for this insn.  */
  struct read_info_type *next;
};
typedef struct read_info_type *read_info_t;

static object_allocator<read_info_type> read_info_type_pool ("read_info_pool");

/* One of these records is created for each insn.  */

struct insn_info_type
{
  /* Set true if the insn contains a store but the insn itself cannot
     be deleted.  This is set if the insn is a parallel and there is
     more than one non dead output or if the insn is in some way
     volatile.  */
  bool cannot_delete;

  /* This field is only used by the global algorithm.  It is set true
     if the insn contains any read of mem except for a (1).  This is
     also set if the insn is a call or has a clobber mem.  If the insn
     contains a wild read, the use_rec will be null.  */
  bool wild_read;

  /* This is true only for CALL instructions which could potentially read
     any non-frame memory location. This field is used by the global
     algorithm.  */
  bool non_frame_wild_read;

  /* This field is only used for the processing of const functions.
     These functions cannot read memory, but they can read the stack
     because that is where they may get their parms.  We need to be
     this conservative because, like the store motion pass, we don't
     consider CALL_INSN_FUNCTION_USAGE when processing call insns.
     Moreover, we need to distinguish two cases:
     1. Before reload (register elimination), the stores related to
	outgoing arguments are stack pointer based and thus deemed
	of non-constant base in this pass.  This requires special
	handling but also means that the frame pointer based stores
	need not be killed upon encountering a const function call.
     2. After reload, the stores related to outgoing arguments can be
	either stack pointer or hard frame pointer based.  This means
	that we have no other choice than also killing all the frame
	pointer based stores upon encountering a const function call.
     This field is set after reload for const function calls and before
     reload for const tail function calls on targets where arg pointer
     is the frame pointer.  Having this set is less severe than a wild
     read, it just means that all the frame related stores are killed
     rather than all the stores.  */
  bool frame_read;

  /* This field is only used for the processing of const functions.
     It is set if the insn may contain a stack pointer based store.  */
  bool stack_pointer_based;

  /* This is true if any of the sets within the store contains a
     cselib base.  Such stores can only be deleted by the local
     algorithm.  */
  bool contains_cselib_groups;

  /* The insn. */
  rtx_insn *insn;

  /* The list of mem sets or mem clobbers that are contained in this
     insn.  If the insn is deletable, it contains only one mem set.
     But it could also contain clobbers.  Insns that contain more than
     one mem set are not deletable, but each of those mems are here in
     order to provide info to delete other insns.  */
  store_info *store_rec;

  /* The linked list of mem uses in this insn.  Only the reads from
     rtx bases are listed here.  The reads to cselib bases are
     completely processed during the first scan and so are never
     created.  */
  read_info_t read_rec;

  /* The live fixed registers.  We assume only fixed registers can
     cause trouble by being clobbered from an expanded pattern;
     storing only the live fixed registers (rather than all registers)
     means less memory needs to be allocated / copied for the individual
     stores.  */
  regset fixed_regs_live;

  /* The prev insn in the basic block.  */
  struct insn_info_type * prev_insn;

  /* The linked list of insns that are in consideration for removal in
     the forwards pass through the basic block.  This pointer may be
     trash as it is not cleared when a wild read occurs.  The only
     time it is guaranteed to be correct is when the traversal starts
     at active_local_stores.  */
  struct insn_info_type * next_local_store;
};
typedef struct insn_info_type *insn_info_t;

static object_allocator<insn_info_type> insn_info_type_pool ("insn_info_pool");

/* The linked list of stores that are under consideration in this
   basic block.  */
static insn_info_t active_local_stores;
static int active_local_stores_len;

struct dse_bb_info_type
{
  /* Pointer to the insn info for the last insn in the block.  These
     are linked so this is how all of the insns are reached.  During
     scanning this is the current insn being scanned.  */
  insn_info_t last_insn;

  /* The info for the global dataflow problem.  */


  /* This is set if the transfer function should and in the wild_read
     bitmap before applying the kill and gen sets.  That vector knocks
     out most of the bits in the bitmap and thus speeds up the
     operations.  */
  bool apply_wild_read;

  /* The following 4 bitvectors hold information about which positions
     of which stores are live or dead.  They are indexed by
     get_bitmap_index.  */

  /* The set of store positions that exist in this block before a wild read.  */
  bitmap gen;

  /* The set of load positions that exist in this block above the
     same position of a store.  */
  bitmap kill;

  /* The set of stores that reach the top of the block without being
     killed by a read.

     Do not represent the in if it is all ones.  Note that this is
     what the bitvector should logically be initialized to for a set
     intersection problem.  However, like the kill set, this is too
     expensive.  So initially, the in set will only be created for the
     exit block and any block that contains a wild read.  */
  bitmap in;

  /* The set of stores that reach the bottom of the block from it's
     successors.

     Do not represent the in if it is all ones.  Note that this is
     what the bitvector should logically be initialized to for a set
     intersection problem.  However, like the kill and in set, this is
     too expensive.  So what is done is that the confluence operator
     just initializes the vector from one of the out sets of the
     successors of the block.  */
  bitmap out;

  /* The following bitvector is indexed by the reg number.  It
     contains the set of regs that are live at the current instruction
     being processed.  While it contains info for all of the
     registers, only the hard registers are actually examined.  It is used
     to assure that shift and/or add sequences that are inserted do not
     accidentally clobber live hard regs.  */
  bitmap regs_live;
};

typedef struct dse_bb_info_type *bb_info_t;

static object_allocator<dse_bb_info_type> dse_bb_info_type_pool
  ("bb_info_pool");

/* Table to hold all bb_infos.  */
static bb_info_t *bb_table;

/* There is a group_info for each rtx base that is used to reference
   memory.  There are also not many of the rtx bases because they are
   very limited in scope.  */

struct group_info
{
  /* The actual base of the address.  */
  rtx rtx_base;

  /* The sequential id of the base.  This allows us to have a
     canonical ordering of these that is not based on addresses.  */
  int id;

  /* True if there are any positions that are to be processed
     globally.  */
  bool process_globally;

  /* True if the base of this group is either the frame_pointer or
     hard_frame_pointer.  */
  bool frame_related;

  /* A mem wrapped around the base pointer for the group in order to do
     read dependency.  It must be given BLKmode in order to encompass all
     the possible offsets from the base.  */
  rtx base_mem;

  /* Canonized version of base_mem's address.  */
  rtx canon_base_addr;

  /* These two sets of two bitmaps are used to keep track of how many
     stores are actually referencing that position from this base.  We
     only do this for rtx bases as this will be used to assign
     positions in the bitmaps for the global problem.  Bit N is set in
     store1 on the first store for offset N.  Bit N is set in store2
     for the second store to offset N.  This is all we need since we
     only care about offsets that have two or more stores for them.

     The "_n" suffix is for offsets less than 0 and the "_p" suffix is
     for 0 and greater offsets.

     There is one special case here, for stores into the stack frame,
     we will or store1 into store2 before deciding which stores look
     at globally.  This is because stores to the stack frame that have
     no other reads before the end of the function can also be
     deleted.  */
  bitmap store1_n, store1_p, store2_n, store2_p;

  /* These bitmaps keep track of offsets in this group escape this function.
     An offset escapes if it corresponds to a named variable whose
     addressable flag is set.  */
  bitmap escaped_n, escaped_p;

  /* The positions in this bitmap have the same assignments as the in,
     out, gen and kill bitmaps.  This bitmap is all zeros except for
     the positions that are occupied by stores for this group.  */
  bitmap group_kill;

  /* The offset_map is used to map the offsets from this base into
     positions in the global bitmaps.  It is only created after all of
     the all of stores have been scanned and we know which ones we
     care about.  */
  int *offset_map_n, *offset_map_p;
  int offset_map_size_n, offset_map_size_p;
};

static object_allocator<group_info> group_info_pool ("rtx_group_info_pool");

/* Index into the rtx_group_vec.  */
static int rtx_group_next_id;


static vec<group_info *> rtx_group_vec;


/* This structure holds the set of changes that are being deferred
   when removing read operation.  See replace_read.  */
struct deferred_change
{

  /* The mem that is being replaced.  */
  rtx *loc;

  /* The reg it is being replaced with.  */
  rtx reg;

  struct deferred_change *next;
};

static object_allocator<deferred_change> deferred_change_pool
  ("deferred_change_pool");

static deferred_change *deferred_change_list = NULL;

/* This is true except if cfun->stdarg -- i.e. we cannot do
   this for vararg functions because they play games with the frame.  */
static bool stores_off_frame_dead_at_return;

/* Counter for stats.  */
static int globally_deleted;
static int locally_deleted;

static bitmap all_blocks;

/* Locations that are killed by calls in the global phase.  */
static bitmap kill_on_calls;

/* The number of bits used in the global bitmaps.  */
static unsigned int current_position;

/*----------------------------------------------------------------------------
   Zeroth step.

   Initialization.
----------------------------------------------------------------------------*/


/* Hashtable callbacks for maintaining the "bases" field of
   store_group_info, given that the addresses are function invariants.  */

struct invariant_group_base_hasher : nofree_ptr_hash <group_info>
{
  static inline hashval_t hash (const group_info *);
  static inline bool equal (const group_info *, const group_info *);
};

inline bool
invariant_group_base_hasher::equal (const group_info *gi1,
				    const group_info *gi2)
{
  return rtx_equal_p (gi1->rtx_base, gi2->rtx_base);
}

inline hashval_t
invariant_group_base_hasher::hash (const group_info *gi)
{
  int do_not_record;
  return hash_rtx (gi->rtx_base, Pmode, &do_not_record, NULL, false);
}

/* Tables of group_info structures, hashed by base value.  */
static hash_table<invariant_group_base_hasher> *rtx_group_table;


/* Get the GROUP for BASE.  Add a new group if it is not there.  */

static group_info *
get_group_info (rtx base)
{
  struct group_info tmp_gi;
  group_info *gi;
  group_info **slot;

  gcc_assert (base != NULL_RTX);

  /* Find the store_base_info structure for BASE, creating a new one
     if necessary.  */
  tmp_gi.rtx_base = base;
  slot = rtx_group_table->find_slot (&tmp_gi, INSERT);
  gi = *slot;

  if (gi == NULL)
    {
      *slot = gi = group_info_pool.allocate ();
      gi->rtx_base = base;
      gi->id = rtx_group_next_id++;
      gi->base_mem = gen_rtx_MEM (BLKmode, base);
      gi->canon_base_addr = canon_rtx (base);
      gi->store1_n = BITMAP_ALLOC (&dse_bitmap_obstack);
      gi->store1_p = BITMAP_ALLOC (&dse_bitmap_obstack);
      gi->store2_n = BITMAP_ALLOC (&dse_bitmap_obstack);
      gi->store2_p = BITMAP_ALLOC (&dse_bitmap_obstack);
      gi->escaped_p = BITMAP_ALLOC (&dse_bitmap_obstack);
      gi->escaped_n = BITMAP_ALLOC (&dse_bitmap_obstack);
      gi->group_kill = BITMAP_ALLOC (&dse_bitmap_obstack);
      gi->process_globally = false;
      gi->frame_related =
	(base == frame_pointer_rtx) || (base == hard_frame_pointer_rtx);
      gi->offset_map_size_n = 0;
      gi->offset_map_size_p = 0;
      gi->offset_map_n = NULL;
      gi->offset_map_p = NULL;
      rtx_group_vec.safe_push (gi);
    }

  return gi;
}


/* Initialization of data structures.  */

static void
dse_step0 (void)
{
  locally_deleted = 0;
  globally_deleted = 0;

  bitmap_obstack_initialize (&dse_bitmap_obstack);
  gcc_obstack_init (&dse_obstack);

  scratch = BITMAP_ALLOC (&reg_obstack);
  kill_on_calls = BITMAP_ALLOC (&dse_bitmap_obstack);


  rtx_group_table = new hash_table<invariant_group_base_hasher> (11);

  bb_table = XNEWVEC (bb_info_t, last_basic_block_for_fn (cfun));
  rtx_group_next_id = 0;

  stores_off_frame_dead_at_return = !cfun->stdarg;

  init_alias_analysis ();
}



/*----------------------------------------------------------------------------
   First step.

   Scan all of the insns.  Any random ordering of the blocks is fine.
   Each block is scanned in forward order to accommodate cselib which
   is used to remove stores with non-constant bases.
----------------------------------------------------------------------------*/

/* Delete all of the store_info recs from INSN_INFO.  */

static void
free_store_info (insn_info_t insn_info)
{
  store_info *cur = insn_info->store_rec;
  while (cur)
    {
      store_info *next = cur->next;
      if (cur->is_large)
	BITMAP_FREE (cur->positions_needed.large.bmap);
      if (cur->cse_base)
	cse_store_info_pool.remove (cur);
      else
	rtx_store_info_pool.remove (cur);
      cur = next;
    }

  insn_info->cannot_delete = true;
  insn_info->contains_cselib_groups = false;
  insn_info->store_rec = NULL;
}

struct note_add_store_info
{
  rtx_insn *first, *current;
  regset fixed_regs_live;
  bool failure;
};

/* Callback for emit_inc_dec_insn_before via note_stores.
   Check if a register is clobbered which is live afterwards.  */

static void
note_add_store (rtx loc, const_rtx expr ATTRIBUTE_UNUSED, void *data)
{
  rtx_insn *insn;
  note_add_store_info *info = (note_add_store_info *) data;

  if (!REG_P (loc))
    return;

  /* If this register is referenced by the current or an earlier insn,
     that's OK.  E.g. this applies to the register that is being incremented
     with this addition.  */
  for (insn = info->first;
       insn != NEXT_INSN (info->current);
       insn = NEXT_INSN (insn))
    if (reg_referenced_p (loc, PATTERN (insn)))
      return;

  /* If we come here, we have a clobber of a register that's only OK
     if that register is not live.  If we don't have liveness information
     available, fail now.  */
  if (!info->fixed_regs_live)
    {
      info->failure = true;
      return;
    }
  /* Now check if this is a live fixed register.  */
  unsigned int end_regno = END_REGNO (loc);
  for (unsigned int regno = REGNO (loc); regno < end_regno; ++regno)
    if (REGNO_REG_SET_P (info->fixed_regs_live, regno))
      info->failure = true;
}

/* Callback for for_each_inc_dec that emits an INSN that sets DEST to
   SRC + SRCOFF before insn ARG.  */

static int
emit_inc_dec_insn_before (rtx mem ATTRIBUTE_UNUSED,
			  rtx op ATTRIBUTE_UNUSED,
			  rtx dest, rtx src, rtx srcoff, void *arg)
{
  insn_info_t insn_info = (insn_info_t) arg;
  rtx_insn *insn = insn_info->insn, *new_insn, *cur;
  note_add_store_info info;

  /* We can reuse all operands without copying, because we are about
     to delete the insn that contained it.  */
  if (srcoff)
    {
      start_sequence ();
      emit_insn (gen_add3_insn (dest, src, srcoff));
      new_insn = get_insns ();
      end_sequence ();
    }
  else
    new_insn = gen_move_insn (dest, src);
  info.first = new_insn;
  info.fixed_regs_live = insn_info->fixed_regs_live;
  info.failure = false;
  for (cur = new_insn; cur; cur = NEXT_INSN (cur))
    {
      info.current = cur;
      note_stores (PATTERN (cur), note_add_store, &info);
    }

  /* If a failure was flagged above, return 1 so that for_each_inc_dec will
     return it immediately, communicating the failure to its caller.  */
  if (info.failure)
    return 1;

  emit_insn_before (new_insn, insn);

  return 0;
}

/* Before we delete INSN_INFO->INSN, make sure that the auto inc/dec, if it
   is there, is split into a separate insn.
   Return true on success (or if there was nothing to do), false on failure.  */

static bool
check_for_inc_dec_1 (insn_info_t insn_info)
{
  rtx_insn *insn = insn_info->insn;
  rtx note = find_reg_note (insn, REG_INC, NULL_RTX);
  if (note)
    return for_each_inc_dec (PATTERN (insn), emit_inc_dec_insn_before,
			     insn_info) == 0;
  return true;
}


/* Entry point for postreload.  If you work on reload_cse, or you need this
   anywhere else, consider if you can provide register liveness information
   and add a parameter to this function so that it can be passed down in
   insn_info.fixed_regs_live.  */
bool
check_for_inc_dec (rtx_insn *insn)
{
  insn_info_type insn_info;
  rtx note;

  insn_info.insn = insn;
  insn_info.fixed_regs_live = NULL;
  note = find_reg_note (insn, REG_INC, NULL_RTX);
  if (note)
    return for_each_inc_dec (PATTERN (insn), emit_inc_dec_insn_before,
			     &insn_info) == 0;
  return true;
}

/* Delete the insn and free all of the fields inside INSN_INFO.  */

static void
delete_dead_store_insn (insn_info_t insn_info)
{
  read_info_t read_info;

  if (!dbg_cnt (dse))
    return;

  if (!check_for_inc_dec_1 (insn_info))
    return;
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Locally deleting insn %d\n",
	     INSN_UID (insn_info->insn));

  free_store_info (insn_info);
  read_info = insn_info->read_rec;

  while (read_info)
    {
      read_info_t next = read_info->next;
      read_info_type_pool.remove (read_info);
      read_info = next;
    }
  insn_info->read_rec = NULL;

  delete_insn (insn_info->insn);
  locally_deleted++;
  insn_info->insn = NULL;

  insn_info->wild_read = false;
}

/* Return whether DECL, a local variable, can possibly escape the current
   function scope.  */

static bool
local_variable_can_escape (tree decl)
{
  if (TREE_ADDRESSABLE (decl))
    return true;

  /* If this is a partitioned variable, we need to consider all the variables
     in the partition.  This is necessary because a store into one of them can
     be replaced with a store into another and this may not change the outcome
     of the escape analysis.  */
  if (cfun->gimple_df->decls_to_pointers != NULL)
    {
      tree *namep = cfun->gimple_df->decls_to_pointers->get (decl);
      if (namep)
	return TREE_ADDRESSABLE (*namep);
    }

  return false;
}

/* Return whether EXPR can possibly escape the current function scope.  */

static bool
can_escape (tree expr)
{
  tree base;
  if (!expr)
    return true;
  base = get_base_address (expr);
  if (DECL_P (base)
      && !may_be_aliased (base)
      && !(TREE_CODE (base) == VAR_DECL
	   && !DECL_EXTERNAL (base)
	   && !TREE_STATIC (base)
	   && local_variable_can_escape (base)))
    return false;
  return true;
}

/* Set the store* bitmaps offset_map_size* fields in GROUP based on
   OFFSET and WIDTH.  */

static void
set_usage_bits (group_info *group, HOST_WIDE_INT offset, HOST_WIDE_INT width,
                tree expr)
{
  HOST_WIDE_INT i;
  bool expr_escapes = can_escape (expr);
  if (offset > -MAX_OFFSET && offset + width < MAX_OFFSET)
    for (i=offset; i<offset+width; i++)
      {
	bitmap store1;
	bitmap store2;
        bitmap escaped;
	int ai;
	if (i < 0)
	  {
	    store1 = group->store1_n;
	    store2 = group->store2_n;
	    escaped = group->escaped_n;
	    ai = -i;
	  }
	else
	  {
	    store1 = group->store1_p;
	    store2 = group->store2_p;
	    escaped = group->escaped_p;
	    ai = i;
	  }

	if (!bitmap_set_bit (store1, ai))
	  bitmap_set_bit (store2, ai);
	else
	  {
	    if (i < 0)
	      {
		if (group->offset_map_size_n < ai)
		  group->offset_map_size_n = ai;
	      }
	    else
	      {
		if (group->offset_map_size_p < ai)
		  group->offset_map_size_p = ai;
	      }
	  }
        if (expr_escapes)
          bitmap_set_bit (escaped, ai);
      }
}

static void
reset_active_stores (void)
{
  active_local_stores = NULL;
  active_local_stores_len = 0;
}

/* Free all READ_REC of the LAST_INSN of BB_INFO.  */

static void
free_read_records (bb_info_t bb_info)
{
  insn_info_t insn_info = bb_info->last_insn;
  read_info_t *ptr = &insn_info->read_rec;
  while (*ptr)
    {
      read_info_t next = (*ptr)->next;
      read_info_type_pool.remove (*ptr);
      *ptr = next;
    }
}

/* Set the BB_INFO so that the last insn is marked as a wild read.  */

static void
add_wild_read (bb_info_t bb_info)
{
  insn_info_t insn_info = bb_info->last_insn;
  insn_info->wild_read = true;
  free_read_records (bb_info);
  reset_active_stores ();
}

/* Set the BB_INFO so that the last insn is marked as a wild read of
   non-frame locations.  */

static void
add_non_frame_wild_read (bb_info_t bb_info)
{
  insn_info_t insn_info = bb_info->last_insn;
  insn_info->non_frame_wild_read = true;
  free_read_records (bb_info);
  reset_active_stores ();
}

/* Return true if X is a constant or one of the registers that behave
   as a constant over the life of a function.  This is equivalent to
   !rtx_varies_p for memory addresses.  */

static bool
const_or_frame_p (rtx x)
{
  if (CONSTANT_P (x))
    return true;

  if (GET_CODE (x) == REG)
    {
      /* Note that we have to test for the actual rtx used for the frame
	 and arg pointers and not just the register number in case we have
	 eliminated the frame and/or arg pointer and are using it
	 for pseudos.  */
      if (x == frame_pointer_rtx || x == hard_frame_pointer_rtx
	  /* The arg pointer varies if it is not a fixed register.  */
	  || (x == arg_pointer_rtx && fixed_regs[ARG_POINTER_REGNUM])
	  || x == pic_offset_table_rtx)
	return true;
      return false;
    }

  return false;
}

/* Take all reasonable action to put the address of MEM into the form
   that we can do analysis on.

   The gold standard is to get the address into the form: address +
   OFFSET where address is something that rtx_varies_p considers a
   constant.  When we can get the address in this form, we can do
   global analysis on it.  Note that for constant bases, address is
   not actually returned, only the group_id.  The address can be
   obtained from that.

   If that fails, we try cselib to get a value we can at least use
   locally.  If that fails we return false.

   The GROUP_ID is set to -1 for cselib bases and the index of the
   group for non_varying bases.

   FOR_READ is true if this is a mem read and false if not.  */

static bool
canon_address (rtx mem,
	       int *group_id,
	       HOST_WIDE_INT *offset,
	       cselib_val **base)
{
  machine_mode address_mode = get_address_mode (mem);
  rtx mem_address = XEXP (mem, 0);
  rtx expanded_address, address;
  int expanded;

  cselib_lookup (mem_address, address_mode, 1, GET_MODE (mem));

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "  mem: ");
      print_inline_rtx (dump_file, mem_address, 0);
      fprintf (dump_file, "\n");
    }

  /* First see if just canon_rtx (mem_address) is const or frame,
     if not, try cselib_expand_value_rtx and call canon_rtx on that.  */
  address = NULL_RTX;
  for (expanded = 0; expanded < 2; expanded++)
    {
      if (expanded)
	{
	  /* Use cselib to replace all of the reg references with the full
	     expression.  This will take care of the case where we have

	     r_x = base + offset;
	     val = *r_x;

	     by making it into

	     val = *(base + offset);  */

	  expanded_address = cselib_expand_value_rtx (mem_address,
						      scratch, 5);

	  /* If this fails, just go with the address from first
	     iteration.  */
	  if (!expanded_address)
	    break;
	}
      else
	expanded_address = mem_address;

      /* Split the address into canonical BASE + OFFSET terms.  */
      address = canon_rtx (expanded_address);

      *offset = 0;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  if (expanded)
	    {
	      fprintf (dump_file, "\n   after cselib_expand address: ");
	      print_inline_rtx (dump_file, expanded_address, 0);
	      fprintf (dump_file, "\n");
	    }

	  fprintf (dump_file, "\n   after canon_rtx address: ");
	  print_inline_rtx (dump_file, address, 0);
	  fprintf (dump_file, "\n");
	}

      if (GET_CODE (address) == CONST)
	address = XEXP (address, 0);

      if (GET_CODE (address) == PLUS
	  && CONST_INT_P (XEXP (address, 1)))
	{
	  *offset = INTVAL (XEXP (address, 1));
	  address = XEXP (address, 0);
	}

      if (ADDR_SPACE_GENERIC_P (MEM_ADDR_SPACE (mem))
	  && const_or_frame_p (address))
	{
	  group_info *group = get_group_info (address);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "  gid=%d offset=%d \n",
		     group->id, (int)*offset);
	  *base = NULL;
	  *group_id = group->id;
	  return true;
	}
    }

  *base = cselib_lookup (address, address_mode, true, GET_MODE (mem));
  *group_id = -1;

  if (*base == NULL)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, " no cselib val - should be a wild read.\n");
      return false;
    }
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "  varying cselib base=%u:%u offset = %d\n",
	     (*base)->uid, (*base)->hash, (int)*offset);
  return true;
}


/* Clear the rhs field from the active_local_stores array.  */

static void
clear_rhs_from_active_local_stores (void)
{
  insn_info_t ptr = active_local_stores;

  while (ptr)
    {
      store_info *store_info = ptr->store_rec;
      /* Skip the clobbers.  */
      while (!store_info->is_set)
	store_info = store_info->next;

      store_info->rhs = NULL;
      store_info->const_rhs = NULL;

      ptr = ptr->next_local_store;
    }
}


/* Mark byte POS bytes from the beginning of store S_INFO as unneeded.  */

static inline void
set_position_unneeded (store_info *s_info, int pos)
{
  if (__builtin_expect (s_info->is_large, false))
    {
      if (bitmap_set_bit (s_info->positions_needed.large.bmap, pos))
	s_info->positions_needed.large.count++;
    }
  else
    s_info->positions_needed.small_bitmask
      &= ~(HOST_WIDE_INT_1U << pos);
}

/* Mark the whole store S_INFO as unneeded.  */

static inline void
set_all_positions_unneeded (store_info *s_info)
{
  if (__builtin_expect (s_info->is_large, false))
    {
      int pos, end = s_info->end - s_info->begin;
      for (pos = 0; pos < end; pos++)
	bitmap_set_bit (s_info->positions_needed.large.bmap, pos);
      s_info->positions_needed.large.count = end;
    }
  else
    s_info->positions_needed.small_bitmask = (unsigned HOST_WIDE_INT) 0;
}

/* Return TRUE if any bytes from S_INFO store are needed.  */

static inline bool
any_positions_needed_p (store_info *s_info)
{
  if (__builtin_expect (s_info->is_large, false))
    return (s_info->positions_needed.large.count
	    < s_info->end - s_info->begin);
  else
    return (s_info->positions_needed.small_bitmask
	    != (unsigned HOST_WIDE_INT) 0);
}

/* Return TRUE if all bytes START through START+WIDTH-1 from S_INFO
   store are needed.  */

static inline bool
all_positions_needed_p (store_info *s_info, int start, int width)
{
  if (__builtin_expect (s_info->is_large, false))
    {
      int end = start + width;
      while (start < end)
	if (bitmap_bit_p (s_info->positions_needed.large.bmap, start++))
	  return false;
      return true;
    }
  else
    {
      unsigned HOST_WIDE_INT mask = lowpart_bitmask (width) << start;
      return (s_info->positions_needed.small_bitmask & mask) == mask;
    }
}


static rtx get_stored_val (store_info *, machine_mode, HOST_WIDE_INT,
			   HOST_WIDE_INT, basic_block, bool);


/* BODY is an instruction pattern that belongs to INSN.  Return 1 if
   there is a candidate store, after adding it to the appropriate
   local store group if so.  */

static int
record_store (rtx body, bb_info_t bb_info)
{
  rtx mem, rhs, const_rhs, mem_addr;
  HOST_WIDE_INT offset = 0;
  HOST_WIDE_INT width = 0;
  insn_info_t insn_info = bb_info->last_insn;
  store_info *store_info = NULL;
  int group_id;
  cselib_val *base = NULL;
  insn_info_t ptr, last, redundant_reason;
  bool store_is_unused;

  if (GET_CODE (body) != SET && GET_CODE (body) != CLOBBER)
    return 0;

  mem = SET_DEST (body);

  /* If this is not used, then this cannot be used to keep the insn
     from being deleted.  On the other hand, it does provide something
     that can be used to prove that another store is dead.  */
  store_is_unused
    = (find_reg_note (insn_info->insn, REG_UNUSED, mem) != NULL);

  /* Check whether that value is a suitable memory location.  */
  if (!MEM_P (mem))
    {
      /* If the set or clobber is unused, then it does not effect our
	 ability to get rid of the entire insn.  */
      if (!store_is_unused)
	insn_info->cannot_delete = true;
      return 0;
    }

  /* At this point we know mem is a mem. */
  if (GET_MODE (mem) == BLKmode)
    {
      if (GET_CODE (XEXP (mem, 0)) == SCRATCH)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, " adding wild read for (clobber (mem:BLK (scratch))\n");
	  add_wild_read (bb_info);
	  insn_info->cannot_delete = true;
	  return 0;
	}
      /* Handle (set (mem:BLK (addr) [... S36 ...]) (const_int 0))
	 as memset (addr, 0, 36);  */
      else if (!MEM_SIZE_KNOWN_P (mem)
	       || MEM_SIZE (mem) <= 0
	       || MEM_SIZE (mem) > MAX_OFFSET
	       || GET_CODE (body) != SET
	       || !CONST_INT_P (SET_SRC (body)))
	{
	  if (!store_is_unused)
	    {
	      /* If the set or clobber is unused, then it does not effect our
		 ability to get rid of the entire insn.  */
	      insn_info->cannot_delete = true;
	      clear_rhs_from_active_local_stores ();
	    }
	  return 0;
	}
    }

  /* We can still process a volatile mem, we just cannot delete it.  */
  if (MEM_VOLATILE_P (mem))
    insn_info->cannot_delete = true;

  if (!canon_address (mem, &group_id, &offset, &base))
    {
      clear_rhs_from_active_local_stores ();
      return 0;
    }

  if (GET_MODE (mem) == BLKmode)
    width = MEM_SIZE (mem);
  else
    width = GET_MODE_SIZE (GET_MODE (mem));

  if (group_id >= 0)
    {
      /* In the restrictive case where the base is a constant or the
	 frame pointer we can do global analysis.  */

      group_info *group
	= rtx_group_vec[group_id];
      tree expr = MEM_EXPR (mem);

      store_info = rtx_store_info_pool.allocate ();
      set_usage_bits (group, offset, width, expr);

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, " processing const base store gid=%d[%d..%d)\n",
		 group_id, (int)offset, (int)(offset+width));
    }
  else
    {
      if (may_be_sp_based_p (XEXP (mem, 0)))
	insn_info->stack_pointer_based = true;
      insn_info->contains_cselib_groups = true;

      store_info = cse_store_info_pool.allocate ();
      group_id = -1;

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, " processing cselib store [%d..%d)\n",
		 (int)offset, (int)(offset+width));
    }

  const_rhs = rhs = NULL_RTX;
  if (GET_CODE (body) == SET
      /* No place to keep the value after ra.  */
      && !reload_completed
      && (REG_P (SET_SRC (body))
	  || GET_CODE (SET_SRC (body)) == SUBREG
	  || CONSTANT_P (SET_SRC (body)))
      && !MEM_VOLATILE_P (mem)
      /* Sometimes the store and reload is used for truncation and
	 rounding.  */
      && !(FLOAT_MODE_P (GET_MODE (mem)) && (flag_float_store)))
    {
      rhs = SET_SRC (body);
      if (CONSTANT_P (rhs))
	const_rhs = rhs;
      else if (body == PATTERN (insn_info->insn))
	{
	  rtx tem = find_reg_note (insn_info->insn, REG_EQUAL, NULL_RTX);
	  if (tem && CONSTANT_P (XEXP (tem, 0)))
	    const_rhs = XEXP (tem, 0);
	}
      if (const_rhs == NULL_RTX && REG_P (rhs))
	{
	  rtx tem = cselib_expand_value_rtx (rhs, scratch, 5);

	  if (tem && CONSTANT_P (tem))
	    const_rhs = tem;
	}
    }

  /* Check to see if this stores causes some other stores to be
     dead.  */
  ptr = active_local_stores;
  last = NULL;
  redundant_reason = NULL;
  mem = canon_rtx (mem);

  if (group_id < 0)
    mem_addr = base->val_rtx;
  else
    {
      group_info *group = rtx_group_vec[group_id];
      mem_addr = group->canon_base_addr;
    }
  if (offset)
    mem_addr = plus_constant (get_address_mode (mem), mem_addr, offset);

  while (ptr)
    {
      insn_info_t next = ptr->next_local_store;
      struct store_info *s_info = ptr->store_rec;
      bool del = true;

      /* Skip the clobbers. We delete the active insn if this insn
	 shadows the set.  To have been put on the active list, it
	 has exactly on set. */
      while (!s_info->is_set)
	s_info = s_info->next;

      if (s_info->group_id == group_id && s_info->cse_base == base)
	{
	  HOST_WIDE_INT i;
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "    trying store in insn=%d gid=%d[%d..%d)\n",
		     INSN_UID (ptr->insn), s_info->group_id,
		     (int)s_info->begin, (int)s_info->end);

	  /* Even if PTR won't be eliminated as unneeded, if both
	     PTR and this insn store the same constant value, we might
	     eliminate this insn instead.  */
	  if (s_info->const_rhs
	      && const_rhs
	      && offset >= s_info->begin
	      && offset + width <= s_info->end
	      && all_positions_needed_p (s_info, offset - s_info->begin,
					 width))
	    {
	      if (GET_MODE (mem) == BLKmode)
		{
		  if (GET_MODE (s_info->mem) == BLKmode
		      && s_info->const_rhs == const_rhs)
		    redundant_reason = ptr;
		}
	      else if (s_info->const_rhs == const0_rtx
		       && const_rhs == const0_rtx)
		redundant_reason = ptr;
	      else
		{
		  rtx val;
		  start_sequence ();
		  val = get_stored_val (s_info, GET_MODE (mem),
					offset, offset + width,
					BLOCK_FOR_INSN (insn_info->insn),
					true);
		  if (get_insns () != NULL)
		    val = NULL_RTX;
		  end_sequence ();
		  if (val && rtx_equal_p (val, const_rhs))
		    redundant_reason = ptr;
		}
	    }

	  for (i = MAX (offset, s_info->begin);
	       i < offset + width && i < s_info->end;
	       i++)
	    set_position_unneeded (s_info, i - s_info->begin);
	}
      else if (s_info->rhs)
	/* Need to see if it is possible for this store to overwrite
	   the value of store_info.  If it is, set the rhs to NULL to
	   keep it from being used to remove a load.  */
	{
	  if (canon_output_dependence (s_info->mem, true,
				       mem, GET_MODE (mem),
				       mem_addr))
	    {
	      s_info->rhs = NULL;
	      s_info->const_rhs = NULL;
	    }
	}

      /* An insn can be deleted if every position of every one of
	 its s_infos is zero.  */
      if (any_positions_needed_p (s_info))
	del = false;

      if (del)
	{
	  insn_info_t insn_to_delete = ptr;

	  active_local_stores_len--;
	  if (last)
	    last->next_local_store = ptr->next_local_store;
	  else
	    active_local_stores = ptr->next_local_store;

	  if (!insn_to_delete->cannot_delete)
	    delete_dead_store_insn (insn_to_delete);
	}
      else
	last = ptr;

      ptr = next;
    }

  /* Finish filling in the store_info.  */
  store_info->next = insn_info->store_rec;
  insn_info->store_rec = store_info;
  store_info->mem = mem;
  store_info->mem_addr = mem_addr;
  store_info->cse_base = base;
  if (width > HOST_BITS_PER_WIDE_INT)
    {
      store_info->is_large = true;
      store_info->positions_needed.large.count = 0;
      store_info->positions_needed.large.bmap = BITMAP_ALLOC (&dse_bitmap_obstack);
    }
  else
    {
      store_info->is_large = false;
      store_info->positions_needed.small_bitmask = lowpart_bitmask (width);
    }
  store_info->group_id = group_id;
  store_info->begin = offset;
  store_info->end = offset + width;
  store_info->is_set = GET_CODE (body) == SET;
  store_info->rhs = rhs;
  store_info->const_rhs = const_rhs;
  store_info->redundant_reason = redundant_reason;

  /* If this is a clobber, we return 0.  We will only be able to
     delete this insn if there is only one store USED store, but we
     can use the clobber to delete other stores earlier.  */
  return store_info->is_set ? 1 : 0;
}


static void
dump_insn_info (const char * start, insn_info_t insn_info)
{
  fprintf (dump_file, "%s insn=%d %s\n", start,
	   INSN_UID (insn_info->insn),
	   insn_info->store_rec ? "has store" : "naked");
}


/* If the modes are different and the value's source and target do not
   line up, we need to extract the value from lower part of the rhs of
   the store, shift it, and then put it into a form that can be shoved
   into the read_insn.  This function generates a right SHIFT of a
   value that is at least ACCESS_SIZE bytes wide of READ_MODE.  The
   shift sequence is returned or NULL if we failed to find a
   shift.  */

static rtx
find_shift_sequence (int access_size,
		     store_info *store_info,
		     machine_mode read_mode,
		     int shift, bool speed, bool require_cst)
{
  machine_mode store_mode = GET_MODE (store_info->mem);
  machine_mode new_mode;
  rtx read_reg = NULL;

  /* Some machines like the x86 have shift insns for each size of
     operand.  Other machines like the ppc or the ia-64 may only have
     shift insns that shift values within 32 or 64 bit registers.
     This loop tries to find the smallest shift insn that will right
     justify the value we want to read but is available in one insn on
     the machine.  */

  for (new_mode = smallest_mode_for_size (access_size * BITS_PER_UNIT,
					  MODE_INT);
       GET_MODE_BITSIZE (new_mode) <= BITS_PER_WORD;
       new_mode = GET_MODE_WIDER_MODE (new_mode))
    {
      rtx target, new_reg, new_lhs;
      rtx_insn *shift_seq, *insn;
      int cost;

      /* If a constant was stored into memory, try to simplify it here,
	 otherwise the cost of the shift might preclude this optimization
	 e.g. at -Os, even when no actual shift will be needed.  */
      if (store_info->const_rhs)
	{
	  unsigned int byte = subreg_lowpart_offset (new_mode, store_mode);
	  rtx ret = simplify_subreg (new_mode, store_info->const_rhs,
				     store_mode, byte);
	  if (ret && CONSTANT_P (ret))
	    {
	      ret = simplify_const_binary_operation (LSHIFTRT, new_mode,
						     ret, GEN_INT (shift));
	      if (ret && CONSTANT_P (ret))
		{
		  byte = subreg_lowpart_offset (read_mode, new_mode);
		  ret = simplify_subreg (read_mode, ret, new_mode, byte);
		  if (ret && CONSTANT_P (ret)
		      && (set_src_cost (ret, read_mode, speed)
			  <= COSTS_N_INSNS (1)))
		    return ret;
		}
	    }
	}

      if (require_cst)
	return NULL_RTX;

      /* Try a wider mode if truncating the store mode to NEW_MODE
	 requires a real instruction.  */
      if (GET_MODE_BITSIZE (new_mode) < GET_MODE_BITSIZE (store_mode)
	  && !TRULY_NOOP_TRUNCATION_MODES_P (new_mode, store_mode))
	continue;

      /* Also try a wider mode if the necessary punning is either not
	 desirable or not possible.  */
      if (!CONSTANT_P (store_info->rhs)
	  && !MODES_TIEABLE_P (new_mode, store_mode))
	continue;

      new_reg = gen_reg_rtx (new_mode);

      start_sequence ();

      /* In theory we could also check for an ashr.  Ian Taylor knows
	 of one dsp where the cost of these two was not the same.  But
	 this really is a rare case anyway.  */
      target = expand_binop (new_mode, lshr_optab, new_reg,
			     GEN_INT (shift), new_reg, 1, OPTAB_DIRECT);

      shift_seq = get_insns ();
      end_sequence ();

      if (target != new_reg || shift_seq == NULL)
	continue;

      cost = 0;
      for (insn = shift_seq; insn != NULL_RTX; insn = NEXT_INSN (insn))
	if (INSN_P (insn))
	  cost += insn_rtx_cost (PATTERN (insn), speed);

      /* The computation up to here is essentially independent
	 of the arguments and could be precomputed.  It may
	 not be worth doing so.  We could precompute if
	 worthwhile or at least cache the results.  The result
	 technically depends on both SHIFT and ACCESS_SIZE,
	 but in practice the answer will depend only on ACCESS_SIZE.  */

      if (cost > COSTS_N_INSNS (1))
	continue;

      new_lhs = extract_low_bits (new_mode, store_mode,
				  copy_rtx (store_info->rhs));
      if (new_lhs == NULL_RTX)
	continue;

      /* We found an acceptable shift.  Generate a move to
	 take the value from the store and put it into the
	 shift pseudo, then shift it, then generate another
	 move to put in into the target of the read.  */
      emit_move_insn (new_reg, new_lhs);
      emit_insn (shift_seq);
      read_reg = extract_low_bits (read_mode, new_mode, new_reg);
      break;
    }

  return read_reg;
}


/* Call back for note_stores to find the hard regs set or clobbered by
   insn.  Data is a bitmap of the hardregs set so far.  */

static void
look_for_hardregs (rtx x, const_rtx pat ATTRIBUTE_UNUSED, void *data)
{
  bitmap regs_set = (bitmap) data;

  if (REG_P (x)
      && HARD_REGISTER_P (x))
    bitmap_set_range (regs_set, REGNO (x), REG_NREGS (x));
}

/* Helper function for replace_read and record_store.
   Attempt to return a value stored in STORE_INFO, from READ_BEGIN
   to one before READ_END bytes read in READ_MODE.  Return NULL
   if not successful.  If REQUIRE_CST is true, return always constant.  */

static rtx
get_stored_val (store_info *store_info, machine_mode read_mode,
		HOST_WIDE_INT read_begin, HOST_WIDE_INT read_end,
		basic_block bb, bool require_cst)
{
  machine_mode store_mode = GET_MODE (store_info->mem);
  int shift;
  int access_size; /* In bytes.  */
  rtx read_reg;

  /* To get here the read is within the boundaries of the write so
     shift will never be negative.  Start out with the shift being in
     bytes.  */
  if (store_mode == BLKmode)
    shift = 0;
  else if (BYTES_BIG_ENDIAN)
    shift = store_info->end - read_end;
  else
    shift = read_begin - store_info->begin;

  access_size = shift + GET_MODE_SIZE (read_mode);

  /* From now on it is bits.  */
  shift *= BITS_PER_UNIT;

  if (shift)
    read_reg = find_shift_sequence (access_size, store_info, read_mode, shift,
    				    optimize_bb_for_speed_p (bb),
				    require_cst);
  else if (store_mode == BLKmode)
    {
      /* The store is a memset (addr, const_val, const_size).  */
      gcc_assert (CONST_INT_P (store_info->rhs));
      store_mode = int_mode_for_mode (read_mode);
      if (store_mode == BLKmode)
	read_reg = NULL_RTX;
      else if (store_info->rhs == const0_rtx)
	read_reg = extract_low_bits (read_mode, store_mode, const0_rtx);
      else if (GET_MODE_BITSIZE (store_mode) > HOST_BITS_PER_WIDE_INT
	       || BITS_PER_UNIT >= HOST_BITS_PER_WIDE_INT)
	read_reg = NULL_RTX;
      else
	{
	  unsigned HOST_WIDE_INT c
	    = INTVAL (store_info->rhs)
	      & ((HOST_WIDE_INT_1 << BITS_PER_UNIT) - 1);
	  int shift = BITS_PER_UNIT;
	  while (shift < HOST_BITS_PER_WIDE_INT)
	    {
	      c |= (c << shift);
	      shift <<= 1;
	    }
	  read_reg = gen_int_mode (c, store_mode);
	  read_reg = extract_low_bits (read_mode, store_mode, read_reg);
	}
    }
  else if (store_info->const_rhs
	   && (require_cst
	       || GET_MODE_CLASS (read_mode) != GET_MODE_CLASS (store_mode)))
    read_reg = extract_low_bits (read_mode, store_mode,
				 copy_rtx (store_info->const_rhs));
  else
    read_reg = extract_low_bits (read_mode, store_mode,
				 copy_rtx (store_info->rhs));
  if (require_cst && read_reg && !CONSTANT_P (read_reg))
    read_reg = NULL_RTX;
  return read_reg;
}

/* Take a sequence of:
     A <- r1
     ...
     ... <- A

   and change it into
   r2 <- r1
   A <- r1
   ...
   ... <- r2

   or

   r3 <- extract (r1)
   r3 <- r3 >> shift
   r2 <- extract (r3)
   ... <- r2

   or

   r2 <- extract (r1)
   ... <- r2

   Depending on the alignment and the mode of the store and
   subsequent load.


   The STORE_INFO and STORE_INSN are for the store and READ_INFO
   and READ_INSN are for the read.  Return true if the replacement
   went ok.  */

static bool
replace_read (store_info *store_info, insn_info_t store_insn,
	      read_info_t read_info, insn_info_t read_insn, rtx *loc,
	      bitmap regs_live)
{
  machine_mode store_mode = GET_MODE (store_info->mem);
  machine_mode read_mode = GET_MODE (read_info->mem);
  rtx_insn *insns, *this_insn;
  rtx read_reg;
  basic_block bb;

  if (!dbg_cnt (dse))
    return false;

  /* Create a sequence of instructions to set up the read register.
     This sequence goes immediately before the store and its result
     is read by the load.

     We need to keep this in perspective.  We are replacing a read
     with a sequence of insns, but the read will almost certainly be
     in cache, so it is not going to be an expensive one.  Thus, we
     are not willing to do a multi insn shift or worse a subroutine
     call to get rid of the read.  */
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "trying to replace %smode load in insn %d"
	     " from %smode store in insn %d\n",
	     GET_MODE_NAME (read_mode), INSN_UID (read_insn->insn),
	     GET_MODE_NAME (store_mode), INSN_UID (store_insn->insn));
  start_sequence ();
  bb = BLOCK_FOR_INSN (read_insn->insn);
  read_reg = get_stored_val (store_info,
			     read_mode, read_info->begin, read_info->end,
			     bb, false);
  if (read_reg == NULL_RTX)
    {
      end_sequence ();
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, " -- could not extract bits of stored value\n");
      return false;
    }
  /* Force the value into a new register so that it won't be clobbered
     between the store and the load.  */
  read_reg = copy_to_mode_reg (read_mode, read_reg);
  insns = get_insns ();
  end_sequence ();

  if (insns != NULL_RTX)
    {
      /* Now we have to scan the set of new instructions to see if the
	 sequence contains and sets of hardregs that happened to be
	 live at this point.  For instance, this can happen if one of
	 the insns sets the CC and the CC happened to be live at that
	 point.  This does occasionally happen, see PR 37922.  */
      bitmap regs_set = BITMAP_ALLOC (&reg_obstack);

      for (this_insn = insns; this_insn != NULL_RTX; this_insn = NEXT_INSN (this_insn))
	note_stores (PATTERN (this_insn), look_for_hardregs, regs_set);

      bitmap_and_into (regs_set, regs_live);
      if (!bitmap_empty_p (regs_set))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file,
		       "abandoning replacement because sequence clobbers live hardregs:");
	      df_print_regset (dump_file, regs_set);
	    }

	  BITMAP_FREE (regs_set);
	  return false;
	}
      BITMAP_FREE (regs_set);
    }

  if (validate_change (read_insn->insn, loc, read_reg, 0))
    {
      deferred_change *change = deferred_change_pool.allocate ();

      /* Insert this right before the store insn where it will be safe
	 from later insns that might change it before the read.  */
      emit_insn_before (insns, store_insn->insn);

      /* And now for the kludge part: cselib croaks if you just
	 return at this point.  There are two reasons for this:

	 1) Cselib has an idea of how many pseudos there are and
	 that does not include the new ones we just added.

	 2) Cselib does not know about the move insn we added
	 above the store_info, and there is no way to tell it
	 about it, because it has "moved on".

	 Problem (1) is fixable with a certain amount of engineering.
	 Problem (2) is requires starting the bb from scratch.  This
	 could be expensive.

	 So we are just going to have to lie.  The move/extraction
	 insns are not really an issue, cselib did not see them.  But
	 the use of the new pseudo read_insn is a real problem because
	 cselib has not scanned this insn.  The way that we solve this
	 problem is that we are just going to put the mem back for now
	 and when we are finished with the block, we undo this.  We
	 keep a table of mems to get rid of.  At the end of the basic
	 block we can put them back.  */

      *loc = read_info->mem;
      change->next = deferred_change_list;
      deferred_change_list = change;
      change->loc = loc;
      change->reg = read_reg;

      /* Get rid of the read_info, from the point of view of the
	 rest of dse, play like this read never happened.  */
      read_insn->read_rec = read_info->next;
      read_info_type_pool.remove (read_info);
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, " -- replaced the loaded MEM with ");
	  print_simple_rtl (dump_file, read_reg);
	  fprintf (dump_file, "\n");
	}
      return true;
    }
  else
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, " -- replacing the loaded MEM with ");
	  print_simple_rtl (dump_file, read_reg);
	  fprintf (dump_file, " led to an invalid instruction\n");
	}
      return false;
    }
}

/* Check the address of MEM *LOC and kill any appropriate stores that may
   be active.  */

static void
check_mem_read_rtx (rtx *loc, bb_info_t bb_info)
{
  rtx mem = *loc, mem_addr;
  insn_info_t insn_info;
  HOST_WIDE_INT offset = 0;
  HOST_WIDE_INT width = 0;
  cselib_val *base = NULL;
  int group_id;
  read_info_t read_info;

  insn_info = bb_info->last_insn;

  if ((MEM_ALIAS_SET (mem) == ALIAS_SET_MEMORY_BARRIER)
      || (MEM_VOLATILE_P (mem)))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, " adding wild read, volatile or barrier.\n");
      add_wild_read (bb_info);
      insn_info->cannot_delete = true;
      return;
    }

  /* If it is reading readonly mem, then there can be no conflict with
     another write. */
  if (MEM_READONLY_P (mem))
    return;

  if (!canon_address (mem, &group_id, &offset, &base))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, " adding wild read, canon_address failure.\n");
      add_wild_read (bb_info);
      return;
    }

  if (GET_MODE (mem) == BLKmode)
    width = -1;
  else
    width = GET_MODE_SIZE (GET_MODE (mem));

  read_info = read_info_type_pool.allocate ();
  read_info->group_id = group_id;
  read_info->mem = mem;
  read_info->begin = offset;
  read_info->end = offset + width;
  read_info->next = insn_info->read_rec;
  insn_info->read_rec = read_info;
  if (group_id < 0)
    mem_addr = base->val_rtx;
  else
    {
      group_info *group = rtx_group_vec[group_id];
      mem_addr = group->canon_base_addr;
    }
  if (offset)
    mem_addr = plus_constant (get_address_mode (mem), mem_addr, offset);

  if (group_id >= 0)
    {
      /* This is the restricted case where the base is a constant or
	 the frame pointer and offset is a constant.  */
      insn_info_t i_ptr = active_local_stores;
      insn_info_t last = NULL;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  if (width == -1)
	    fprintf (dump_file, " processing const load gid=%d[BLK]\n",
		     group_id);
	  else
	    fprintf (dump_file, " processing const load gid=%d[%d..%d)\n",
		     group_id, (int)offset, (int)(offset+width));
	}

      while (i_ptr)
	{
	  bool remove = false;
	  store_info *store_info = i_ptr->store_rec;

	  /* Skip the clobbers.  */
	  while (!store_info->is_set)
	    store_info = store_info->next;

	  /* There are three cases here.  */
	  if (store_info->group_id < 0)
	    /* We have a cselib store followed by a read from a
	       const base. */
	    remove
	      = canon_true_dependence (store_info->mem,
				       GET_MODE (store_info->mem),
				       store_info->mem_addr,
				       mem, mem_addr);

	  else if (group_id == store_info->group_id)
	    {
	      /* This is a block mode load.  We may get lucky and
		 canon_true_dependence may save the day.  */
	      if (width == -1)
		remove
		  = canon_true_dependence (store_info->mem,
					   GET_MODE (store_info->mem),
					   store_info->mem_addr,
					   mem, mem_addr);

	      /* If this read is just reading back something that we just
		 stored, rewrite the read.  */
	      else
		{
		  if (store_info->rhs
		      && offset >= store_info->begin
		      && offset + width <= store_info->end
		      && all_positions_needed_p (store_info,
						 offset - store_info->begin,
						 width)
		      && replace_read (store_info, i_ptr, read_info,
				       insn_info, loc, bb_info->regs_live))
		    return;

		  /* The bases are the same, just see if the offsets
		     overlap.  */
		  if ((offset < store_info->end)
		      && (offset + width > store_info->begin))
		    remove = true;
		}
	    }

	  /* else
	     The else case that is missing here is that the
	     bases are constant but different.  There is nothing
	     to do here because there is no overlap.  */

	  if (remove)
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		dump_insn_info ("removing from active", i_ptr);

	      active_local_stores_len--;
	      if (last)
		last->next_local_store = i_ptr->next_local_store;
	      else
		active_local_stores = i_ptr->next_local_store;
	    }
	  else
	    last = i_ptr;
	  i_ptr = i_ptr->next_local_store;
	}
    }
  else
    {
      insn_info_t i_ptr = active_local_stores;
      insn_info_t last = NULL;
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, " processing cselib load mem:");
	  print_inline_rtx (dump_file, mem, 0);
	  fprintf (dump_file, "\n");
	}

      while (i_ptr)
	{
	  bool remove = false;
	  store_info *store_info = i_ptr->store_rec;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, " processing cselib load against insn %d\n",
		     INSN_UID (i_ptr->insn));

	  /* Skip the clobbers.  */
	  while (!store_info->is_set)
	    store_info = store_info->next;

	  /* If this read is just reading back something that we just
	     stored, rewrite the read.  */
	  if (store_info->rhs
	      && store_info->group_id == -1
	      && store_info->cse_base == base
	      && width != -1
	      && offset >= store_info->begin
	      && offset + width <= store_info->end
	      && all_positions_needed_p (store_info,
					 offset - store_info->begin, width)
	      && replace_read (store_info, i_ptr,  read_info, insn_info, loc,
			       bb_info->regs_live))
	    return;

	  remove = canon_true_dependence (store_info->mem,
					  GET_MODE (store_info->mem),
					  store_info->mem_addr,
					  mem, mem_addr);

	  if (remove)
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		dump_insn_info ("removing from active", i_ptr);

	      active_local_stores_len--;
	      if (last)
		last->next_local_store = i_ptr->next_local_store;
	      else
		active_local_stores = i_ptr->next_local_store;
	    }
	  else
	    last = i_ptr;
	  i_ptr = i_ptr->next_local_store;
	}
    }
}

/* A note_uses callback in which DATA points the INSN_INFO for
   as check_mem_read_rtx.  Nullify the pointer if i_m_r_m_r returns
   true for any part of *LOC.  */

static void
check_mem_read_use (rtx *loc, void *data)
{
  subrtx_ptr_iterator::array_type array;
  FOR_EACH_SUBRTX_PTR (iter, array, loc, NONCONST)
    {
      rtx *loc = *iter;
      if (MEM_P (*loc))
	check_mem_read_rtx (loc, (bb_info_t) data);
    }
}


/* Get arguments passed to CALL_INSN.  Return TRUE if successful.
   So far it only handles arguments passed in registers.  */

static bool
get_call_args (rtx call_insn, tree fn, rtx *args, int nargs)
{
  CUMULATIVE_ARGS args_so_far_v;
  cumulative_args_t args_so_far;
  tree arg;
  int idx;

  INIT_CUMULATIVE_ARGS (args_so_far_v, TREE_TYPE (fn), NULL_RTX, 0, 3);
  args_so_far = pack_cumulative_args (&args_so_far_v);

  arg = TYPE_ARG_TYPES (TREE_TYPE (fn));
  for (idx = 0;
       arg != void_list_node && idx < nargs;
       arg = TREE_CHAIN (arg), idx++)
    {
      machine_mode mode = TYPE_MODE (TREE_VALUE (arg));
      rtx reg, link, tmp;
      reg = targetm.calls.function_arg (args_so_far, mode, NULL_TREE, true);
      if (!reg || !REG_P (reg) || GET_MODE (reg) != mode
	  || GET_MODE_CLASS (mode) != MODE_INT)
	return false;

      for (link = CALL_INSN_FUNCTION_USAGE (call_insn);
	   link;
	   link = XEXP (link, 1))
	if (GET_CODE (XEXP (link, 0)) == USE)
	  {
	    args[idx] = XEXP (XEXP (link, 0), 0);
	    if (REG_P (args[idx])
		&& REGNO (args[idx]) == REGNO (reg)
		&& (GET_MODE (args[idx]) == mode
		    || (GET_MODE_CLASS (GET_MODE (args[idx])) == MODE_INT
			&& (GET_MODE_SIZE (GET_MODE (args[idx]))
			    <= UNITS_PER_WORD)
			&& (GET_MODE_SIZE (GET_MODE (args[idx]))
			    > GET_MODE_SIZE (mode)))))
	      break;
	  }
      if (!link)
	return false;

      tmp = cselib_expand_value_rtx (args[idx], scratch, 5);
      if (GET_MODE (args[idx]) != mode)
	{
	  if (!tmp || !CONST_INT_P (tmp))
	    return false;
	  tmp = gen_int_mode (INTVAL (tmp), mode);
	}
      if (tmp)
	args[idx] = tmp;

      targetm.calls.function_arg_advance (args_so_far, mode, NULL_TREE, true);
    }
  if (arg != void_list_node || idx != nargs)
    return false;
  return true;
}

/* Return a bitmap of the fixed registers contained in IN.  */

static bitmap
copy_fixed_regs (const_bitmap in)
{
  bitmap ret;

  ret = ALLOC_REG_SET (NULL);
  bitmap_and (ret, in, fixed_reg_set_regset);
  return ret;
}

/* Apply record_store to all candidate stores in INSN.  Mark INSN
   if some part of it is not a candidate store and assigns to a
   non-register target.  */

static void
scan_insn (bb_info_t bb_info, rtx_insn *insn)
{
  rtx body;
  insn_info_type *insn_info = insn_info_type_pool.allocate ();
  int mems_found = 0;
  memset (insn_info, 0, sizeof (struct insn_info_type));

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\n**scanning insn=%d\n",
	     INSN_UID (insn));

  insn_info->prev_insn = bb_info->last_insn;
  insn_info->insn = insn;
  bb_info->last_insn = insn_info;

  if (DEBUG_INSN_P (insn))
    {
      insn_info->cannot_delete = true;
      return;
    }

  /* Look at all of the uses in the insn.  */
  note_uses (&PATTERN (insn), check_mem_read_use, bb_info);

  if (CALL_P (insn))
    {
      bool const_call;
      rtx call, sym;
      tree memset_call = NULL_TREE;

      insn_info->cannot_delete = true;

      /* Const functions cannot do anything bad i.e. read memory,
	 however, they can read their parameters which may have
	 been pushed onto the stack.
	 memset and bzero don't read memory either.  */
      const_call = RTL_CONST_CALL_P (insn);
      if (!const_call
	  && (call = get_call_rtx_from (insn))
	  && (sym = XEXP (XEXP (call, 0), 0))
	  && GET_CODE (sym) == SYMBOL_REF
	  && SYMBOL_REF_DECL (sym)
	  && TREE_CODE (SYMBOL_REF_DECL (sym)) == FUNCTION_DECL
	  && DECL_BUILT_IN_CLASS (SYMBOL_REF_DECL (sym)) == BUILT_IN_NORMAL
	  && DECL_FUNCTION_CODE (SYMBOL_REF_DECL (sym)) == BUILT_IN_MEMSET)
	memset_call = SYMBOL_REF_DECL (sym);

      if (const_call || memset_call)
	{
	  insn_info_t i_ptr = active_local_stores;
	  insn_info_t last = NULL;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "%s call %d\n",
		     const_call ? "const" : "memset", INSN_UID (insn));

	  /* See the head comment of the frame_read field.  */
	  if (reload_completed
	      /* Tail calls are storing their arguments using
		 arg pointer.  If it is a frame pointer on the target,
		 even before reload we need to kill frame pointer based
		 stores.  */
	      || (SIBLING_CALL_P (insn)
		  && HARD_FRAME_POINTER_IS_ARG_POINTER))
	    insn_info->frame_read = true;

	  /* Loop over the active stores and remove those which are
	     killed by the const function call.  */
	  while (i_ptr)
	    {
	      bool remove_store = false;

	      /* The stack pointer based stores are always killed.  */
	      if (i_ptr->stack_pointer_based)
	        remove_store = true;

	      /* If the frame is read, the frame related stores are killed.  */
	      else if (insn_info->frame_read)
		{
		  store_info *store_info = i_ptr->store_rec;

		  /* Skip the clobbers.  */
		  while (!store_info->is_set)
		    store_info = store_info->next;

		  if (store_info->group_id >= 0
		      && rtx_group_vec[store_info->group_id]->frame_related)
		    remove_store = true;
		}

	      if (remove_store)
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    dump_insn_info ("removing from active", i_ptr);

		  active_local_stores_len--;
		  if (last)
		    last->next_local_store = i_ptr->next_local_store;
		  else
		    active_local_stores = i_ptr->next_local_store;
		}
	      else
		last = i_ptr;

	      i_ptr = i_ptr->next_local_store;
	    }

	  if (memset_call)
	    {
	      rtx args[3];
	      if (get_call_args (insn, memset_call, args, 3)
		  && CONST_INT_P (args[1])
		  && CONST_INT_P (args[2])
		  && INTVAL (args[2]) > 0)
		{
		  rtx mem = gen_rtx_MEM (BLKmode, args[0]);
		  set_mem_size (mem, INTVAL (args[2]));
		  body = gen_rtx_SET (mem, args[1]);
		  mems_found += record_store (body, bb_info);
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "handling memset as BLKmode store\n");
		  if (mems_found == 1)
		    {
		      if (active_local_stores_len++
			  >= PARAM_VALUE (PARAM_MAX_DSE_ACTIVE_LOCAL_STORES))
			{
			  active_local_stores_len = 1;
			  active_local_stores = NULL;
			}
		      insn_info->fixed_regs_live
			= copy_fixed_regs (bb_info->regs_live);
		      insn_info->next_local_store = active_local_stores;
		      active_local_stores = insn_info;
		    }
		}
	      else
		clear_rhs_from_active_local_stores ();
	    }
	}
      else if (SIBLING_CALL_P (insn) && reload_completed)
	/* Arguments for a sibling call that are pushed to memory are passed
	   using the incoming argument pointer of the current function.  After
	   reload that might be (and likely is) frame pointer based.  */
	add_wild_read (bb_info);
      else
	/* Every other call, including pure functions, may read any memory
           that is not relative to the frame.  */
        add_non_frame_wild_read (bb_info);

      return;
    }

  /* Assuming that there are sets in these insns, we cannot delete
     them.  */
  if ((GET_CODE (PATTERN (insn)) == CLOBBER)
      || volatile_refs_p (PATTERN (insn))
      || (!cfun->can_delete_dead_exceptions && !insn_nothrow_p (insn))
      || (RTX_FRAME_RELATED_P (insn))
      || find_reg_note (insn, REG_FRAME_RELATED_EXPR, NULL_RTX))
    insn_info->cannot_delete = true;

  body = PATTERN (insn);
  if (GET_CODE (body) == PARALLEL)
    {
      int i;
      for (i = 0; i < XVECLEN (body, 0); i++)
	mems_found += record_store (XVECEXP (body, 0, i), bb_info);
    }
  else
    mems_found += record_store (body, bb_info);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "mems_found = %d, cannot_delete = %s\n",
	     mems_found, insn_info->cannot_delete ? "true" : "false");

  /* If we found some sets of mems, add it into the active_local_stores so
     that it can be locally deleted if found dead or used for
     replace_read and redundant constant store elimination.  Otherwise mark
     it as cannot delete.  This simplifies the processing later.  */
  if (mems_found == 1)
    {
      if (active_local_stores_len++
	  >= PARAM_VALUE (PARAM_MAX_DSE_ACTIVE_LOCAL_STORES))
	{
	  active_local_stores_len = 1;
	  active_local_stores = NULL;
	}
      insn_info->fixed_regs_live = copy_fixed_regs (bb_info->regs_live);
      insn_info->next_local_store = active_local_stores;
      active_local_stores = insn_info;
    }
  else
    insn_info->cannot_delete = true;
}


/* Remove BASE from the set of active_local_stores.  This is a
   callback from cselib that is used to get rid of the stores in
   active_local_stores.  */

static void
remove_useless_values (cselib_val *base)
{
  insn_info_t insn_info = active_local_stores;
  insn_info_t last = NULL;

  while (insn_info)
    {
      store_info *store_info = insn_info->store_rec;
      bool del = false;

      /* If ANY of the store_infos match the cselib group that is
	 being deleted, then the insn can not be deleted.  */
      while (store_info)
	{
	  if ((store_info->group_id == -1)
	      && (store_info->cse_base == base))
	    {
	      del = true;
	      break;
	    }
	  store_info = store_info->next;
	}

      if (del)
	{
	  active_local_stores_len--;
	  if (last)
	    last->next_local_store = insn_info->next_local_store;
	  else
	    active_local_stores = insn_info->next_local_store;
	  free_store_info (insn_info);
	}
      else
	last = insn_info;

      insn_info = insn_info->next_local_store;
    }
}


/* Do all of step 1.  */

static void
dse_step1 (void)
{
  basic_block bb;
  bitmap regs_live = BITMAP_ALLOC (&reg_obstack);

  cselib_init (0);
  all_blocks = BITMAP_ALLOC (NULL);
  bitmap_set_bit (all_blocks, ENTRY_BLOCK);
  bitmap_set_bit (all_blocks, EXIT_BLOCK);

  FOR_ALL_BB_FN (bb, cfun)
    {
      insn_info_t ptr;
      bb_info_t bb_info = dse_bb_info_type_pool.allocate ();

      memset (bb_info, 0, sizeof (dse_bb_info_type));
      bitmap_set_bit (all_blocks, bb->index);
      bb_info->regs_live = regs_live;

      bitmap_copy (regs_live, DF_LR_IN (bb));
      df_simulate_initialize_forwards (bb, regs_live);

      bb_table[bb->index] = bb_info;
      cselib_discard_hook = remove_useless_values;

      if (bb->index >= NUM_FIXED_BLOCKS)
	{
	  rtx_insn *insn;

	  active_local_stores = NULL;
	  active_local_stores_len = 0;
	  cselib_clear_table ();

	  /* Scan the insns.  */
	  FOR_BB_INSNS (bb, insn)
	    {
	      if (INSN_P (insn))
		scan_insn (bb_info, insn);
	      cselib_process_insn (insn);
	      if (INSN_P (insn))
		df_simulate_one_insn_forwards (bb, insn, regs_live);
	    }

	  /* This is something of a hack, because the global algorithm
	     is supposed to take care of the case where stores go dead
	     at the end of the function.  However, the global
	     algorithm must take a more conservative view of block
	     mode reads than the local alg does.  So to get the case
	     where you have a store to the frame followed by a non
	     overlapping block more read, we look at the active local
	     stores at the end of the function and delete all of the
	     frame and spill based ones.  */
	  if (stores_off_frame_dead_at_return
	      && (EDGE_COUNT (bb->succs) == 0
		  || (single_succ_p (bb)
		      && single_succ (bb) == EXIT_BLOCK_PTR_FOR_FN (cfun)
		      && ! crtl->calls_eh_return)))
	    {
	      insn_info_t i_ptr = active_local_stores;
	      while (i_ptr)
		{
		  store_info *store_info = i_ptr->store_rec;

		  /* Skip the clobbers.  */
		  while (!store_info->is_set)
		    store_info = store_info->next;
		  if (store_info->group_id >= 0)
		    {
		      group_info *group = rtx_group_vec[store_info->group_id];
		      if (group->frame_related && !i_ptr->cannot_delete)
			delete_dead_store_insn (i_ptr);
		    }

		  i_ptr = i_ptr->next_local_store;
		}
	    }

	  /* Get rid of the loads that were discovered in
	     replace_read.  Cselib is finished with this block.  */
	  while (deferred_change_list)
	    {
	      deferred_change *next = deferred_change_list->next;

	      /* There is no reason to validate this change.  That was
		 done earlier.  */
	      *deferred_change_list->loc = deferred_change_list->reg;
	      deferred_change_pool.remove (deferred_change_list);
	      deferred_change_list = next;
	    }

	  /* Get rid of all of the cselib based store_infos in this
	     block and mark the containing insns as not being
	     deletable.  */
	  ptr = bb_info->last_insn;
	  while (ptr)
	    {
	      if (ptr->contains_cselib_groups)
		{
		  store_info *s_info = ptr->store_rec;
		  while (s_info && !s_info->is_set)
		    s_info = s_info->next;
		  if (s_info
		      && s_info->redundant_reason
		      && s_info->redundant_reason->insn
		      && !ptr->cannot_delete)
		    {
		      if (dump_file && (dump_flags & TDF_DETAILS))
			fprintf (dump_file, "Locally deleting insn %d "
					    "because insn %d stores the "
					    "same value and couldn't be "
					    "eliminated\n",
				 INSN_UID (ptr->insn),
				 INSN_UID (s_info->redundant_reason->insn));
		      delete_dead_store_insn (ptr);
		    }
		  free_store_info (ptr);
		}
	      else
		{
		  store_info *s_info;

		  /* Free at least positions_needed bitmaps.  */
		  for (s_info = ptr->store_rec; s_info; s_info = s_info->next)
		    if (s_info->is_large)
		      {
			BITMAP_FREE (s_info->positions_needed.large.bmap);
			s_info->is_large = false;
		      }
		}
	      ptr = ptr->prev_insn;
	    }

	  cse_store_info_pool.release ();
	}
      bb_info->regs_live = NULL;
    }

  BITMAP_FREE (regs_live);
  cselib_finish ();
  rtx_group_table->empty ();
}


/*----------------------------------------------------------------------------
   Second step.

   Assign each byte position in the stores that we are going to
   analyze globally to a position in the bitmaps.  Returns true if
   there are any bit positions assigned.
----------------------------------------------------------------------------*/

static void
dse_step2_init (void)
{
  unsigned int i;
  group_info *group;

  FOR_EACH_VEC_ELT (rtx_group_vec, i, group)
    {
      /* For all non stack related bases, we only consider a store to
	 be deletable if there are two or more stores for that
	 position.  This is because it takes one store to make the
	 other store redundant.  However, for the stores that are
	 stack related, we consider them if there is only one store
	 for the position.  We do this because the stack related
	 stores can be deleted if their is no read between them and
	 the end of the function.

	 To make this work in the current framework, we take the stack
	 related bases add all of the bits from store1 into store2.
	 This has the effect of making the eligible even if there is
	 only one store.   */

      if (stores_off_frame_dead_at_return && group->frame_related)
	{
	  bitmap_ior_into (group->store2_n, group->store1_n);
	  bitmap_ior_into (group->store2_p, group->store1_p);
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "group %d is frame related ", i);
	}

      group->offset_map_size_n++;
      group->offset_map_n = XOBNEWVEC (&dse_obstack, int,
				       group->offset_map_size_n);
      group->offset_map_size_p++;
      group->offset_map_p = XOBNEWVEC (&dse_obstack, int,
				       group->offset_map_size_p);
      group->process_globally = false;
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "group %d(%d+%d): ", i,
		   (int)bitmap_count_bits (group->store2_n),
		   (int)bitmap_count_bits (group->store2_p));
	  bitmap_print (dump_file, group->store2_n, "n ", " ");
	  bitmap_print (dump_file, group->store2_p, "p ", "\n");
	}
    }
}


/* Init the offset tables.  */

static bool
dse_step2 (void)
{
  unsigned int i;
  group_info *group;
  /* Position 0 is unused because 0 is used in the maps to mean
     unused.  */
  current_position = 1;
  FOR_EACH_VEC_ELT (rtx_group_vec, i, group)
    {
      bitmap_iterator bi;
      unsigned int j;

      memset (group->offset_map_n, 0, sizeof (int) * group->offset_map_size_n);
      memset (group->offset_map_p, 0, sizeof (int) * group->offset_map_size_p);
      bitmap_clear (group->group_kill);

      EXECUTE_IF_SET_IN_BITMAP (group->store2_n, 0, j, bi)
	{
	  bitmap_set_bit (group->group_kill, current_position);
          if (bitmap_bit_p (group->escaped_n, j))
	    bitmap_set_bit (kill_on_calls, current_position);
	  group->offset_map_n[j] = current_position++;
	  group->process_globally = true;
	}
      EXECUTE_IF_SET_IN_BITMAP (group->store2_p, 0, j, bi)
	{
	  bitmap_set_bit (group->group_kill, current_position);
          if (bitmap_bit_p (group->escaped_p, j))
	    bitmap_set_bit (kill_on_calls, current_position);
	  group->offset_map_p[j] = current_position++;
	  group->process_globally = true;
	}
    }
  return current_position != 1;
}



/*----------------------------------------------------------------------------
  Third step.

  Build the bit vectors for the transfer functions.
----------------------------------------------------------------------------*/


/* Look up the bitmap index for OFFSET in GROUP_INFO.  If it is not
   there, return 0.  */

static int
get_bitmap_index (group_info *group_info, HOST_WIDE_INT offset)
{
  if (offset < 0)
    {
      HOST_WIDE_INT offset_p = -offset;
      if (offset_p >= group_info->offset_map_size_n)
	return 0;
      return group_info->offset_map_n[offset_p];
    }
  else
    {
      if (offset >= group_info->offset_map_size_p)
	return 0;
      return group_info->offset_map_p[offset];
    }
}


/* Process the STORE_INFOs into the bitmaps into GEN and KILL.  KILL
   may be NULL. */

static void
scan_stores (store_info *store_info, bitmap gen, bitmap kill)
{
  while (store_info)
    {
      HOST_WIDE_INT i;
      group_info *group_info
	= rtx_group_vec[store_info->group_id];
      if (group_info->process_globally)
	for (i = store_info->begin; i < store_info->end; i++)
	  {
	    int index = get_bitmap_index (group_info, i);
	    if (index != 0)
	      {
		bitmap_set_bit (gen, index);
		if (kill)
		  bitmap_clear_bit (kill, index);
	      }
	  }
      store_info = store_info->next;
    }
}


/* Process the READ_INFOs into the bitmaps into GEN and KILL.  KILL
   may be NULL.  */

static void
scan_reads (insn_info_t insn_info, bitmap gen, bitmap kill)
{
  read_info_t read_info = insn_info->read_rec;
  int i;
  group_info *group;

  /* If this insn reads the frame, kill all the frame related stores.  */
  if (insn_info->frame_read)
    {
      FOR_EACH_VEC_ELT (rtx_group_vec, i, group)
	if (group->process_globally && group->frame_related)
	  {
	    if (kill)
	      bitmap_ior_into (kill, group->group_kill);
	    bitmap_and_compl_into (gen, group->group_kill);
	  }
    }
  if (insn_info->non_frame_wild_read)
    {
      /* Kill all non-frame related stores.  Kill all stores of variables that
         escape.  */
      if (kill)
        bitmap_ior_into (kill, kill_on_calls);
      bitmap_and_compl_into (gen, kill_on_calls);
      FOR_EACH_VEC_ELT (rtx_group_vec, i, group)
	if (group->process_globally && !group->frame_related)
	  {
	    if (kill)
	      bitmap_ior_into (kill, group->group_kill);
	    bitmap_and_compl_into (gen, group->group_kill);
	  }
    }
  while (read_info)
    {
      FOR_EACH_VEC_ELT (rtx_group_vec, i, group)
	{
	  if (group->process_globally)
	    {
	      if (i == read_info->group_id)
		{
		  if (read_info->begin > read_info->end)
		    {
		      /* Begin > end for block mode reads.  */
		      if (kill)
			bitmap_ior_into (kill, group->group_kill);
		      bitmap_and_compl_into (gen, group->group_kill);
		    }
		  else
		    {
		      /* The groups are the same, just process the
			 offsets.  */
		      HOST_WIDE_INT j;
		      for (j = read_info->begin; j < read_info->end; j++)
			{
			  int index = get_bitmap_index (group, j);
			  if (index != 0)
			    {
			      if (kill)
				bitmap_set_bit (kill, index);
			      bitmap_clear_bit (gen, index);
			    }
			}
		    }
		}
	      else
		{
		  /* The groups are different, if the alias sets
		     conflict, clear the entire group.  We only need
		     to apply this test if the read_info is a cselib
		     read.  Anything with a constant base cannot alias
		     something else with a different constant
		     base.  */
		  if ((read_info->group_id < 0)
		      && canon_true_dependence (group->base_mem,
						GET_MODE (group->base_mem),
						group->canon_base_addr,
						read_info->mem, NULL_RTX))
		    {
		      if (kill)
			bitmap_ior_into (kill, group->group_kill);
		      bitmap_and_compl_into (gen, group->group_kill);
		    }
		}
	    }
	}

      read_info = read_info->next;
    }
}


/* Return the insn in BB_INFO before the first wild read or if there
   are no wild reads in the block, return the last insn.  */

static insn_info_t
find_insn_before_first_wild_read (bb_info_t bb_info)
{
  insn_info_t insn_info = bb_info->last_insn;
  insn_info_t last_wild_read = NULL;

  while (insn_info)
    {
      if (insn_info->wild_read)
	{
	  last_wild_read = insn_info->prev_insn;
	  /* Block starts with wild read.  */
	  if (!last_wild_read)
	    return NULL;
	}

      insn_info = insn_info->prev_insn;
    }

  if (last_wild_read)
    return last_wild_read;
  else
    return bb_info->last_insn;
}


/* Scan the insns in BB_INFO starting at PTR and going to the top of
   the block in order to build the gen and kill sets for the block.
   We start at ptr which may be the last insn in the block or may be
   the first insn with a wild read.  In the latter case we are able to
   skip the rest of the block because it just does not matter:
   anything that happens is hidden by the wild read.  */

static void
dse_step3_scan (basic_block bb)
{
  bb_info_t bb_info = bb_table[bb->index];
  insn_info_t insn_info;

  insn_info = find_insn_before_first_wild_read (bb_info);

  /* In the spill case or in the no_spill case if there is no wild
     read in the block, we will need a kill set.  */
  if (insn_info == bb_info->last_insn)
    {
      if (bb_info->kill)
	bitmap_clear (bb_info->kill);
      else
	bb_info->kill = BITMAP_ALLOC (&dse_bitmap_obstack);
    }
  else
    if (bb_info->kill)
      BITMAP_FREE (bb_info->kill);

  while (insn_info)
    {
      /* There may have been code deleted by the dce pass run before
	 this phase.  */
      if (insn_info->insn && INSN_P (insn_info->insn))
	{
	  scan_stores (insn_info->store_rec, bb_info->gen, bb_info->kill);
	  scan_reads (insn_info, bb_info->gen, bb_info->kill);
	}

      insn_info = insn_info->prev_insn;
    }
}


/* Set the gen set of the exit block, and also any block with no
   successors that does not have a wild read.  */

static void
dse_step3_exit_block_scan (bb_info_t bb_info)
{
  /* The gen set is all 0's for the exit block except for the
     frame_pointer_group.  */

  if (stores_off_frame_dead_at_return)
    {
      unsigned int i;
      group_info *group;

      FOR_EACH_VEC_ELT (rtx_group_vec, i, group)
	{
	  if (group->process_globally && group->frame_related)
	    bitmap_ior_into (bb_info->gen, group->group_kill);
	}
    }
}


/* Find all of the blocks that are not backwards reachable from the
   exit block or any block with no successors (BB).  These are the
   infinite loops or infinite self loops.  These blocks will still
   have their bits set in UNREACHABLE_BLOCKS.  */

static void
mark_reachable_blocks (sbitmap unreachable_blocks, basic_block bb)
{
  edge e;
  edge_iterator ei;

  if (bitmap_bit_p (unreachable_blocks, bb->index))
    {
      bitmap_clear_bit (unreachable_blocks, bb->index);
      FOR_EACH_EDGE (e, ei, bb->preds)
	{
	  mark_reachable_blocks (unreachable_blocks, e->src);
	}
    }
}

/* Build the transfer functions for the function.  */

static void
dse_step3 ()
{
  basic_block bb;
  sbitmap unreachable_blocks = sbitmap_alloc (last_basic_block_for_fn (cfun));
  sbitmap_iterator sbi;
  bitmap all_ones = NULL;
  unsigned int i;

  bitmap_ones (unreachable_blocks);

  FOR_ALL_BB_FN (bb, cfun)
    {
      bb_info_t bb_info = bb_table[bb->index];
      if (bb_info->gen)
	bitmap_clear (bb_info->gen);
      else
	bb_info->gen = BITMAP_ALLOC (&dse_bitmap_obstack);

      if (bb->index == ENTRY_BLOCK)
	;
      else if (bb->index == EXIT_BLOCK)
	dse_step3_exit_block_scan (bb_info);
      else
	dse_step3_scan (bb);
      if (EDGE_COUNT (bb->succs) == 0)
	mark_reachable_blocks (unreachable_blocks, bb);

      /* If this is the second time dataflow is run, delete the old
	 sets.  */
      if (bb_info->in)
	BITMAP_FREE (bb_info->in);
      if (bb_info->out)
	BITMAP_FREE (bb_info->out);
    }

  /* For any block in an infinite loop, we must initialize the out set
     to all ones.  This could be expensive, but almost never occurs in
     practice. However, it is common in regression tests.  */
  EXECUTE_IF_SET_IN_BITMAP (unreachable_blocks, 0, i, sbi)
    {
      if (bitmap_bit_p (all_blocks, i))
	{
	  bb_info_t bb_info = bb_table[i];
	  if (!all_ones)
	    {
	      unsigned int j;
	      group_info *group;

	      all_ones = BITMAP_ALLOC (&dse_bitmap_obstack);
	      FOR_EACH_VEC_ELT (rtx_group_vec, j, group)
		bitmap_ior_into (all_ones, group->group_kill);
	    }
	  if (!bb_info->out)
	    {
	      bb_info->out = BITMAP_ALLOC (&dse_bitmap_obstack);
	      bitmap_copy (bb_info->out, all_ones);
	    }
	}
    }

  if (all_ones)
    BITMAP_FREE (all_ones);
  sbitmap_free (unreachable_blocks);
}



/*----------------------------------------------------------------------------
   Fourth step.

   Solve the bitvector equations.
----------------------------------------------------------------------------*/


/* Confluence function for blocks with no successors.  Create an out
   set from the gen set of the exit block.  This block logically has
   the exit block as a successor.  */



static void
dse_confluence_0 (basic_block bb)
{
  bb_info_t bb_info = bb_table[bb->index];

  if (bb->index == EXIT_BLOCK)
    return;

  if (!bb_info->out)
    {
      bb_info->out = BITMAP_ALLOC (&dse_bitmap_obstack);
      bitmap_copy (bb_info->out, bb_table[EXIT_BLOCK]->gen);
    }
}

/* Propagate the information from the in set of the dest of E to the
   out set of the src of E.  If the various in or out sets are not
   there, that means they are all ones.  */

static bool
dse_confluence_n (edge e)
{
  bb_info_t src_info = bb_table[e->src->index];
  bb_info_t dest_info = bb_table[e->dest->index];

  if (dest_info->in)
    {
      if (src_info->out)
	bitmap_and_into (src_info->out, dest_info->in);
      else
	{
	  src_info->out = BITMAP_ALLOC (&dse_bitmap_obstack);
	  bitmap_copy (src_info->out, dest_info->in);
	}
    }
  return true;
}


/* Propagate the info from the out to the in set of BB_INDEX's basic
   block.  There are three cases:

   1) The block has no kill set.  In this case the kill set is all
   ones.  It does not matter what the out set of the block is, none of
   the info can reach the top.  The only thing that reaches the top is
   the gen set and we just copy the set.

   2) There is a kill set but no out set and bb has successors.  In
   this case we just return. Eventually an out set will be created and
   it is better to wait than to create a set of ones.

   3) There is both a kill and out set.  We apply the obvious transfer
   function.
*/

static bool
dse_transfer_function (int bb_index)
{
  bb_info_t bb_info = bb_table[bb_index];

  if (bb_info->kill)
    {
      if (bb_info->out)
	{
	  /* Case 3 above.  */
	  if (bb_info->in)
	    return bitmap_ior_and_compl (bb_info->in, bb_info->gen,
					 bb_info->out, bb_info->kill);
	  else
	    {
	      bb_info->in = BITMAP_ALLOC (&dse_bitmap_obstack);
	      bitmap_ior_and_compl (bb_info->in, bb_info->gen,
				    bb_info->out, bb_info->kill);
	      return true;
	    }
	}
      else
	/* Case 2 above.  */
	return false;
    }
  else
    {
      /* Case 1 above.  If there is already an in set, nothing
	 happens.  */
      if (bb_info->in)
	return false;
      else
	{
	  bb_info->in = BITMAP_ALLOC (&dse_bitmap_obstack);
	  bitmap_copy (bb_info->in, bb_info->gen);
	  return true;
	}
    }
}

/* Solve the dataflow equations.  */

static void
dse_step4 (void)
{
  df_simple_dataflow (DF_BACKWARD, NULL, dse_confluence_0,
		      dse_confluence_n, dse_transfer_function,
	   	      all_blocks, df_get_postorder (DF_BACKWARD),
		      df_get_n_blocks (DF_BACKWARD));
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      basic_block bb;

      fprintf (dump_file, "\n\n*** Global dataflow info after analysis.\n");
      FOR_ALL_BB_FN (bb, cfun)
	{
	  bb_info_t bb_info = bb_table[bb->index];

	  df_print_bb_index (bb, dump_file);
	  if (bb_info->in)
	    bitmap_print (dump_file, bb_info->in, "  in:   ", "\n");
	  else
	    fprintf (dump_file, "  in:   *MISSING*\n");
	  if (bb_info->gen)
	    bitmap_print (dump_file, bb_info->gen, "  gen:  ", "\n");
	  else
	    fprintf (dump_file, "  gen:  *MISSING*\n");
	  if (bb_info->kill)
	    bitmap_print (dump_file, bb_info->kill, "  kill: ", "\n");
	  else
	    fprintf (dump_file, "  kill: *MISSING*\n");
	  if (bb_info->out)
	    bitmap_print (dump_file, bb_info->out, "  out:  ", "\n");
	  else
	    fprintf (dump_file, "  out:  *MISSING*\n\n");
	}
    }
}



/*----------------------------------------------------------------------------
   Fifth step.

   Delete the stores that can only be deleted using the global information.
----------------------------------------------------------------------------*/


static void
dse_step5 (void)
{
  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      bb_info_t bb_info = bb_table[bb->index];
      insn_info_t insn_info = bb_info->last_insn;
      bitmap v = bb_info->out;

      while (insn_info)
	{
	  bool deleted = false;
	  if (dump_file && insn_info->insn)
	    {
	      fprintf (dump_file, "starting to process insn %d\n",
		       INSN_UID (insn_info->insn));
	      bitmap_print (dump_file, v, "  v:  ", "\n");
	    }

	  /* There may have been code deleted by the dce pass run before
	     this phase.  */
	  if (insn_info->insn
	      && INSN_P (insn_info->insn)
	      && (!insn_info->cannot_delete)
	      && (!bitmap_empty_p (v)))
	    {
	      store_info *store_info = insn_info->store_rec;

	      /* Try to delete the current insn.  */
	      deleted = true;

	      /* Skip the clobbers.  */
	      while (!store_info->is_set)
		store_info = store_info->next;

	      HOST_WIDE_INT i;
	      group_info *group_info = rtx_group_vec[store_info->group_id];

	      for (i = store_info->begin; i < store_info->end; i++)
		{
		  int index = get_bitmap_index (group_info, i);

		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "i = %d, index = %d\n", (int)i, index);
		  if (index == 0 || !bitmap_bit_p (v, index))
		    {
		      if (dump_file && (dump_flags & TDF_DETAILS))
			fprintf (dump_file, "failing at i = %d\n", (int)i);
		      deleted = false;
		      break;
		    }
		}
	      if (deleted)
		{
		  if (dbg_cnt (dse)
		      && check_for_inc_dec_1 (insn_info))
		    {
		      delete_insn (insn_info->insn);
		      insn_info->insn = NULL;
		      globally_deleted++;
		    }
		}
	    }
	  /* We do want to process the local info if the insn was
	     deleted.  For instance, if the insn did a wild read, we
	     no longer need to trash the info.  */
	  if (insn_info->insn
	      && INSN_P (insn_info->insn)
	      && (!deleted))
	    {
	      scan_stores (insn_info->store_rec, v, NULL);
	      if (insn_info->wild_read)
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "wild read\n");
		  bitmap_clear (v);
		}
	      else if (insn_info->read_rec
                       || insn_info->non_frame_wild_read)
		{
		  if (dump_file && !insn_info->non_frame_wild_read)
		    fprintf (dump_file, "regular read\n");
                  else if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "non-frame wild read\n");
		  scan_reads (insn_info, v, NULL);
		}
	    }

	  insn_info = insn_info->prev_insn;
	}
    }
}



/*----------------------------------------------------------------------------
   Sixth step.

   Delete stores made redundant by earlier stores (which store the same
   value) that couldn't be eliminated.
----------------------------------------------------------------------------*/

static void
dse_step6 (void)
{
  basic_block bb;

  FOR_ALL_BB_FN (bb, cfun)
    {
      bb_info_t bb_info = bb_table[bb->index];
      insn_info_t insn_info = bb_info->last_insn;

      while (insn_info)
	{
	  /* There may have been code deleted by the dce pass run before
	     this phase.  */
	  if (insn_info->insn
	      && INSN_P (insn_info->insn)
	      && !insn_info->cannot_delete)
	    {
	      store_info *s_info = insn_info->store_rec;

	      while (s_info && !s_info->is_set)
		s_info = s_info->next;
	      if (s_info
		  && s_info->redundant_reason
		  && s_info->redundant_reason->insn
		  && INSN_P (s_info->redundant_reason->insn))
		{
		  rtx_insn *rinsn = s_info->redundant_reason->insn;
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "Locally deleting insn %d "
					"because insn %d stores the "
					"same value and couldn't be "
					"eliminated\n",
					INSN_UID (insn_info->insn),
					INSN_UID (rinsn));
		  delete_dead_store_insn (insn_info);
		}
	    }
	  insn_info = insn_info->prev_insn;
	}
    }
}

/*----------------------------------------------------------------------------
   Seventh step.

   Destroy everything left standing.
----------------------------------------------------------------------------*/

static void
dse_step7 (void)
{
  bitmap_obstack_release (&dse_bitmap_obstack);
  obstack_free (&dse_obstack, NULL);

  end_alias_analysis ();
  free (bb_table);
  delete rtx_group_table;
  rtx_group_table = NULL;
  rtx_group_vec.release ();
  BITMAP_FREE (all_blocks);
  BITMAP_FREE (scratch);

  rtx_store_info_pool.release ();
  read_info_type_pool.release ();
  insn_info_type_pool.release ();
  dse_bb_info_type_pool.release ();
  group_info_pool.release ();
  deferred_change_pool.release ();
}


/* -------------------------------------------------------------------------
   DSE
   ------------------------------------------------------------------------- */

/* Callback for running pass_rtl_dse.  */

static unsigned int
rest_of_handle_dse (void)
{
  df_set_flags (DF_DEFER_INSN_RESCAN);

  /* Need the notes since we must track live hardregs in the forwards
     direction.  */
  df_note_add_problem ();
  df_analyze ();

  dse_step0 ();
  dse_step1 ();
  dse_step2_init ();
  if (dse_step2 ())
    {
      df_set_flags (DF_LR_RUN_DCE);
      df_analyze ();
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "doing global processing\n");
      dse_step3 ();
      dse_step4 ();
      dse_step5 ();
    }

  dse_step6 ();
  dse_step7 ();

  if (dump_file)
    fprintf (dump_file, "dse: local deletions = %d, global deletions = %d\n",
	     locally_deleted, globally_deleted);

  /* DSE can eliminate potentially-trapping MEMs.
     Remove any EH edges associated with them.  */
  if ((locally_deleted || globally_deleted)
      && cfun->can_throw_non_call_exceptions
      && purge_all_dead_edges ())
    cleanup_cfg (0);

  return 0;
}

namespace {

const pass_data pass_data_rtl_dse1 =
{
  RTL_PASS, /* type */
  "dse1", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_DSE1, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_df_finish, /* todo_flags_finish */
};

class pass_rtl_dse1 : public rtl_opt_pass
{
public:
  pass_rtl_dse1 (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_rtl_dse1, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      return optimize > 0 && flag_dse && dbg_cnt (dse1);
    }

  virtual unsigned int execute (function *) { return rest_of_handle_dse (); }

}; // class pass_rtl_dse1

} // anon namespace

rtl_opt_pass *
make_pass_rtl_dse1 (gcc::context *ctxt)
{
  return new pass_rtl_dse1 (ctxt);
}

namespace {

const pass_data pass_data_rtl_dse2 =
{
  RTL_PASS, /* type */
  "dse2", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_DSE2, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_df_finish, /* todo_flags_finish */
};

class pass_rtl_dse2 : public rtl_opt_pass
{
public:
  pass_rtl_dse2 (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_rtl_dse2, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      return optimize > 0 && flag_dse && dbg_cnt (dse2);
    }

  virtual unsigned int execute (function *) { return rest_of_handle_dse (); }

}; // class pass_rtl_dse2

} // anon namespace

rtl_opt_pass *
make_pass_rtl_dse2 (gcc::context *ctxt)
{
  return new pass_rtl_dse2 (ctxt);
}
