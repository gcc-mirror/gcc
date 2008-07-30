/* RTL dead store elimination.
   Copyright (C) 2005, 2006, 2007 Free Software Foundation, Inc.

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
#include "hashtab.h"
#include "tm.h"
#include "rtl.h"
#include "tree.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "flags.h"
#include "df.h"
#include "cselib.h"
#include "timevar.h"
#include "tree-pass.h"
#include "alloc-pool.h"
#include "alias.h"
#include "insn-config.h"
#include "expr.h"
#include "recog.h"
#include "dse.h"
#include "optabs.h"
#include "dbgcnt.h"

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
   processes the spill spill slots.  This differs from the second
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

   6) Cleanup.

   This step uses cselib and canon_rtx to build the largest expression
   possible for each address.  This pass is a forwards pass through
   each basic block.  From the point of view of the global technique,
   the first pass could examine a block in either direction.  The
   forwards ordering is to accommodate cselib.

   We a simplifying assumption: addresses fall into four broad
   categories:

   1) base has rtx_varies_p == false, offset is constant.
   2) base has rtx_varies_p == false, offset variable.
   3) base has rtx_varies_p == true, offset constant.
   4) base has rtx_varies_p == true, offset variable.

   The local passes are able to process all 4 kinds of addresses.  The
   global pass only handles (1).

   The global problem is formulated as follows:

     A store, S1, to address A, where A is not relative to the stack
     frame, can be eliminated if all paths from S1 to the end of the
     of the function contain another store to A before a read to A.

     If the address A is relative to the stack frame, a store S2 to A
     can be eliminated if there are no paths from S1 that reach the
     end of the function that read A before another store to A.  In
     this case S2 can be deleted if there are paths to from S2 to the
     end of the function that have no reads or writes to A.  This
     second case allows stores to the stack frame to be deleted that
     would otherwise die when the function returns.  This cannot be
     done if stores_off_frame_dead_at_return is not true.  See the doc
     for that variable for when this variable is false.

     The global problem is formulated as a backwards set union
     dataflow problem where the stores are the gens and reads are the
     kills.  Set union problems are rare and require some special
     handling given our representation of bitmaps.  A straightforward
     implementation of requires a lot of bitmaps filled with 1s.
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


static bitmap scratch = NULL;
struct insn_info;

/* This structure holds information about a candidate store.  */
struct store_info 
{

  /* False means this is a clobber.  */
  bool is_set;

  /* The id of the mem group of the base address.  If rtx_varies_p is
     true, this is -1.  Otherwise, it is the index into the group
     table.  */
  int group_id;
  
  /* This is the cselib value.  */
  cselib_val *cse_base;

  /* This canonized mem.  */
  rtx mem;

  /* The result of get_addr on mem.  */
  rtx mem_addr;

  /* If this is non-zero, it is the alias set of a spill location.  */
  alias_set_type alias_set;

  /* The offset of the first and byte before the last byte associated
     with the operation.  */
  int begin, end;

  /* An bitmask as wide as the number of bytes in the word that
     contains a 1 if the byte may be needed.  The store is unused if
     all of the bits are 0.  */
  long positions_needed;

  /* The next store info for this insn.  */
  struct store_info *next;

  /* The right hand side of the store.  This is used if there is a
     subsequent reload of the mems address somewhere later in the
     basic block.  */
  rtx rhs;  
};

typedef struct store_info *store_info_t;
static alloc_pool cse_store_info_pool;
static alloc_pool rtx_store_info_pool;

/* This structure holds information about a load.  These are only
   built for rtx bases.  */
struct read_info 
{
  /* The id of the mem group of the base address.  */
  int group_id;

  /* If this is non-zero, it is the alias set of a spill location.  */
  alias_set_type alias_set;

  /* The offset of the first and byte after the last byte associated
     with the operation.  If begin == end == 0, the read did not have
     a constant offset.  */
  int begin, end;

  /* The mem being read.  */
  rtx mem;

  /* The next read_info for this insn.  */
  struct read_info *next;
};
typedef struct read_info *read_info_t;
static alloc_pool read_info_pool;


/* One of these records is created for each insn.  */

struct insn_info 
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
     This field is set after reload for const function calls.  Having
     this set is less severe than a wild read, it just means that all
     the frame related stores are killed rather than all the stores.  */
  bool frame_read;

  /* This field is only used for the processing of const functions.
     It is set if the insn may contain a stack pointer based store.  */
  bool stack_pointer_based;

  /* This is true if any of the sets within the store contains a
     cselib base.  Such stores can only be deleted by the local
     algorithm.  */
  bool contains_cselib_groups;

  /* The insn. */
  rtx insn;

  /* The list of mem sets or mem clobbers that are contained in this
     insn.  If the insn is deletable, it contains only one mem set.
     But it could also contain clobbers.  Insns that contain more than
     one mem set are not deletable, but each of those mems are here in
     order to provide info to delete other insns.  */
  store_info_t store_rec;

  /* The linked list of mem uses in this insn.  Only the reads from
     rtx bases are listed here.  The reads to cselib bases are
     completely processed during the first scan and so are never
     created.  */
  read_info_t read_rec;

  /* The prev insn in the basic block.  */
  struct insn_info * prev_insn;

  /* The linked list of insns that are in consideration for removal in
     the forwards pass thru the basic block.  This pointer may be
     trash as it is not cleared when a wild read occurs.  The only
     time it is guaranteed to be correct is when the traveral starts
     at active_local_stores.  */
  struct insn_info * next_local_store;
};

typedef struct insn_info *insn_info_t;
static alloc_pool insn_info_pool;

/* The linked list of stores that are under consideration in this
   basic block.  */   
static insn_info_t active_local_stores;

struct bb_info 
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
};

typedef struct bb_info *bb_info_t;
static alloc_pool bb_info_pool;

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

  /* A mem wrapped around the base pointer for the group in order to
     do read dependency.  */
  rtx base_mem;
  
  /* Canonized version of base_mem, most likely the same thing.  */
  rtx canon_base_mem;

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

  /* The positions in this bitmap have the same assignments as the in,
     out, gen and kill bitmaps.  This bitmap is all zeros except for
     the positions that are occupied by stores for this group.  */
  bitmap group_kill;

  /* True if there are any positions that are to be processed
     globally.  */
  bool process_globally;

  /* True if the base of this group is either the frame_pointer or
     hard_frame_pointer.  */
  bool frame_related;

  /* The offset_map is used to map the offsets from this base into
     positions in the global bitmaps.  It is only created after all of
     the all of stores have been scanned and we know which ones we
     care about.  */
  int *offset_map_n, *offset_map_p; 
  int offset_map_size_n, offset_map_size_p; 
};
typedef struct group_info *group_info_t;
typedef const struct group_info *const_group_info_t;
static alloc_pool rtx_group_info_pool;

/* Tables of group_info structures, hashed by base value.  */
static htab_t rtx_group_table;

/* Index into the rtx_group_vec.  */
static int rtx_group_next_id;

DEF_VEC_P(group_info_t);
DEF_VEC_ALLOC_P(group_info_t,heap);

static VEC(group_info_t,heap) *rtx_group_vec;


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

typedef struct deferred_change *deferred_change_t;
static alloc_pool deferred_change_pool;

static deferred_change_t deferred_change_list = NULL;

/* This are used to hold the alias sets of spill variables.  Since
   these are never aliased and there may be a lot of them, it makes
   sense to treat them specially.  This bitvector is only allocated in
   calls from dse_record_singleton_alias_set which currently is only
   made during reload1.  So when dse is called before reload this
   mechanism does nothing.  */

static bitmap clear_alias_sets = NULL;

/* The set of clear_alias_sets that have been disqualified because
   there are loads or stores using a different mode than the alias set
   was registered with.  */ 
static bitmap disqualified_clear_alias_sets = NULL;

/* The group that holds all of the clear_alias_sets.  */
static group_info_t clear_alias_group;

/* The modes of the clear_alias_sets.  */
static htab_t clear_alias_mode_table;

/* Hash table element to look up the mode for an alias set.  */
struct clear_alias_mode_holder
{
  alias_set_type alias_set;
  enum machine_mode mode;
};

static alloc_pool clear_alias_mode_pool;

/* This is true except for two cases:
   (1) current_function_stdarg -- i.e. we cannot do this 
       for vararg functions because they play games with the frame.  
   (2) In ada, it is sometimes not safe to do assume that any stores
       based off the stack frame go dead at the exit to a function.  */
static bool stores_off_frame_dead_at_return;

/* Counter for stats.  */
static int globally_deleted; 
static int locally_deleted; 
static int spill_deleted; 
      
static bitmap all_blocks;

/* The number of bits used in the global bitmaps.  */
static unsigned int current_position;


static bool gate_dse (void);


/*----------------------------------------------------------------------------
   Zeroth step.

   Initialization.  
----------------------------------------------------------------------------*/

/* Hashtable callbacks for maintaining the "bases" field of
   store_group_info, given that the addresses are function invariants.  */

static int
clear_alias_mode_eq (const void *p1, const void *p2)
{
  const struct clear_alias_mode_holder * h1 
    = (const struct clear_alias_mode_holder *) p1;
  const struct clear_alias_mode_holder * h2 
    = (const struct clear_alias_mode_holder *) p2;
  return h1->alias_set == h2->alias_set;
}


static hashval_t
clear_alias_mode_hash (const void *p)
{
  const struct clear_alias_mode_holder *holder 
    = (const struct clear_alias_mode_holder *) p;
  return holder->alias_set;
}


/* Find the entry associated with ALIAS_SET.  */

static struct clear_alias_mode_holder *
clear_alias_set_lookup (alias_set_type alias_set)
{
  struct clear_alias_mode_holder tmp_holder;
  void **slot;
  
  tmp_holder.alias_set = alias_set;
  slot = htab_find_slot (clear_alias_mode_table, &tmp_holder, NO_INSERT);
  gcc_assert (*slot);
  
  return *slot;
}


/* Hashtable callbacks for maintaining the "bases" field of
   store_group_info, given that the addresses are function invariants.  */

static int
invariant_group_base_eq (const void *p1, const void *p2)
{
  const_group_info_t gi1 = (const_group_info_t) p1;
  const_group_info_t gi2 = (const_group_info_t) p2;
  return rtx_equal_p (gi1->rtx_base, gi2->rtx_base);
}


static hashval_t
invariant_group_base_hash (const void *p)
{
  const_group_info_t gi = (const_group_info_t) p;
  int do_not_record;
  return hash_rtx (gi->rtx_base, Pmode, &do_not_record, NULL, false);
}


/* Get the GROUP for BASE.  Add a new group if it is not there.  */

static group_info_t
get_group_info (rtx base)
{
  struct group_info tmp_gi; 
  group_info_t gi; 
  void **slot;

  if (base)
    {
      /* Find the store_base_info structure for BASE, creating a new one
	 if necessary.  */
      tmp_gi.rtx_base = base;
      slot = htab_find_slot (rtx_group_table, &tmp_gi, INSERT);
      gi = (group_info_t) *slot;
    }
  else
    {
      if (!clear_alias_group)
	{
	  clear_alias_group = gi = pool_alloc (rtx_group_info_pool);
	  memset (gi, 0, sizeof (struct group_info));
	  gi->id = rtx_group_next_id++;
	  gi->store1_n = BITMAP_ALLOC (NULL);
	  gi->store1_p = BITMAP_ALLOC (NULL);
	  gi->store2_n = BITMAP_ALLOC (NULL);
	  gi->store2_p = BITMAP_ALLOC (NULL);
	  gi->group_kill = BITMAP_ALLOC (NULL);
	  gi->process_globally = false;
	  gi->offset_map_size_n = 0;
	  gi->offset_map_size_p = 0;
	  gi->offset_map_n = NULL;
	  gi->offset_map_p = NULL;
	  VEC_safe_push (group_info_t, heap, rtx_group_vec, gi);
	}
      return clear_alias_group;
    }

  if (gi == NULL)
    {
      *slot = gi = pool_alloc (rtx_group_info_pool);
      gi->rtx_base = base;
      gi->id = rtx_group_next_id++;
      gi->base_mem = gen_rtx_MEM (QImode, base);
      gi->canon_base_mem = canon_rtx (gi->base_mem);
      gi->store1_n = BITMAP_ALLOC (NULL);
      gi->store1_p = BITMAP_ALLOC (NULL);
      gi->store2_n = BITMAP_ALLOC (NULL);
      gi->store2_p = BITMAP_ALLOC (NULL);
      gi->group_kill = BITMAP_ALLOC (NULL);
      gi->process_globally = false;
      gi->frame_related = 
	(base == frame_pointer_rtx) || (base == hard_frame_pointer_rtx);
      gi->offset_map_size_n = 0;
      gi->offset_map_size_p = 0;
      gi->offset_map_n = NULL;
      gi->offset_map_p = NULL;
      VEC_safe_push (group_info_t, heap, rtx_group_vec, gi);
    }

  return gi;
}


/* Initialization of data structures.  */

static void
dse_step0 (void)
{
  locally_deleted = 0;
  globally_deleted = 0;
  spill_deleted = 0;

  scratch = BITMAP_ALLOC (NULL);

  rtx_store_info_pool
    = create_alloc_pool ("rtx_store_info_pool", 
			 sizeof (struct store_info), 100);
  read_info_pool
    = create_alloc_pool ("read_info_pool", 
			 sizeof (struct read_info), 100);
  insn_info_pool
    = create_alloc_pool ("insn_info_pool", 
			 sizeof (struct insn_info), 100);
  bb_info_pool
    = create_alloc_pool ("bb_info_pool", 
			 sizeof (struct bb_info), 100);
  rtx_group_info_pool
    = create_alloc_pool ("rtx_group_info_pool", 
			 sizeof (struct group_info), 100);
  deferred_change_pool
    = create_alloc_pool ("deferred_change_pool", 
			 sizeof (struct deferred_change), 10);

  rtx_group_table = htab_create (11, invariant_group_base_hash,
				 invariant_group_base_eq, NULL);

  bb_table = XCNEWVEC (bb_info_t, last_basic_block);
  rtx_group_next_id = 0;

  stores_off_frame_dead_at_return = 
    (!(TREE_CODE (TREE_TYPE (current_function_decl)) == FUNCTION_TYPE
       && (TYPE_RETURNS_STACK_DEPRESSED (TREE_TYPE (current_function_decl)))))
    && (!current_function_stdarg);

  init_alias_analysis ();
  
  if (clear_alias_sets)
    clear_alias_group = get_group_info (NULL);
  else
    clear_alias_group = NULL;
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
  store_info_t store_info = insn_info->store_rec;
  while (store_info)
    {
      store_info_t next = store_info->next;
      if (store_info->cse_base)
	pool_free (cse_store_info_pool, store_info);
      else
	pool_free (rtx_store_info_pool, store_info);
      store_info = next;
    }

  insn_info->cannot_delete = true;
  insn_info->contains_cselib_groups = false;
  insn_info->store_rec = NULL;
}


struct insn_size {
  int size;
  rtx insn;
};


/* Add an insn to do the add inside a x if it is a
   PRE/POST-INC/DEC/MODIFY.  D is an structure containing the insn and
   the size of the mode of the MEM that this is inside of.  */

static int
replace_inc_dec (rtx *r, void *d)
{
  rtx x = *r;
  struct insn_size *data = (struct insn_size *)d;
  switch (GET_CODE (x))
    {
    case PRE_INC:
    case POST_INC:
      {
	rtx r1 = XEXP (x, 0);
	rtx c = gen_int_mode (Pmode, data->size);
	emit_insn_before (gen_rtx_SET (Pmode, r1, 
				       gen_rtx_PLUS (Pmode, r1, c)),
			  data->insn);
	return -1;
      }
		 
    case PRE_DEC:
    case POST_DEC:
      {
	rtx r1 = XEXP (x, 0);
	rtx c = gen_int_mode (Pmode, -data->size);
	emit_insn_before (gen_rtx_SET (Pmode, r1, 
				       gen_rtx_PLUS (Pmode, r1, c)),
			  data->insn);
	return -1;
      }
	
    case PRE_MODIFY:
    case POST_MODIFY:
      {
	/* We can reuse the add because we are about to delete the
	   insn that contained it.  */
	rtx add = XEXP (x, 0);
	rtx r1 = XEXP (add, 0);
	emit_insn_before (gen_rtx_SET (Pmode, r1, add), data->insn);
	return -1;
      }

    default:
      return 0;
    }
}
			 

/* If X is a MEM, check the address to see if it is PRE/POST-INC/DEC/MODIFY
   and generate an add to replace that.  */

static int
replace_inc_dec_mem (rtx *r, void *d)
{
  rtx x = *r;
  if (x != NULL_RTX && MEM_P (x))
    {
      struct insn_size data;

      data.size = GET_MODE_SIZE (GET_MODE (x));
      data.insn = (rtx) d;

      for_each_rtx (&XEXP (x, 0), replace_inc_dec, &data);
	
      return -1;
    }
  return 0;
}

/* Before we delete INSN, make sure that the auto inc/dec, if it is
   there, is split into a separate insn.  */

static void
check_for_inc_dec (rtx insn)
{
  rtx note = find_reg_note (insn, REG_INC, NULL_RTX);
  if (note)
    for_each_rtx (&insn, replace_inc_dec_mem, insn);
}


/* Delete the insn and free all of the fields inside INSN_INFO.  */ 

static void
delete_dead_store_insn (insn_info_t insn_info)
{
  read_info_t read_info;

  if (!dbg_cnt (dse))
    return;

  check_for_inc_dec (insn_info->insn);
  if (dump_file)
    {
      fprintf (dump_file, "Locally deleting insn %d ", 
	       INSN_UID (insn_info->insn));
      if (insn_info->store_rec->alias_set)
	fprintf (dump_file, "alias set %d\n", 
		 (int) insn_info->store_rec->alias_set);
      else
	fprintf (dump_file, "\n");
    }

  free_store_info (insn_info);
  read_info = insn_info->read_rec;
	
  while (read_info)
    {
      read_info_t next = read_info->next;
      pool_free (read_info_pool, read_info);
      read_info = next;
    }
  insn_info->read_rec = NULL;

  delete_insn (insn_info->insn);
  locally_deleted++;
  insn_info->insn = NULL;

  insn_info->wild_read = false;
}


/* Set the store* bitmaps offset_map_size* fields in GROUP based on
   OFFSET and WIDTH.  */

static void
set_usage_bits (group_info_t group, HOST_WIDE_INT offset, HOST_WIDE_INT width)
{
  HOST_WIDE_INT i;

  if ((offset > -MAX_OFFSET) && (offset < MAX_OFFSET))
    for (i=offset; i<offset+width; i++)
      {
	bitmap store1;
	bitmap store2;
	int ai;
	if (i < 0)
	  {
	    store1 = group->store1_n;
	    store2 = group->store2_n;
	    ai = -i;
	  }
	else
	  {
	    store1 = group->store1_p;
	    store2 = group->store2_p;
	    ai = i;
	  }
	
	if (bitmap_bit_p (store1, ai))
	  bitmap_set_bit (store2, ai);
	else 
	  {
	    bitmap_set_bit (store1, ai);
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
      }
}


/* Set the BB_INFO so that the last insn is marked as a wild read.  */

static void
add_wild_read (bb_info_t bb_info)
{
  insn_info_t insn_info = bb_info->last_insn;
  read_info_t *ptr = &insn_info->read_rec;

  while (*ptr)
    {
      read_info_t next = (*ptr)->next;
      if ((*ptr)->alias_set == 0)
        {
          pool_free (read_info_pool, *ptr);
          *ptr = next;
	}
      else 
	ptr = &(*ptr)->next;
    }
  insn_info->wild_read = true;
  active_local_stores = NULL;
}


/* Return true if X is a constant or one of the registers that behave
   as a constant over the life of a function.  This is equivalent to
   !rtx_varies_p for memory addresses.  */

static bool
const_or_frame_p (rtx x)
{
  switch (GET_CODE (x))
    {
    case MEM:
      return MEM_READONLY_P (x);

    case CONST:
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_VECTOR:
    case SYMBOL_REF:
    case LABEL_REF:
      return true;

    case REG:
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

    default:
      return false;
    }
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
	       alias_set_type *alias_set_out,
	       int *group_id,
	       HOST_WIDE_INT *offset, 
	       cselib_val **base)
{
  rtx mem_address = XEXP (mem, 0);
  rtx expanded_address, address;
  /* Make sure that cselib is has initialized all of the operands of
     the address before asking it to do the subst.  */

  if (clear_alias_sets)
    {
      /* If this is a spill, do not do any further processing.  */
      alias_set_type alias_set = MEM_ALIAS_SET (mem);
      if (dump_file)
	fprintf (dump_file, "found alias set %d\n", (int) alias_set);
      if (bitmap_bit_p (clear_alias_sets, alias_set))
	{
	  struct clear_alias_mode_holder *entry 
	    = clear_alias_set_lookup (alias_set);

	  /* If the modes do not match, we cannot process this set.  */
	  if (entry->mode != GET_MODE (mem))
	    {
	      if (dump_file)
		fprintf (dump_file, 
			 "disqualifying alias set %d, (%s) != (%s)\n", 
			 (int) alias_set, GET_MODE_NAME (entry->mode), 
			 GET_MODE_NAME (GET_MODE (mem)));
	      
	      bitmap_set_bit (disqualified_clear_alias_sets, alias_set);
	      return false;
	    }

	  *alias_set_out = alias_set;
	  *group_id = clear_alias_group->id;
	  return true;
	}
    }

  *alias_set_out = 0;

  cselib_lookup (mem_address, Pmode, 1);

  if (dump_file)
    {
      fprintf (dump_file, "  mem: ");
      print_inline_rtx (dump_file, mem_address, 0);
      fprintf (dump_file, "\n");
    }

  /* Use cselib to replace all of the reg references with the full
     expression.  This will take care of the case where we have 

     r_x = base + offset;
     val = *r_x;
   
     by making it into 

     val = *(base + offset);  
  */

  expanded_address = cselib_expand_value_rtx (mem_address, scratch, 5);

  /* If this fails, just go with the mem_address.  */
  if (!expanded_address)
    expanded_address = mem_address;

  /* Split the address into canonical BASE + OFFSET terms.  */
  address = canon_rtx (expanded_address);

  *offset = 0;

  if (dump_file)
    {
      fprintf (dump_file, "\n   after cselib_expand address: ");
      print_inline_rtx (dump_file, expanded_address, 0);
      fprintf (dump_file, "\n");

      fprintf (dump_file, "\n   after canon_rtx address: ");
      print_inline_rtx (dump_file, address, 0);
      fprintf (dump_file, "\n");
    }

  if (GET_CODE (address) == CONST)
    address = XEXP (address, 0);

  if (GET_CODE (address) == PLUS && GET_CODE (XEXP (address, 1)) == CONST_INT)
    {
      *offset = INTVAL (XEXP (address, 1));
      address = XEXP (address, 0);
    }

  if (const_or_frame_p (address))
    {
      group_info_t group = get_group_info (address);

      if (dump_file)
	fprintf (dump_file, "  gid=%d offset=%d \n", group->id, (int)*offset);
      *base = NULL;
      *group_id = group->id;
    }
  else
    {
      *base = cselib_lookup (address, Pmode, true);
      *group_id = -1;

      if (*base == NULL)
	{
	  if (dump_file)
	    fprintf (dump_file, " no cselib val - should be a wild read.\n");
	  return false;
	}
      if (dump_file)
	fprintf (dump_file, "  varying cselib base=%d offset = %d\n", 
		 (*base)->value, (int)*offset);
    }
  return true;
}


/* Clear the rhs field from the active_local_stores array.  */

static void
clear_rhs_from_active_local_stores (void)
{
  insn_info_t ptr = active_local_stores;

  while (ptr)
    {
      store_info_t store_info = ptr->store_rec;
      /* Skip the clobbers.  */
      while (!store_info->is_set)
	store_info = store_info->next;

      store_info->rhs = NULL;

      ptr = ptr->next_local_store;
    }
}


/* BODY is an instruction pattern that belongs to INSN.  Return 1 if
   there is a candidate store, after adding it to the appropriate
   local store group if so.  */

static int
record_store (rtx body, bb_info_t bb_info)
{
  rtx mem;
  HOST_WIDE_INT offset = 0;
  HOST_WIDE_INT width = 0;
  alias_set_type spill_alias_set;
  insn_info_t insn_info = bb_info->last_insn;
  store_info_t store_info = NULL;
  int group_id;
  cselib_val *base = NULL;
  insn_info_t ptr, last;
  bool store_is_unused;

  if (GET_CODE (body) != SET && GET_CODE (body) != CLOBBER)
    return 0;

  /* If this is not used, then this cannot be used to keep the insn
     from being deleted.  On the other hand, it does provide something
     that can be used to prove that another store is dead.  */
  store_is_unused
    = (find_reg_note (insn_info->insn, REG_UNUSED, body) != NULL);

  /* Check whether that value is a suitable memory location.  */
  mem = SET_DEST (body);
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
	  if (dump_file) 
	    fprintf (dump_file, " adding wild read for (clobber (mem:BLK (scratch))\n");
	  add_wild_read (bb_info);
	  insn_info->cannot_delete = true;
	}
      else if (!store_is_unused)
	{
	  /* If the set or clobber is unused, then it does not effect our
	     ability to get rid of the entire insn.  */
	  insn_info->cannot_delete = true;
	  clear_rhs_from_active_local_stores ();
	}
      return 0;
    }

  /* We can still process a volatile mem, we just cannot delete it.  */
  if (MEM_VOLATILE_P (mem))
      insn_info->cannot_delete = true;

  if (!canon_address (mem, &spill_alias_set, &group_id, &offset, &base))
    {
      clear_rhs_from_active_local_stores ();
      return 0;
    }

  width = GET_MODE_SIZE (GET_MODE (mem));

  if (spill_alias_set)
    {
      bitmap store1 = clear_alias_group->store1_p;
      bitmap store2 = clear_alias_group->store2_p;
      
      if (bitmap_bit_p (store1, spill_alias_set))
	bitmap_set_bit (store2, spill_alias_set);
      else 
	bitmap_set_bit (store1, spill_alias_set);
	
      if (clear_alias_group->offset_map_size_p < spill_alias_set)
	clear_alias_group->offset_map_size_p = spill_alias_set;
  
      store_info = pool_alloc (rtx_store_info_pool);

      if (dump_file)
	fprintf (dump_file, " processing spill store %d(%s)\n",
		 (int) spill_alias_set, GET_MODE_NAME (GET_MODE (mem)));
    }
  else if (group_id >= 0)
    {
      /* In the restrictive case where the base is a constant or the
	 frame pointer we can do global analysis.  */
      
      group_info_t group 
	= VEC_index (group_info_t, rtx_group_vec, group_id);
      
      store_info = pool_alloc (rtx_store_info_pool);
      set_usage_bits (group, offset, width);

      if (dump_file)
	fprintf (dump_file, " processing const base store gid=%d[%d..%d)\n",
		 group_id, (int)offset, (int)(offset+width));
    }
  else
    {
      rtx base_term = find_base_term (XEXP (mem, 0));
      if (!base_term
	  || (GET_CODE (base_term) == ADDRESS
	      && GET_MODE (base_term) == Pmode
	      && XEXP (base_term, 0) == stack_pointer_rtx))
	insn_info->stack_pointer_based = true;
      insn_info->contains_cselib_groups = true;

      store_info = pool_alloc (cse_store_info_pool);
      group_id = -1;

      if (dump_file)
	fprintf (dump_file, " processing cselib store [%d..%d)\n",
		 (int)offset, (int)(offset+width));
    }

  /* Check to see if this stores causes some other stores to be
     dead.  */
  ptr = active_local_stores;
  last = NULL;

  while (ptr)
    {
      insn_info_t next = ptr->next_local_store;
      store_info_t s_info = ptr->store_rec;
      bool delete = true;

      /* Skip the clobbers. We delete the active insn if this insn
	 shadows the set.  To have been put on the active list, it
	 has exactly on set. */
      while (!s_info->is_set)
	s_info = s_info->next;

      if (s_info->alias_set != spill_alias_set)
	delete = false;
      else if (s_info->alias_set)
	{
	  struct clear_alias_mode_holder *entry 
	    = clear_alias_set_lookup (s_info->alias_set);
	  /* Generally, spills cannot be processed if and of the
	     references to the slot have a different mode.  But if
	     we are in the same block and mode is exactly the same
	     between this store and one before in the same block,
	     we can still delete it.  */
	  if ((GET_MODE (mem) == GET_MODE (s_info->mem))
	      && (GET_MODE (mem) == entry->mode))
	    {
	      delete = true;
	      s_info->positions_needed = 0;
	    }
	  if (dump_file)
	    fprintf (dump_file, "    trying spill store in insn=%d alias_set=%d\n",
		     INSN_UID (ptr->insn), (int) s_info->alias_set);
	}
      else if ((s_info->group_id == group_id) 
	       && (s_info->cse_base == base))
	{
	  HOST_WIDE_INT i;
	  if (dump_file)
	    fprintf (dump_file, "    trying store in insn=%d gid=%d[%d..%d)\n",
		     INSN_UID (ptr->insn), s_info->group_id, 
		     (int)s_info->begin, (int)s_info->end);
	  for (i = offset; i < offset+width; i++)
	    if (i >= s_info->begin && i < s_info->end)
	      s_info->positions_needed &= ~(1L << (i - s_info->begin));
	}
      else if (s_info->rhs)
	/* Need to see if it is possible for this store to overwrite
	   the value of store_info.  If it is, set the rhs to NULL to
	   keep it from being used to remove a load.  */
	{
	  if (canon_true_dependence (s_info->mem, 
				     GET_MODE (s_info->mem),
				     s_info->mem_addr,
				     mem, rtx_varies_p))
	    s_info->rhs = NULL;
	}
      
      /* An insn can be deleted if every position of every one of
	 its s_infos is zero.  */
      if (s_info->positions_needed != 0)
	delete = false;
      
      if (delete)
	{
	  insn_info_t insn_to_delete = ptr;
	  
	  if (last)
	    last->next_local_store = ptr->next_local_store;
	  else
	    active_local_stores = ptr->next_local_store;
	  
	  delete_dead_store_insn (insn_to_delete);
	}
      else
	last = ptr;
      
      ptr = next;
    }
  
  gcc_assert ((unsigned) width < sizeof (store_info->positions_needed) * CHAR_BIT);
  
  /* Finish filling in the store_info.  */
  store_info->next = insn_info->store_rec;
  insn_info->store_rec = store_info;
  store_info->mem = canon_rtx (mem);
  store_info->alias_set = spill_alias_set;
  store_info->mem_addr = get_addr (XEXP (mem, 0));
  store_info->cse_base = base;
  store_info->positions_needed = (1L << width) - 1;
  store_info->group_id = group_id;
  store_info->begin = offset;
  store_info->end = offset + width;
  store_info->is_set = GET_CODE (body) == SET;

  if (store_info->is_set 
      /* No place to keep the value after ra.  */
      && !reload_completed
      /* The careful reviewer may wish to comment my checking that the
	 rhs of a store is always a reg.  */
      && REG_P (SET_SRC (body))
      /* Sometimes the store and reload is used for truncation and
	 rounding.  */
      && !(FLOAT_MODE_P (GET_MODE (mem)) && (flag_float_store)))
    store_info->rhs = SET_SRC (body);
  else
    store_info->rhs = NULL;
  
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
find_shift_sequence (rtx read_reg,
		     int access_size,
		     store_info_t store_info,
		     read_info_t read_info,
		     int shift)
{
  enum machine_mode store_mode = GET_MODE (store_info->mem);
  enum machine_mode read_mode = GET_MODE (read_info->mem);
  rtx chosen_seq = NULL;

  /* Some machines like the x86 have shift insns for each size of
     operand.  Other machines like the ppc or the ia-64 may only have
     shift insns that shift values within 32 or 64 bit registers.
     This loop tries to find the smallest shift insn that will right
     justify the value we want to read but is available in one insn on
     the machine.  */

  for (; access_size <= UNITS_PER_WORD; access_size *= 2)
    {
      rtx target, new_reg, shift_seq, insn;
      enum machine_mode new_mode;
      int cost;

      /* Try a wider mode if truncating the store mode to ACCESS_SIZE
	 bytes requires a real instruction.  */
      if (access_size < GET_MODE_SIZE (store_mode)
	  && !TRULY_NOOP_TRUNCATION (access_size * BITS_PER_UNIT,
				     GET_MODE_BITSIZE (store_mode)))
	continue;

      new_mode = smallest_mode_for_size (access_size * BITS_PER_UNIT,
					 MODE_INT);
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
	  cost += insn_rtx_cost (PATTERN (insn));

      /* The computation up to here is essentially independent
	 of the arguments and could be precomputed.  It may
	 not be worth doing so.  We could precompute if
	 worthwhile or at least cache the results.  The result
	 technically depends on both SHIFT and ACCESS_SIZE,
	 but in practice the answer will depend only on ACCESS_SIZE.  */

      if (cost > COSTS_N_INSNS (1))
	continue;

      /* We found an acceptable shift.  Generate a move to
	 take the value from the store and put it into the
	 shift pseudo, then shift it, then generate another
	 move to put in into the target of the read.  */
      start_sequence ();
      emit_move_insn (new_reg, gen_lowpart (new_mode, store_info->rhs));
      emit_insn (shift_seq);
      convert_move (read_reg, new_reg, 1);
		  
      if (dump_file)
	{
	  fprintf (dump_file, " -- adding extract insn r%d:%s = r%d:%s\n",
		   REGNO (new_reg), GET_MODE_NAME (new_mode),
		   REGNO (store_info->rhs), GET_MODE_NAME (store_mode));
		      
	  fprintf (dump_file, " -- with shift of r%d by %d\n",
		   REGNO(new_reg), shift);
	  fprintf (dump_file, " -- and second extract insn r%d:%s = r%d:%s\n",
		   REGNO (read_reg), GET_MODE_NAME (read_mode),
		   REGNO (new_reg), GET_MODE_NAME (new_mode));
	}
		  
      /* Get the three insn sequence and return it.  */
      chosen_seq = get_insns ();
      end_sequence ();
      break;
    }

  return chosen_seq;
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
replace_read (store_info_t store_info, insn_info_t store_insn, 
	      read_info_t read_info, insn_info_t read_insn, rtx *loc)
{
  enum machine_mode store_mode = GET_MODE (store_info->mem);
  enum machine_mode read_mode = GET_MODE (read_info->mem);
  int shift;
  int access_size; /* In bytes.  */
  rtx read_reg = gen_reg_rtx (read_mode);
  rtx shift_seq = NULL;

  if (!dbg_cnt (dse))
    return false;

  if (GET_MODE_CLASS (read_mode) != MODE_INT
      || GET_MODE_CLASS (store_mode) != MODE_INT)
    return false;

  /* To get here the read is within the boundaries of the write so
     shift will never be negative.  Start out with the shift being in
     bytes.  */
  if (BYTES_BIG_ENDIAN)
    shift = store_info->end - read_info->end;
  else
    shift = read_info->begin - store_info->begin;

  access_size = shift + GET_MODE_SIZE (read_mode);

  /* From now on it is bits.  */
  shift *= BITS_PER_UNIT;

  /* We need to keep this in perspective.  We are replacing a read
     with a sequence of insns, but the read will almost certainly be
     in cache, so it is not going to be an expensive one.  Thus, we
     are not willing to do a multi insn shift or worse a subroutine
     call to get rid of the read.  */
  if (shift)
    {
      if (access_size > UNITS_PER_WORD)
	return false;

      shift_seq = find_shift_sequence (read_reg, access_size, store_info,
				       read_info, shift);
      if (!shift_seq)
	return false;
    }

  if (dump_file)
    fprintf (dump_file, "replacing load at %d from store at %d\n",
	     INSN_UID (read_insn->insn), INSN_UID (store_insn->insn)); 

  if (validate_change (read_insn->insn, loc, read_reg, 0))
    {
      rtx insns;
      deferred_change_t deferred_change = pool_alloc (deferred_change_pool);
      
      if (read_mode == store_mode)
	{
	  start_sequence ();
	  
	  /* The modes are the same and everything lines up.  Just
	     generate a simple move.  */
	  emit_move_insn (read_reg, store_info->rhs);
	  if (dump_file)
	    fprintf (dump_file, " -- adding move insn r%d = r%d\n",
		     REGNO (read_reg), REGNO (store_info->rhs));
	  insns = get_insns ();
	  end_sequence ();
	}
      else if (shift)
	insns = shift_seq;
      else
	{
	  /* The modes are different but the lsb are in the same
	     place, we need to extract the value in the right from the
	     rhs of the store.  */
	  start_sequence ();
	  convert_move (read_reg, store_info->rhs, 1);
	  
	  if (dump_file)
	    fprintf (dump_file, " -- adding extract insn r%d:%s = r%d:%s\n",
		     REGNO (read_reg), GET_MODE_NAME (read_mode),
		     REGNO (store_info->rhs), GET_MODE_NAME (store_mode));
	  insns = get_insns ();
	  end_sequence ();
	}

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
      deferred_change->next = deferred_change_list;
      deferred_change_list = deferred_change;
      deferred_change->loc = loc;
      deferred_change->reg = read_reg;
      
      /* Get rid of the read_info, from the point of view of the
	 rest of dse, play like this read never happened.  */
      read_insn->read_rec = read_info->next;
      pool_free (read_info_pool, read_info);
      return true;
    }
  else 
    {
      if (dump_file)
	fprintf (dump_file, " -- validation failure\n"); 
      return false;
    }
}

/* A for_each_rtx callback in which DATA is the bb_info.  Check to see
   if LOC is a mem and if it is look at the address and kill any
   appropriate stores that may be active.  */

static int
check_mem_read_rtx (rtx *loc, void *data)
{
  rtx mem = *loc;
  bb_info_t bb_info;
  insn_info_t insn_info;
  HOST_WIDE_INT offset = 0;
  HOST_WIDE_INT width = 0;
  alias_set_type spill_alias_set = 0;
  cselib_val *base = NULL;  
  int group_id;
  read_info_t read_info;

  if (!mem || !MEM_P (mem))
    return 0;

  bb_info = (bb_info_t) data;
  insn_info = bb_info->last_insn;

  if ((MEM_ALIAS_SET (mem) == ALIAS_SET_MEMORY_BARRIER)
      || (MEM_VOLATILE_P (mem)))
    {
      if (dump_file)
	fprintf (dump_file, " adding wild read, volatile or barrier.\n");
      add_wild_read (bb_info);
      insn_info->cannot_delete = true;
      return 0;
    }

  /* If it is reading readonly mem, then there can be no conflict with
     another write. */
  if (MEM_READONLY_P (mem))
    return 0;

  if (!canon_address (mem, &spill_alias_set, &group_id, &offset, &base))
    {
      if (dump_file)
	fprintf (dump_file, " adding wild read, canon_address failure.\n");
      add_wild_read (bb_info);
      return 0;
    }

  if (GET_MODE (mem) == BLKmode)
    width = -1;
  else
    width = GET_MODE_SIZE (GET_MODE (mem));

  read_info = pool_alloc (read_info_pool);
  read_info->group_id = group_id;
  read_info->mem = mem;
  read_info->alias_set = spill_alias_set;
  read_info->begin = offset;
  read_info->end = offset + width;
  read_info->next = insn_info->read_rec;
  insn_info->read_rec = read_info;

  /* We ignore the clobbers in store_info.  The is mildly aggressive,
     but there really should not be a clobber followed by a read.  */

  if (spill_alias_set)
    {
      insn_info_t i_ptr = active_local_stores;
      insn_info_t last = NULL;

      if (dump_file)
	fprintf (dump_file, " processing spill load %d\n",
		 (int) spill_alias_set);

      while (i_ptr)
	{
	  store_info_t store_info = i_ptr->store_rec;

	  /* Skip the clobbers.  */
	  while (!store_info->is_set)
	    store_info = store_info->next;
	  
	  if (store_info->alias_set == spill_alias_set)
	    {
	      if (dump_file)
		dump_insn_info ("removing from active", i_ptr);

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
  else if (group_id >= 0)
    {
      /* This is the restricted case where the base is a constant or
	 the frame pointer and offset is a constant.  */
      insn_info_t i_ptr = active_local_stores;
      insn_info_t last = NULL;
      
      if (dump_file)
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
	  store_info_t store_info = i_ptr->store_rec;
	  
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
				       mem, rtx_varies_p);
	  
	  else if (group_id == store_info->group_id)
	    {
	      /* This is a block mode load.  We may get lucky and
		 canon_true_dependence may save the day.  */
	      if (width == -1)
		remove 
		  = canon_true_dependence (store_info->mem, 
					   GET_MODE (store_info->mem),
					   store_info->mem_addr,
					   mem, rtx_varies_p);
	      
	      /* If this read is just reading back something that we just
		 stored, rewrite the read.  */
	      else 
		{
		  if (store_info->rhs
		      && (offset >= store_info->begin)
		      && (offset + width <= store_info->end))
		    {
		      int mask = ((1L << width) - 1) << (offset - store_info->begin);
		      
		      if ((store_info->positions_needed & mask) == mask
			  && replace_read (store_info, i_ptr, 
					   read_info, insn_info, loc))
			return 0;
		    }
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
	      if (dump_file)
		dump_insn_info ("removing from active", i_ptr);

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
      if (dump_file)
	{
	  fprintf (dump_file, " processing cselib load mem:");
	  print_inline_rtx (dump_file, mem, 0);
	  fprintf (dump_file, "\n");
	}

      while (i_ptr)
	{
	  bool remove = false;
	  store_info_t store_info = i_ptr->store_rec;
	  
	  if (dump_file)
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
	      && (offset >= store_info->begin)
	      && (offset + width <= store_info->end))
	    {
	      int mask = ((1L << width) - 1) << (offset - store_info->begin);
	      
	      if ((store_info->positions_needed & mask) == mask
		  && replace_read (store_info, i_ptr, 
				   read_info, insn_info, loc))
		return 0;
	    }

	  if (!store_info->alias_set)
	    remove = canon_true_dependence (store_info->mem, 
					    GET_MODE (store_info->mem),
					    store_info->mem_addr,
					    mem, rtx_varies_p);
	  
	  if (remove)
	    {
	      if (dump_file)
		dump_insn_info ("removing from active", i_ptr);
	      
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
  return 0;
}

/* A for_each_rtx callback in which DATA points the INSN_INFO for 
   as check_mem_read_rtx.  Nullify the pointer if i_m_r_m_r returns
   true for any part of *LOC.  */

static void
check_mem_read_use (rtx *loc, void *data)
{
  for_each_rtx (loc, check_mem_read_rtx, data);
}

/* Apply record_store to all candidate stores in INSN.  Mark INSN
   if some part of it is not a candidate store and assigns to a
   non-register target.  */

static void
scan_insn (bb_info_t bb_info, rtx insn)
{
  rtx body;
  insn_info_t insn_info = pool_alloc (insn_info_pool);
  int mems_found = 0;
  memset (insn_info, 0, sizeof (struct insn_info));

  if (dump_file)
    fprintf (dump_file, "\n**scanning insn=%d\n",
	     INSN_UID (insn));

  insn_info->prev_insn = bb_info->last_insn;
  insn_info->insn = insn;
  bb_info->last_insn = insn_info;
  

  /* Cselib clears the table for this case, so we have to essentially
     do the same.  */
  if (NONJUMP_INSN_P (insn)
      && GET_CODE (PATTERN (insn)) == ASM_OPERANDS
      && MEM_VOLATILE_P (PATTERN (insn)))
    {
      add_wild_read (bb_info);
      insn_info->cannot_delete = true;
      return;
    }

  /* Look at all of the uses in the insn.  */
  note_uses (&PATTERN (insn), check_mem_read_use, bb_info);

  if (CALL_P (insn))
    {
      insn_info->cannot_delete = true;

      /* Const functions cannot do anything bad i.e. read memory,
	 however, they can read their parameters which may have
	 been pushed onto the stack.  */
      if (CONST_OR_PURE_CALL_P (insn) && !pure_call_p (insn))
	{
	  insn_info_t i_ptr = active_local_stores;
	  insn_info_t last = NULL;

	  if (dump_file)
	    fprintf (dump_file, "const call %d\n", INSN_UID (insn));

	  /* See the head comment of the frame_read field.  */
	  if (reload_completed)
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
		  store_info_t store_info = i_ptr->store_rec;

		  /* Skip the clobbers.  */
		  while (!store_info->is_set)
		    store_info = store_info->next;

		  if (store_info->group_id >= 0
		      && VEC_index (group_info_t, rtx_group_vec,
				    store_info->group_id)->frame_related)
		    remove_store = true;
		}

	      if (remove_store)
		{
		  if (dump_file)
		    dump_insn_info ("removing from active", i_ptr);
		  
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
	/* Every other call, including pure functions, may read memory.  */
	add_wild_read (bb_info);

      return;
    }

  /* Assuming that there are sets in these insns, we cannot delete
     them.  */
  if ((GET_CODE (PATTERN (insn)) == CLOBBER)
      || volatile_refs_p (PATTERN (insn))
      || (flag_non_call_exceptions && may_trap_p (PATTERN (insn)))
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

  if (dump_file)
    fprintf (dump_file, "mems_found = %d, cannot_delete = %s\n", 
	     mems_found, insn_info->cannot_delete ? "true" : "false");

  /* If we found some sets of mems, and the insn has not been marked
     cannot delete, add it into the active_local_stores so that it can
     be locally deleted if found dead.  Otherwise mark it as cannot
     delete.  This simplifies the processing later.  */ 
  if (mems_found == 1 && !insn_info->cannot_delete)
    {
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
      store_info_t store_info = insn_info->store_rec;
      bool delete = false;

      /* If ANY of the store_infos match the cselib group that is
	 being deleted, then the insn can not be deleted.  */
      while (store_info)
	{
	  if ((store_info->group_id == -1) 
	      && (store_info->cse_base == base))
	    {
	      delete = true;
	      break;
	    }
	  store_info = store_info->next;
	}

      if (delete)
	{
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

  cselib_init (false);
  all_blocks = BITMAP_ALLOC (NULL);
  bitmap_set_bit (all_blocks, ENTRY_BLOCK);
  bitmap_set_bit (all_blocks, EXIT_BLOCK);

  FOR_ALL_BB (bb)
    {
      insn_info_t ptr;
      bb_info_t bb_info = pool_alloc (bb_info_pool);

      memset (bb_info, 0, sizeof (struct bb_info));
      bitmap_set_bit (all_blocks, bb->index);

      bb_table[bb->index] = bb_info;
      cselib_discard_hook = remove_useless_values;

      if (bb->index >= NUM_FIXED_BLOCKS)
	{
	  rtx insn;

	  cse_store_info_pool
	    = create_alloc_pool ("cse_store_info_pool", 
				 sizeof (struct store_info), 100);
	  active_local_stores = NULL;
	  cselib_clear_table ();
	  
	  /* Scan the insns.  */
	  FOR_BB_INSNS (bb, insn)
	    {
	      if (INSN_P (insn))
		scan_insn (bb_info, insn);
	      cselib_process_insn (insn);
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
		      && single_succ (bb) == EXIT_BLOCK_PTR
		      && ! current_function_calls_eh_return)))
	    {
	      insn_info_t i_ptr = active_local_stores;
	      while (i_ptr)
		{
		  store_info_t store_info = i_ptr->store_rec;

		  /* Skip the clobbers.  */
		  while (!store_info->is_set)
		    store_info = store_info->next;
		  if (store_info->alias_set)
		    delete_dead_store_insn (i_ptr);
		  else 
		    if (store_info->group_id >= 0)
		      {
			group_info_t group 
			  = VEC_index (group_info_t, rtx_group_vec, store_info->group_id);
			if (group->frame_related)
			  delete_dead_store_insn (i_ptr);
		      }

		  i_ptr = i_ptr->next_local_store;
		}
	    }

	  /* Get rid of the loads that were discovered in
	     replace_read.  Cselib is finished with this block.  */
	  while (deferred_change_list)
	    {
	      deferred_change_t next = deferred_change_list->next;

	      /* There is no reason to validate this change.  That was
		 done earlier.  */
	      *deferred_change_list->loc = deferred_change_list->reg;
	      pool_free (deferred_change_pool, deferred_change_list);
	      deferred_change_list = next;
	    }

	  /* Get rid of all of the cselib based store_infos in this
	     block and mark the containing insns as not being
	     deletable.  */
	  ptr = bb_info->last_insn;
	  while (ptr)
	    {
	      if (ptr->contains_cselib_groups)
		free_store_info (ptr);
	      ptr = ptr->prev_insn;
	    }

	  free_alloc_pool (cse_store_info_pool);
	}
    }

  cselib_finish ();
  htab_empty (rtx_group_table);
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
  group_info_t group;

  for (i = 0; VEC_iterate (group_info_t, rtx_group_vec, i, group); i++)
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
	  if (dump_file)
	    fprintf (dump_file, "group %d is frame related ", i); 
	}

      group->offset_map_size_n++;
      group->offset_map_n = XNEWVEC (int, group->offset_map_size_n);
      group->offset_map_size_p++;
      group->offset_map_p = XNEWVEC (int, group->offset_map_size_p);
      group->process_globally = false;
      if (dump_file)
	{
	  fprintf (dump_file, "group %d(%d+%d): ", i, 
		   (int)bitmap_count_bits (group->store2_n),
		   (int)bitmap_count_bits (group->store2_p));
	  bitmap_print (dump_file, group->store2_n, "n ", " ");
	  bitmap_print (dump_file, group->store2_p, "p ", "\n");
	}
    }
}


/* Init the offset tables for the normal case.  */

static bool
dse_step2_nospill (void)
{
  unsigned int i;
  group_info_t group;
  /* Position 0 is unused because 0 is used in the maps to mean
     unused.  */
  current_position = 1;

  for (i = 0; VEC_iterate (group_info_t, rtx_group_vec, i, group); i++)
    {
      bitmap_iterator bi;
      unsigned int j;

      if (group == clear_alias_group)
	continue;

      memset (group->offset_map_n, 0, sizeof(int) * group->offset_map_size_n);
      memset (group->offset_map_p, 0, sizeof(int) * group->offset_map_size_p);
      bitmap_clear (group->group_kill);

      EXECUTE_IF_SET_IN_BITMAP (group->store2_n, 0, j, bi)
	{
	  bitmap_set_bit (group->group_kill, current_position);
	  group->offset_map_n[j] = current_position++;
	  group->process_globally = true;
	}
      EXECUTE_IF_SET_IN_BITMAP (group->store2_p, 0, j, bi)
	{
	  bitmap_set_bit (group->group_kill, current_position); 
	  group->offset_map_p[j] = current_position++;
	  group->process_globally = true;
	}
    }
  return current_position != 1;
}


/* Init the offset tables for the spill case.  */

static bool
dse_step2_spill (void)
{
  unsigned int j;
  group_info_t group = clear_alias_group;
  bitmap_iterator bi;

  /* Position 0 is unused because 0 is used in the maps to mean
     unused.  */
  current_position = 1;

  if (dump_file)
    {
      bitmap_print (dump_file, clear_alias_sets, 
		    "clear alias sets              ", "\n");
      bitmap_print (dump_file, disqualified_clear_alias_sets, 
		    "disqualified clear alias sets ", "\n");
    }

  memset (group->offset_map_n, 0, sizeof(int) * group->offset_map_size_n);
  memset (group->offset_map_p, 0, sizeof(int) * group->offset_map_size_p);
  bitmap_clear (group->group_kill);
  
  /* Remove the disqualified positions from the store2_p set.  */
  bitmap_and_compl_into (group->store2_p, disqualified_clear_alias_sets);
  
  /* We do not need to process the store2_n set because
     alias_sets are always positive.  */
  EXECUTE_IF_SET_IN_BITMAP (group->store2_p, 0, j, bi)
    {
      bitmap_set_bit (group->group_kill, current_position); 
      group->offset_map_p[j] = current_position++;
      group->process_globally = true;
    }

  return current_position != 1;
}



/*----------------------------------------------------------------------------
  Third step.
  
  Build the bit vectors for the transfer functions.
----------------------------------------------------------------------------*/


/* Note that this is NOT a general purpose function.  Any mem that has
   an alias set registered here expected to be COMPLETELY unaliased:
   i.e it's addresses are not and need not be examined.  

   It is known that all references to this address will have this
   alias set and there are NO other references to this address in the
   function.  

   Currently the only place that is known to be clean enough to use
   this interface is the code that assigns the spill locations.  

   All of the mems that have alias_sets registered are subjected to a
   very powerful form of dse where function calls, volatile reads and
   writes, and reads from random location are not taken into account.  

   It is also assumed that these locations go dead when the function
   returns.  This assumption could be relaxed if there were found to
   be places that this assumption was not correct.

   The MODE is passed in and saved.  The mode of each load or store to
   a mem with ALIAS_SET is checked against MEM.  If the size of that
   load or store is different from MODE, processing is halted on this
   alias set.  For the vast majority of aliases sets, all of the loads
   and stores will use the same mode.  But vectors are treated
   differently: the alias set is established for the entire vector,
   but reload will insert loads and stores for individual elements and
   we do not necessarily have the information to track those separate
   elements.  So when we see a mode mismatch, we just bail.  */


void 
dse_record_singleton_alias_set (alias_set_type alias_set, 
				enum machine_mode mode)
{
  struct clear_alias_mode_holder tmp_holder;
  struct clear_alias_mode_holder *entry;
  void **slot;

  /* If we are not going to run dse, we need to return now or there
     will be problems with allocating the bitmaps.  */
  if ((!gate_dse()) || !alias_set)
    return;

  if (!clear_alias_sets)
    {
      clear_alias_sets = BITMAP_ALLOC (NULL);
      disqualified_clear_alias_sets = BITMAP_ALLOC (NULL);
      clear_alias_mode_table = htab_create (11, clear_alias_mode_hash,
					    clear_alias_mode_eq, NULL);
      clear_alias_mode_pool = create_alloc_pool ("clear_alias_mode_pool", 
						 sizeof (struct clear_alias_mode_holder), 100);
    }

  bitmap_set_bit (clear_alias_sets, alias_set);

  tmp_holder.alias_set = alias_set;

  slot = htab_find_slot (clear_alias_mode_table, &tmp_holder, INSERT);
  gcc_assert (*slot == NULL);

  *slot = entry = pool_alloc (clear_alias_mode_pool);
  entry->alias_set = alias_set;
  entry->mode = mode;
}


/* Remove ALIAS_SET from the sets of stack slots being considered.  */

void 
dse_invalidate_singleton_alias_set (alias_set_type alias_set)
{
  if ((!gate_dse()) || !alias_set)
    return;

  bitmap_clear_bit (clear_alias_sets, alias_set);
}


/* Look up the bitmap index for OFFSET in GROUP_INFO.  If it is not
   there, return 0.  */

static int
get_bitmap_index (group_info_t group_info, HOST_WIDE_INT offset)
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
scan_stores_nospill (store_info_t store_info, bitmap gen, bitmap kill)
{
  while (store_info)
    {
      HOST_WIDE_INT i;
      group_info_t group_info 
	= VEC_index (group_info_t, rtx_group_vec, store_info->group_id);
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


/* Process the STORE_INFOs into the bitmaps into GEN and KILL.  KILL
   may be NULL. */

static void 
scan_stores_spill (store_info_t store_info, bitmap gen, bitmap kill)
{
  while (store_info)
    {
      if (store_info->alias_set)
	{
	  int index = get_bitmap_index (clear_alias_group, 
					store_info->alias_set);
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
scan_reads_nospill (insn_info_t insn_info, bitmap gen, bitmap kill)
{
  read_info_t read_info = insn_info->read_rec;
  int i;
  group_info_t group;

  /* If this insn reads the frame, kill all the frame related stores.  */
  if (insn_info->frame_read)
    {
      for (i = 0; VEC_iterate (group_info_t, rtx_group_vec, i, group); i++)
	if (group->process_globally && group->frame_related)
	  {
	    if (kill)
	      bitmap_ior_into (kill, group->group_kill);
	    bitmap_and_compl_into (gen, group->group_kill); 
	  }
    }

  while (read_info)
    {
      for (i = 0; VEC_iterate (group_info_t, rtx_group_vec, i, group); i++)
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
						QImode,
						group->canon_base_mem,
						read_info->mem, rtx_varies_p))
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

/* Process the READ_INFOs into the bitmaps into GEN and KILL.  KILL
   may be NULL.  */

static void
scan_reads_spill (read_info_t read_info, bitmap gen, bitmap kill)
{
  while (read_info)
    {
      if (read_info->alias_set)
	{
	  int index = get_bitmap_index (clear_alias_group, 
					read_info->alias_set);
	  if (index != 0)
	    {
	      if (kill)
		bitmap_set_bit (kill, index);
	      bitmap_clear_bit (gen, index);
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
dse_step3_scan (bool for_spills, basic_block bb)
{
  bb_info_t bb_info = bb_table[bb->index];
  insn_info_t insn_info;

  if (for_spills)
    /* There are no wild reads in the spill case.  */
    insn_info = bb_info->last_insn;
  else
    insn_info = find_insn_before_first_wild_read (bb_info);
    
  /* In the spill case or in the no_spill case if there is no wild
     read in the block, we will need a kill set.  */
  if (insn_info == bb_info->last_insn)
    {
      if (bb_info->kill)
	bitmap_clear (bb_info->kill);
      else
	bb_info->kill = BITMAP_ALLOC (NULL);
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
	  /* Process the read(s) last.  */ 
	  if (for_spills)
	    {
	      scan_stores_spill (insn_info->store_rec, bb_info->gen, bb_info->kill);
	      scan_reads_spill (insn_info->read_rec, bb_info->gen, bb_info->kill);
	    }
	  else
	    {
	      scan_stores_nospill (insn_info->store_rec, bb_info->gen, bb_info->kill);
	      scan_reads_nospill (insn_info, bb_info->gen, bb_info->kill);
	    }
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
      group_info_t group;
      
      for (i = 0; VEC_iterate (group_info_t, rtx_group_vec, i, group); i++)
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

  if (TEST_BIT (unreachable_blocks, bb->index))
    {
      RESET_BIT (unreachable_blocks, bb->index);
      FOR_EACH_EDGE (e, ei, bb->preds)
	{								
	  mark_reachable_blocks (unreachable_blocks, e->src);
	}								
    }
}

/* Build the transfer functions for the function.  */

static void
dse_step3 (bool for_spills)
{
  basic_block bb;
  sbitmap unreachable_blocks = sbitmap_alloc (last_basic_block);
  sbitmap_iterator sbi;
  bitmap all_ones = NULL;
  unsigned int i;
  
  sbitmap_ones (unreachable_blocks);

  FOR_ALL_BB (bb)
    {
      bb_info_t bb_info = bb_table[bb->index];
      if (bb_info->gen)
	bitmap_clear (bb_info->gen);
      else
	bb_info->gen = BITMAP_ALLOC (NULL);

      if (bb->index == ENTRY_BLOCK)
	;
      else if (bb->index == EXIT_BLOCK)
	dse_step3_exit_block_scan (bb_info);
      else
	dse_step3_scan (for_spills, bb);
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
  EXECUTE_IF_SET_IN_SBITMAP (unreachable_blocks, 0, i, sbi)
    {
      if (bitmap_bit_p (all_blocks, i))
	{
	  bb_info_t bb_info = bb_table[i];
	  if (!all_ones)
	    {
	      unsigned int j;
	      group_info_t group;

	      all_ones = BITMAP_ALLOC (NULL);
	      for (j = 0; VEC_iterate (group_info_t, rtx_group_vec, j, group); j++)
		bitmap_ior_into (all_ones, group->group_kill);
	    }
	  if (!bb_info->out)
	    {
	      bb_info->out = BITMAP_ALLOC (NULL);
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
      bb_info->out = BITMAP_ALLOC (NULL);
      bitmap_copy (bb_info->out, bb_table[EXIT_BLOCK]->gen);
    }
}

/* Propagate the information from the in set of the dest of E to the
   out set of the src of E.  If the various in or out sets are not
   there, that means they are all ones.  */

static void
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
	  src_info->out = BITMAP_ALLOC (NULL);
	  bitmap_copy (src_info->out, dest_info->in);
	}
    }
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
	      bb_info->in = BITMAP_ALLOC (NULL);
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
	  bb_info->in = BITMAP_ALLOC (NULL);
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
  if (dump_file)
    {
      basic_block bb;

      fprintf (dump_file, "\n\n*** Global dataflow info after analysis.\n");
      FOR_ALL_BB (bb)
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
dse_step5_nospill (void)
{
  basic_block bb;
  FOR_EACH_BB (bb)
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
	      store_info_t store_info = insn_info->store_rec;

	      /* Try to delete the current insn.  */
	      deleted = true;
	      
	      /* Skip the clobbers.  */
	      while (!store_info->is_set)
		store_info = store_info->next;

	      if (store_info->alias_set)
		deleted = false;
	      else
		{
		  HOST_WIDE_INT i;
		  group_info_t group_info 
		    = VEC_index (group_info_t, rtx_group_vec, store_info->group_id);
		  
		  for (i = store_info->begin; i < store_info->end; i++)
		    {
		      int index = get_bitmap_index (group_info, i);
		      
		      if (dump_file)
			fprintf (dump_file, "i = %d, index = %d\n", (int)i, index); 
		      if (index == 0 || !bitmap_bit_p (v, index))
			{
			  if (dump_file)
			    fprintf (dump_file, "failing at i = %d\n", (int)i); 
			  deleted = false;
			  break;
			}
		    }
		}
	      if (deleted)
		{
		  if (dbg_cnt (dse))
		    {
		      check_for_inc_dec (insn_info->insn);
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
	      scan_stores_nospill (insn_info->store_rec, v, NULL);
	      if (insn_info->wild_read)
		{
		  if (dump_file)
		    fprintf (dump_file, "wild read\n");
		  bitmap_clear (v);
		}
	      else if (insn_info->read_rec)
		{
		  if (dump_file)
		    fprintf (dump_file, "regular read\n");
		  scan_reads_nospill (insn_info, v, NULL);
		}
	    }
	      
	  insn_info = insn_info->prev_insn;
	}
    }
}


static void
dse_step5_spill (void)
{
  basic_block bb;
  FOR_EACH_BB (bb)
    {
      bb_info_t bb_info = bb_table[bb->index];
      insn_info_t insn_info = bb_info->last_insn;
      bitmap v = bb_info->out;

      while (insn_info)
	{
	  bool deleted = false;
	  /* There may have been code deleted by the dce pass run before
	     this phase.  */
	  if (insn_info->insn 
	      && INSN_P (insn_info->insn)
	      && (!insn_info->cannot_delete)
	      && (!bitmap_empty_p (v)))
	    {
	      /* Try to delete the current insn.  */
	      store_info_t store_info = insn_info->store_rec;
	      deleted = true;
	      
	      while (store_info)
		{
		  if (store_info->alias_set)
		    {
		      int index = get_bitmap_index (clear_alias_group, 
						    store_info->alias_set);
		      if (index == 0 || !bitmap_bit_p (v, index))
			{
			  deleted = false;
			  break;
			}
		    }
		  else 
		    deleted = false;
		  store_info = store_info->next;
		}
	      if (deleted && dbg_cnt (dse))
		{
		  if (dump_file)
		    fprintf (dump_file, "Spill deleting insn %d\n", 
			     INSN_UID (insn_info->insn));
		  check_for_inc_dec (insn_info->insn);
		  delete_insn (insn_info->insn);
		  spill_deleted++;
		  insn_info->insn = NULL;
		}
	    }
	  
	  if (insn_info->insn 
	      && INSN_P (insn_info->insn)
	      && (!deleted))
	    {
	      scan_stores_spill (insn_info->store_rec, v, NULL);
	      scan_reads_spill (insn_info->read_rec, v, NULL);
	    }
	      
	  insn_info = insn_info->prev_insn;
	}
    }
}



/*----------------------------------------------------------------------------
   Sixth step.

   Destroy everything left standing. 
----------------------------------------------------------------------------*/

static void 
dse_step6 (bool global_done)
{
  unsigned int i;
  group_info_t group;
  basic_block bb;
  
  for (i = 0; VEC_iterate (group_info_t, rtx_group_vec, i, group); i++)
    {
      free (group->offset_map_n);
      free (group->offset_map_p);
      BITMAP_FREE (group->store1_n);
      BITMAP_FREE (group->store1_p);
      BITMAP_FREE (group->store2_n);
      BITMAP_FREE (group->store2_p);
      BITMAP_FREE (group->group_kill);
    }

  if (global_done)
    FOR_ALL_BB (bb)
      {
	bb_info_t bb_info = bb_table[bb->index];
	BITMAP_FREE (bb_info->gen);
	if (bb_info->kill)
	  BITMAP_FREE (bb_info->kill);
	if (bb_info->in)
	  BITMAP_FREE (bb_info->in);
	if (bb_info->out)
	  BITMAP_FREE (bb_info->out);
      }

  if (clear_alias_sets)
    {
      BITMAP_FREE (clear_alias_sets);
      BITMAP_FREE (disqualified_clear_alias_sets);
      free_alloc_pool (clear_alias_mode_pool);
      htab_delete (clear_alias_mode_table);
    }

  end_alias_analysis ();
  free (bb_table);
  htab_delete (rtx_group_table);
  VEC_free (group_info_t, heap, rtx_group_vec);
  BITMAP_FREE (all_blocks);
  BITMAP_FREE (scratch);

  free_alloc_pool (rtx_store_info_pool);
  free_alloc_pool (read_info_pool);
  free_alloc_pool (insn_info_pool);
  free_alloc_pool (bb_info_pool);
  free_alloc_pool (rtx_group_info_pool);
  free_alloc_pool (deferred_change_pool);
}


/* -------------------------------------------------------------------------
   DSE
   ------------------------------------------------------------------------- */

/* Callback for running pass_rtl_dse.  */

static unsigned int
rest_of_handle_dse (void)
{
  bool did_global = false;

  df_set_flags (DF_DEFER_INSN_RESCAN);

  dse_step0 ();
  dse_step1 ();
  dse_step2_init ();
  if (dse_step2_nospill ())
    {
      df_set_flags (DF_LR_RUN_DCE);
      df_analyze ();
      did_global = true;
      if (dump_file)
	fprintf (dump_file, "doing global processing\n");
      dse_step3 (false);
      dse_step4 ();
      dse_step5_nospill ();
    }

  /* For the instance of dse that runs after reload, we make a special
     pass to process the spills.  These are special in that they are
     totally transparent, i.e, there is no aliasing issues that need
     to be considered.  This means that the wild reads that kill
     everything else do not apply here.  */ 
  if (clear_alias_sets && dse_step2_spill ())
    {
      if (!did_global)
	{
	  df_set_flags (DF_LR_RUN_DCE);
	  df_analyze ();
	}
      did_global = true;
      if (dump_file)
	fprintf (dump_file, "doing global spill processing\n");
      dse_step3 (true);
      dse_step4 ();
      dse_step5_spill ();
    }
  
  dse_step6 (did_global);

  if (dump_file)
    fprintf (dump_file, "dse: local deletions = %d, global deletions = %d, spill deletions = %d\n",
	     locally_deleted, globally_deleted, spill_deleted);
  return 0;
}

static bool
gate_dse (void)
{
  return optimize > 0 && flag_dse;
}

struct tree_opt_pass pass_rtl_dse1 =
{
  "dse1",                               /* name */
  gate_dse,                             /* gate */
  rest_of_handle_dse,                   /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_DSE1,                              /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_dump_func |
  TODO_df_finish | TODO_verify_rtl_sharing |
  TODO_ggc_collect,                     /* todo_flags_finish */
  'w'                                   /* letter */
};

struct tree_opt_pass pass_rtl_dse2 =
{
  "dse2",                               /* name */
  gate_dse,                             /* gate */
  rest_of_handle_dse,                   /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_DSE2,                              /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_dump_func |
  TODO_df_finish | TODO_verify_rtl_sharing |
  TODO_ggc_collect,                     /* todo_flags_finish */
  'w'                                   /* letter */
};
