/* LTO partitioning logic routines.
   Copyright (C) 2009-2024 Free Software Foundation, Inc.

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

#include "config.h"
#define INCLUDE_MEMORY
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "function.h"
#include "basic-block.h"
#include "tree.h"
#include "gimple.h"
#include "alloc-pool.h"
#include "stringpool.h"
#include "cgraph.h"
#include "lto-streamer.h"
#include "symbol-summary.h"
#include "tree-vrp.h"
#include "sreal.h"
#include "ipa-cp.h"
#include "ipa-prop.h"
#include "ipa-fnsummary.h"
#include "lto-partition.h"

#include <limits>

vec<ltrans_partition> ltrans_partitions;

static void add_symbol_to_partition (ltrans_partition part, symtab_node *node);


/* Helper for qsort; compare partitions and return one with smaller order.  */

static int
cmp_partitions_order (const void *a, const void *b)
{
  const struct ltrans_partition_def *pa
     = *(struct ltrans_partition_def *const *)a;
  const struct ltrans_partition_def *pb
     = *(struct ltrans_partition_def *const *)b;
  int ordera = -1, orderb = -1;

  if (lto_symtab_encoder_size (pa->encoder))
    ordera = lto_symtab_encoder_deref (pa->encoder, 0)->order;
  if (lto_symtab_encoder_size (pb->encoder))
    orderb = lto_symtab_encoder_deref (pb->encoder, 0)->order;
  return orderb - ordera;
}

/* Create new partition with name NAME.
   Does not push into ltrans_partitions.  */
static ltrans_partition
new_partition_no_push (const char *name)
{
  ltrans_partition part = XCNEW (struct ltrans_partition_def);
  part->encoder = lto_symtab_encoder_new (false);
  part->name = name;
  part->insns = 0;
  part->symbols = 0;
  return part;
}

/* Create new partition with name NAME.  */

static ltrans_partition
new_partition (const char *name)
{
  ltrans_partition part = new_partition_no_push (name);
  ltrans_partitions.safe_push (part);
  return part;
}

/* Free memory used by ltrans partition.
   Encoder can be kept to be freed after streaming.  */
static void
free_ltrans_partition (ltrans_partition part, bool delete_encoder)
{
  if (part->initializers_visited)
    delete part->initializers_visited;
  if (delete_encoder)
    lto_symtab_encoder_delete (part->encoder);
  free (part);
}

/* Free memory used by ltrans datastructures.  */

void
free_ltrans_partitions (void)
{
  unsigned int idx;
  ltrans_partition part;
  for (idx = 0; ltrans_partitions.iterate (idx, &part); idx++)
    free_ltrans_partition (part, false);
  ltrans_partitions.release ();
}

/* Return true if symbol is already in some partition.  */

static inline bool
symbol_partitioned_p (symtab_node *node)
{
  return node->aux;
}

/* Add references into the partition.  */
static void
add_references_to_partition (ltrans_partition part, symtab_node *node)
{
  int i;
  struct ipa_ref *ref = NULL;

  /* Add all duplicated references to the partition.  */
  for (i = 0; node->iterate_reference (i, ref); i++)
    if (ref->referred->get_partitioning_class () == SYMBOL_DUPLICATE)
      add_symbol_to_partition (part, ref->referred);
    /* References to a readonly variable may be constant foled into its value.
       Recursively look into the initializers of the constant variable and add
       references, too.  */
    else if (is_a <varpool_node *> (ref->referred)
	     && (dyn_cast <varpool_node *> (ref->referred)
		 ->ctor_useable_for_folding_p ())
	     && !lto_symtab_encoder_in_partition_p (part->encoder, ref->referred))
      {
	if (!part->initializers_visited)
	  part->initializers_visited = new hash_set<symtab_node *>;
	if (!part->initializers_visited->add (ref->referred))
	  add_references_to_partition (part, ref->referred);
      }
}

/* Helper function for add_symbol_to_partition doing the actual dirty work
   of adding NODE to PART.  */

static bool
add_symbol_to_partition_1 (ltrans_partition part, symtab_node *node)
{
  enum symbol_partitioning_class c = node->get_partitioning_class ();
  struct ipa_ref *ref;
  symtab_node *node1;

  /* If NODE is already there, we have nothing to do.  */
  if (lto_symtab_encoder_in_partition_p (part->encoder, node))
    return true;

  /* non-duplicated aliases or tunks of a duplicated symbol needs to be output
     just once.

     Be lax about comdats; they may or may not be duplicated and we may
     end up in need to duplicate keyed comdat because it has unkeyed alias.  */
  if (c == SYMBOL_PARTITION && !DECL_COMDAT (node->decl)
      && symbol_partitioned_p (node))
    return false;

  /* Be sure that we never try to duplicate partitioned symbol
     or add external symbol.  */
  gcc_assert (c != SYMBOL_EXTERNAL
	      && (c == SYMBOL_DUPLICATE || !symbol_partitioned_p (node)));

  part->symbols++;

  lto_set_symtab_encoder_in_partition (part->encoder, node);

  if (symbol_partitioned_p (node))
    {
      node->in_other_partition = 1;
      if (dump_file)
	fprintf (dump_file,
		 "Symbol node %s now used in multiple partitions\n",
		 node->dump_name ());
    }
  node->aux = (void *)((size_t)node->aux + 1);

  if (cgraph_node *cnode = dyn_cast <cgraph_node *> (node))
    {
      struct cgraph_edge *e;
      if (!node->alias && c == SYMBOL_PARTITION)
	part->insns += ipa_size_summaries->get (cnode)->size;

      /* Add all inline clones and callees that are duplicated.  */
      for (e = cnode->callees; e; e = e->next_callee)
	if (!e->inline_failed)
	  add_symbol_to_partition_1 (part, e->callee);
	else if (e->callee->get_partitioning_class () == SYMBOL_DUPLICATE)
	  add_symbol_to_partition (part, e->callee);

      /* Add all thunks associated with the function.  */
      for (e = cnode->callers; e; e = e->next_caller)
	if (e->caller->thunk && !e->caller->inlined_to)
	  add_symbol_to_partition_1 (part, e->caller);
    }

  add_references_to_partition (part, node);

  /* Add all aliases associated with the symbol.  */

  FOR_EACH_ALIAS (node, ref)
    if (!ref->referring->transparent_alias)
      add_symbol_to_partition_1 (part, ref->referring);
    else
      {
	struct ipa_ref *ref2;
	/* We do not need to add transparent aliases if they are not used.
	   However we must add aliases of transparent aliases if they exist.  */
	FOR_EACH_ALIAS (ref->referring, ref2)
	  {
	    /* Nested transparent aliases are not permitted.  */
	    gcc_checking_assert (!ref2->referring->transparent_alias);
	    add_symbol_to_partition_1 (part, ref2->referring);
	  }
      }

  /* Ensure that SAME_COMDAT_GROUP lists all allways added in a group.  */
  if (node->same_comdat_group)
    for (node1 = node->same_comdat_group;
	 node1 != node; node1 = node1->same_comdat_group)
      if (!node->alias)
	{
	  bool added = add_symbol_to_partition_1 (part, node1);
	  gcc_assert (added);
	}
  return true;
}

/* If symbol NODE is really part of other symbol's definition (i.e. it is
   internal label, thunk, alias or so), return the outer symbol.
   When add_symbol_to_partition_1 is called on the outer symbol it must
   eventually add NODE, too.  */
static symtab_node *
contained_in_symbol (symtab_node *node)
{
  /* There is no need to consider transparent aliases to be part of the
     definition: they are only useful insite the partition they are output
     and thus we will always see an explicit reference to it.  */
  if (node->transparent_alias)
    return node;
  if (cgraph_node *cnode = dyn_cast <cgraph_node *> (node))
    {
      cnode = cnode->function_symbol ();
      if (cnode->inlined_to)
	cnode = cnode->inlined_to;
      return cnode;
    }
  else if (varpool_node *vnode = dyn_cast <varpool_node *> (node))
    return vnode->ultimate_alias_target ();
  return node;
}

/* Add symbol NODE to partition.  When definition of NODE is part
   of other symbol definition, add the other symbol, too.  */

static void
add_symbol_to_partition (ltrans_partition part, symtab_node *node)
{
  symtab_node *node1;

  /* Verify that we do not try to duplicate something that cannot be.  */
  gcc_checking_assert (node->get_partitioning_class () == SYMBOL_DUPLICATE
		       || !symbol_partitioned_p (node));

  while ((node1 = contained_in_symbol (node)) != node)
    node = node1;

  /* If we have duplicated symbol contained in something we cannot duplicate,
     we are very badly screwed.  The other way is possible, so we do not
     assert this in add_symbol_to_partition_1.

     Be lax about comdats; they may or may not be duplicated and we may
     end up in need to duplicate keyed comdat because it has unkeyed alias.  */

  gcc_assert (node->get_partitioning_class () == SYMBOL_DUPLICATE
	      || DECL_COMDAT (node->decl)
	      || !symbol_partitioned_p (node));

  add_symbol_to_partition_1 (part, node);
}

/* Undo all additions until number of cgraph nodes in PARITION is N_CGRAPH_NODES
   and number of varpool nodes is N_VARPOOL_NODES.  */

static void
undo_partition (ltrans_partition partition, unsigned int n_nodes)
{
  while (lto_symtab_encoder_size (partition->encoder) > (int)n_nodes)
    {
      symtab_node *node = lto_symtab_encoder_deref (partition->encoder,
						   n_nodes);
      partition->symbols--;
      cgraph_node *cnode;

      /* After UNDO we no longer know what was visited.  */
      if (partition->initializers_visited)
	delete partition->initializers_visited;
      partition->initializers_visited = NULL;

      if (!node->alias && (cnode = dyn_cast <cgraph_node *> (node))
          && node->get_partitioning_class () == SYMBOL_PARTITION)
	partition->insns -= ipa_size_summaries->get (cnode)->size;
      lto_symtab_encoder_delete_node (partition->encoder, node);
      node->aux = (void *)((size_t)node->aux - 1);
    }
}

/* Group cgrah nodes by input files.  This is used mainly for testing
   right now.  */

void
lto_1_to_1_map (void)
{
  symtab_node *node;
  struct lto_file_decl_data *file_data;
  hash_map<lto_file_decl_data *, ltrans_partition> pmap;
  ltrans_partition partition;
  int npartitions = 0;

  FOR_EACH_SYMBOL (node)
    {
      if (node->get_partitioning_class () != SYMBOL_PARTITION
	  || symbol_partitioned_p (node))
	continue;

      file_data = node->lto_file_data;

      if (file_data)
	{
          ltrans_partition *slot = &pmap.get_or_insert (file_data);
          if (*slot)
	    partition = *slot;
	  else
	    {
	      partition = new_partition (file_data->file_name);
	      *slot = partition;
	      npartitions++;
	    }
	}
      else if (!file_data && ltrans_partitions.length ())
	partition = ltrans_partitions[0];
      else
	{
	  partition = new_partition ("");
	  npartitions++;
	}

      add_symbol_to_partition (partition, node);
    }

  /* If the cgraph is empty, create one cgraph node set so that there is still
     an output file for any variables that need to be exported in a DSO.  */
  if (!npartitions)
    new_partition ("empty");

  /* Order partitions by order of symbols because they are linked into binary
     that way.  */
  ltrans_partitions.qsort (cmp_partitions_order);
}

/* Maximal partitioning.  Put every new symbol into new partition if possible.  */

void
lto_max_map (void)
{
  symtab_node *node;
  ltrans_partition partition;
  int npartitions = 0;

  FOR_EACH_SYMBOL (node)
    {
      if (node->get_partitioning_class () != SYMBOL_PARTITION
	  || symbol_partitioned_p (node))
	continue;
      partition = new_partition (node->asm_name ());
      add_symbol_to_partition (partition, node);
      npartitions++;
    }
  if (!npartitions)
    new_partition ("empty");
}

/* Helper function for qsort; sort nodes by order.  */
static int
node_cmp (const void *pa, const void *pb)
{
  const symtab_node *a = *static_cast<const symtab_node * const *> (pa);
  const symtab_node *b = *static_cast<const symtab_node * const *> (pb);
  return b->order - a->order;
}

/* Add all symtab nodes from NEXT_NODE to PARTITION in order.  */

static void
add_sorted_nodes (vec<symtab_node *> &next_nodes, ltrans_partition partition)
{
  unsigned i;
  symtab_node *node;

  next_nodes.qsort (node_cmp);
  FOR_EACH_VEC_ELT (next_nodes, i, node)
    if (!symbol_partitioned_p (node))
      add_symbol_to_partition (partition, node);
}

/* Return true if we should account reference from N1 to N2 in cost
   of partition boundary.  */

bool
account_reference_p (symtab_node *n1, symtab_node *n2)
{
  if (cgraph_node *cnode = dyn_cast <cgraph_node *> (n1))
    n1 = cnode;
  /* Do not account references from aliases - they are never split across
     partitions.  */
  if (n1->alias)
    return false;
  /* Do not account recursion - the code below will handle it incorrectly
     otherwise.  Do not account references to external symbols: they will
     never become local.  Finally do not account references to duplicated
     symbols: they will be always local.  */
  if (n1 == n2
      || !n2->definition
      || n2->get_partitioning_class () != SYMBOL_PARTITION)
    return false;
  /* If referring node is external symbol do not account it to boundary
     cost. Those are added into units only to enable possible constant
     folding and devirtulization.

     Here we do not know if it will ever be added to some partition
     (this is decided by compute_ltrans_boundary) and second it is not
     that likely that constant folding will actually use the reference.  */
  if (contained_in_symbol (n1)
	->get_partitioning_class () == SYMBOL_EXTERNAL)
    return false;
  return true;
}

/* Joins two partitions into one.
   NULL partitions are equivalent to empty partition.
   If both partition are non-null, symbols from FROM are added into INTO.  */
static ltrans_partition
join_partitions (ltrans_partition into, ltrans_partition from)
{
  if (!into)
    return from;
  if (!from)
    return into;

  lto_symtab_encoder_iterator lsei;
  lto_symtab_encoder_t encoder = from->encoder;

  /* If aux is non zero, it will not be added to the new partition.  Since
     adding symbols is recursive, it is safer to reduce aux of all symbols
     before adding any symbols to other partition.  */
  for (lsei = lsei_start (encoder); !lsei_end_p (lsei); lsei_next (&lsei))
    {
      symtab_node *node = lsei_node (lsei);
      node->aux = (void *)((size_t)node->aux - 1);
    }

  for (lsei = lsei_start (encoder); !lsei_end_p (lsei); lsei_next (&lsei))
    {
      symtab_node *node = lsei_node (lsei);

      if (symbol_partitioned_p (node))
	continue;

      add_symbol_to_partition (into, node);
    }

  free_ltrans_partition (from, true);

  return into;
}

/* Takes symbols from given partitions and splits them into N partitions where
   each partitions contains one symbol and its requirements.  */
static std::vector<ltrans_partition>
split_partition_into_nodes (ltrans_partition part)
{
  std::vector<ltrans_partition> partitions;

  lto_symtab_encoder_iterator lsei;
  lto_symtab_encoder_t encoder = part->encoder;

  for (lsei = lsei_start (encoder); !lsei_end_p (lsei); lsei_next (&lsei))
    {
      symtab_node *node = lsei_node (lsei);
      node->aux = (void *)((size_t)node->aux - 1);
    }

  for (lsei = lsei_start (encoder); !lsei_end_p (lsei); lsei_next (&lsei))
    {
      symtab_node *node = lsei_node (lsei);

      if (node->get_partitioning_class () != SYMBOL_PARTITION
	  || symbol_partitioned_p (node))
	continue;

      ltrans_partition new_part = new_partition_no_push (part->name);
      add_symbol_to_partition (new_part, node);
      partitions.push_back (new_part);
    }

  return partitions;
}

/* Returns whether partition contains symbols that cannot be reordered.  */
static bool
is_partition_reorder (ltrans_partition part)
{
  lto_symtab_encoder_iterator lsei;
  lto_symtab_encoder_t encoder = part->encoder;

  for (lsei = lsei_start (encoder); !lsei_end_p (lsei); lsei_next (&lsei))
    {
      symtab_node *node = lsei_node (lsei);
      if (node->no_reorder)
	return false;
    }
  return true;
}

/* Represents groups of symbols, that should be partitioned into n_partitions
   partitions.  */
class partition_set
{
public:
  /* Metadata to easily pass copy to new partition_set.  */
  class metadata
  {
  public:
    /* Partitions can be reordered.  */
    bool reorder;
    /* Partitions can be split into individual symbols.  */
    bool splitable;

    metadata (bool reorder, bool splitable):
      reorder (reorder), splitable (splitable)
    {}
  };
  metadata data;

  /* Symbol groups.  Use push (g) to insert symbols.  */
  std::vector<ltrans_partition> sym_groups;
  /* Number of partitions these symbols should be partitioned into.  */
  size_t n_partitions;
  /* Total number of instructions of all symbols.  */
  int64_t insns;

  /* Constructor.  Symbols and n_partitions can be added later.  */
  partition_set (metadata data, std::vector<ltrans_partition> sym_groups = {},
		 size_t n_partitions = 0)
    : data (data), sym_groups (std::move (sym_groups)),
    n_partitions (n_partitions), insns (0)
  {
    for (ltrans_partition g: this->sym_groups)
      insns += g->insns;
  }

  /* Adds symbol group and updates total insns.  */
  void
  push (ltrans_partition g)
  {
    sym_groups.push_back (g);
    insns += g->insns;
  }

  /* Returns whether any symbols group is contained.  */
  bool
  empty ()
  {
    return sym_groups.empty ();
  }
};

/* Distributes total n_partitions among partition_sets.
   Aims to be as fair as possible.  */
static void
distribute_n_partitions (std::vector<partition_set>& ps, size_t n_partitions)
{
  gcc_assert (ps.size ());
  gcc_assert (ps.size () <= n_partitions);

  int64_t total_size = 0;

  for (partition_set& p: ps)
    {
      total_size += p.insns;
      p.n_partitions = 0;
    }

  if (total_size <= 0)
    total_size = 1;

  size_t n_partitions_allocated = 0;

  /* First we allocate largest amount of partitions so that target_sizes are
     larger than target size of total (insns/total_size).
     All partition_set must have n_partitions at least one.  */
  for (partition_set& p: ps)
    {
      p.n_partitions = n_partitions * p.insns / total_size;
      if (p.n_partitions == 0 && p.sym_groups.size ())
	p.n_partitions = 1;
      if (!p.data.splitable)
	p.n_partitions = std::min (p.n_partitions, p.sym_groups.size ());

      n_partitions_allocated += p.n_partitions;
    }

  /* Rare special case, with a lot of initially 0 sized splits.  */
  while (n_partitions_allocated > n_partitions)
    {
      size_t idx = 0;
      int64_t min = std::numeric_limits<int64_t>::max ();

      for (size_t i = 0; i < ps.size (); ++i)
	{
	  if (ps[i].n_partitions <= 1)
	    continue;

	  int64_t target_size = ps[i].insns / ps[i].n_partitions;
	  if (min > target_size)
	    {
	      min = target_size;
	      idx = i;
	    }
	}

      ps[idx].n_partitions--;
      n_partitions_allocated--;
    }

  /* Typical case where with any increase of n_partitions target size will cross
     total target size.  We optimize for minimal:
     (old_target_size - total_target_size)
     - (total_target_size - new_target_size).  */
  while (n_partitions_allocated < n_partitions)
    {
      size_t idx = 0;
      int64_t max = 0;

      for (size_t i = 0; i < ps.size (); ++i)
	{
	  if (ps[i].sym_groups.size () <= 1 && !ps[i].data.splitable)
	    continue;

	  int64_t target_size = ps[i].insns / ps[i].n_partitions;
	  int64_t new_target_size = ps[i].insns / (ps[i].n_partitions + 1);

	  int64_t positive_change = target_size + new_target_size;

	  if (max < positive_change)
	    {
	      max = positive_change;
	      idx = i;
	    }
	}

      ps[idx].n_partitions++;
      n_partitions_allocated++;
    }
}

/* Splits off symbol groups that are larger than target size.
   n_partitions are then distributed between individual
   split off symbol groups, and everything else as a whole.

   Split off symbol groups with n_partitions > 1, are
   then split into individual symbols.

   Order is not conserved.  This pass is ignored if reorder is not allowed.  */
static std::vector<partition_set>
partition_over_target_split (partition_set& p)
{
  gcc_assert (p.n_partitions >= 1);

  std::vector<partition_set> all;
  partition_set small (p.data);

  int64_t target_size = p.insns / p.n_partitions;

  for (ltrans_partition g: p.sym_groups)
    {
      if (g->insns > target_size
	  && (p.data.reorder || is_partition_reorder (g)))
	all.push_back (partition_set (p.data, {g}));
      else
	small.push (g);
    }

  if (all.empty ())
    return {};

  if (small.sym_groups.size ())
    {
      /* Handles special case where n_partitions might be smaller than
	 all.size ().  Which can happen as result of interger division or with
	 0 sized partition_sets.  Then also prevents too small symbol group.
	 This should also be a special case; more common one,
	 but with no correctness problems.  */
      if (all.size () && (
	     small.insns < (int64_t) p.n_partitions
	  || small.insns < target_size * 0.6
	  || small.insns < param_min_partition_size))
	{
	  size_t idx = 0;
	  int64_t min_insns = std::numeric_limits<int64_t>::max ();
	  for (size_t i = 0; i < all.size (); ++i)
	    {
	      if (all[i].insns < min_insns)
		{
		  min_insns = all[i].insns;
		  idx = i;
		}
	    }

	  gcc_assert (all[idx].sym_groups.size () == 1);

	  ltrans_partition& into = all[idx].sym_groups[0];
	  for (ltrans_partition g: small.sym_groups)
	    into = join_partitions (into, g);

	  all[idx].insns = into->insns;
	}
      else
	{
	  gcc_assert (all.size () < p.n_partitions);
	  all.push_back (std::move (small));
	}
    }

  distribute_n_partitions (all, p.n_partitions);

  for (partition_set& p: all)
    {
      gcc_assert (p.sym_groups.size ());

      /* Handles large symbol groups (large files) that will be
	 further divided.  */
      if (p.sym_groups.size () == 1 && p.n_partitions > 1)
	{
	  p.sym_groups = split_partition_into_nodes (p.sym_groups[0]);
	  p.data.reorder = false;
	  p.data.splitable = false;
	}
    }

  return all;
}

/* Splits partition_set into two partition_sets with
   equal or off by one n_partitions.
   Order is conserved.  */
static std::vector<partition_set>
partition_binary_split (partition_set& p)
{
  gcc_assert (p.n_partitions > 1);

  if (p.sym_groups.size () < 2)
    return {};

  int64_t target_size = p.insns / p.n_partitions;


  std::vector<partition_set> result (2, partition_set (p.data));
  partition_set& first  = result[0];
  partition_set& second = result[1];

  first.n_partitions = p.n_partitions/2;
  second.n_partitions = p.n_partitions - first.n_partitions;

  int64_t first_target_size = first.n_partitions * target_size;

  int64_t insns = 0;
  for (ltrans_partition g: p.sym_groups)
    {
      /* We want at least one symbol in first partition.  */
      if (first.empty ())
	first.push (g);
      else if (insns < first_target_size)
	{
	  if (insns + g->insns < first_target_size)
	    first.push (g);
	  else
	    {
	      /* Target splitting point is in this symbol group.  */
	      int64_t diff_first  = first_target_size - insns;
	      int64_t diff_second = (insns + g->insns) - first_target_size;

	      if (diff_first * second.n_partitions
		  > diff_second * first.n_partitions)
		first.push (g);
	      else
		second.push (g);
	    }
	}
      else
	second.push (g);

      insns += g->insns;
    }

  return result;
}

/* Split partition_set into 'into' partition_sets with equal or off by one
   number of symbol groups.  Sizes of symbol groups are ignored for deciding
   where to split. n_partitions is then distributed among new partition_sets
   based on their sizes.
   Order in conserved.  */
static std::vector<partition_set>
partition_fixed_split (partition_set& p, size_t into)
{
  gcc_assert (into < p.n_partitions);

  std::vector<partition_set> result;

  for (size_t i = 0; i < into; ++i)
    {
      size_t begin = i * p.sym_groups.size () / into;
      size_t end = (i + 1) * p.sym_groups.size () / into;

      auto it = p.sym_groups.begin ();
      result.push_back (partition_set (p.data, {it + begin, it + end}));
    }

  distribute_n_partitions (result, p.n_partitions);

  return result;
}


/* Base implementation to inherit from for all Partitioners.  */
class partitioner_base {
public:
  /* Partitions sym_groups into n_partitions partitions inserted into
     ltrans_partitions.  */
  void
  apply (std::vector<ltrans_partition>& sym_groups, int n_partitions)
  {
    partition_set p (partition_set::metadata (true, true),
		     std::move (sym_groups), n_partitions);
    split (p, 0);
  }

protected:
  partitioner_base (int64_t min_partition_size, int64_t max_partition_size):
    min_partition_size (min_partition_size),
    max_partition_size (max_partition_size)
  {
    gcc_assert (min_partition_size != 0);
    gcc_assert (max_partition_size != 0);
  }
  virtual ~partitioner_base ()
  {}

  /* Joins all symbol groups into one finalized partition.  */
  void
  finalize (partition_set& p)
  {
    ltrans_partition joined = NULL;

    for (ltrans_partition g: p.sym_groups)
      joined = join_partitions (joined, g);

    if (joined)
      ltrans_partitions.safe_push (joined);
  }

  /* Splits all partition_sets.  */
  void
  split_list (std::vector<partition_set>& ps, uintptr_t state)
  {
    for (partition_set& p: ps)
      split (p, state);
  }

  /* Handles common cases:
     too large or small n_partitions, or n_partitions = 1.
     And then calls split_state.  */
  void
  split (partition_set& p, uintptr_t state)
  {
    size_t min_partitions = p.insns / max_partition_size + 1;
    size_t max_partitions = p.insns / min_partition_size;
    if (!p.data.splitable)
      max_partitions = std::min (max_partitions, p.sym_groups.size ());

    p.n_partitions = std::max (p.n_partitions, min_partitions);
    p.n_partitions = std::min (p.n_partitions, max_partitions);

    if (p.n_partitions <= 1)
      return finalize (p);

    split_state (p, state);
  }

  /* State machine for specific partitioner implementation.  */
  virtual void
  split_state (partition_set& p, uintptr_t state) = 0;

  int64_t min_partition_size, max_partition_size;
};


/* Partitioner combining fixed, over_target, and binary partitionings.  */
class partitioner_default: public partitioner_base
{
public:
  partitioner_default (int64_t min_partition_size, int64_t max_partition_size):
    partitioner_base (min_partition_size, max_partition_size)
  {}

private:
  virtual void
  split_state (partition_set& p, uintptr_t state)
  {
    const uintptr_t FIXED = 0;
    const uintptr_t OVER_TARGET = 1;
    const uintptr_t BINARY = 2;

    std::vector<partition_set> ps;

    switch (state)
      {
      case FIXED:
	if (p.n_partitions > 64 && p.sym_groups.size () >= 4)
	  {
	    ps = partition_fixed_split (p, 4);
	    split_list (ps, OVER_TARGET);
	    break;
	  }

	/* FALLTHROUGH */
      case OVER_TARGET:
	ps = partition_over_target_split (p);
	if (!ps.empty ())
	  {
	    split_list (ps, BINARY);
	    break;
	  }

	/* FALLTHROUGH */
      case BINARY:
	ps = partition_binary_split (p);
	if (!ps.empty ())
	  {
	    split_list (ps, OVER_TARGET);
	    break;
	  }

	/* FALLTHROUGH */
      default:
	finalize (p);
      }
  }
};

/* Group cgraph nodes into equally-sized partitions.
   It tries to keep symbols from single source file together to minimize
   propagation of divergence.

   It starts with symbols already grouped by source files.  If reasonably
   possible it only either combines several files into one final partition,
   or, if a file is large, split the file into several final partitions.

   Intermediate representation is partition_set which contains set of
   groups of symbols (each group corresponding to original source file) and
   number of final partitions this partition_set should split into.

   First partition_fixed_split splits partition_set into constant number of
   partition_sets with equal number of symbols groups.  If for example there
   are 39 source files, the resulting partition_sets will contain 10, 10,
   10, and 9 source files.  This splitting intentionally ignores estimated
   instruction counts to minimize propagation of divergence.

   Second partition_over_target_split separates too large files and splits
   them into individual symbols to be combined back into several smaller
   files in next step.

   Third partition_binary_split splits partition_set into two halves until
   it should be split into only one final partition, at which point the
   remaining symbols are joined into one final partition.
*/

void
lto_cache_map (int n_lto_partitions, int max_partition_size)
{
  lto_1_to_1_map ();

  std::vector<ltrans_partition> partitions;
  for (unsigned i = 0; i < ltrans_partitions.length (); ++i)
    {
      ltrans_partition part = ltrans_partitions[i];
      partitions.push_back (part);
    }
  ltrans_partitions.truncate (0);

  partitioner_default partitioner = partitioner_default
    (param_min_partition_size, max_partition_size);

  partitioner.apply (partitions, n_lto_partitions);
}

/* Group cgraph nodes into equally-sized partitions.

   The partitioning algorithm is simple: nodes are taken in predefined order.
   The order corresponds to the order we want functions to have in the final
   output.  In the future this will be given by function reordering pass, but
   at the moment we use the topological order, which is a good approximation.

   The goal is to partition this linear order into intervals (partitions) so
   that all the partitions have approximately the same size and the number of
   callgraph or IPA reference edges crossing boundaries is minimal.

   This is a lot faster (O(n) in size of callgraph) than algorithms doing
   priority-based graph clustering that are generally O(n^2) and, since
   WHOPR is designed to make things go well across partitions, it leads
   to good results.

   We compute the expected size of a partition as:

     max (total_size / lto_partitions, min_partition_size)

   We use dynamic expected size of partition so small programs are partitioned
   into enough partitions to allow use of multiple CPUs, while large programs
   are not partitioned too much.  Creating too many partitions significantly
   increases the streaming overhead.

   In the future, we would like to bound the maximal size of partitions so as
   to prevent the LTRANS stage from consuming too much memory.  At the moment,
   however, the WPA stage is the most memory intensive for large benchmarks,
   since too many types and declarations are read into memory.

   The function implements a simple greedy algorithm.  Nodes are being added
   to the current partition until after 3/4 of the expected partition size is
   reached.  Past this threshold, we keep track of boundary size (number of
   edges going to other partitions) and continue adding functions until after
   the current partition has grown to twice the expected partition size.  Then
   the process is undone to the point where the minimal ratio of boundary size
   and in-partition calls was reached.  */

void
lto_balanced_map (int n_lto_partitions, int max_partition_size)
{
  int n_varpool_nodes = 0, varpool_pos = 0, best_varpool_pos = 0;
  int best_noreorder_pos = 0;
  auto_vec <cgraph_node *> order (symtab->cgraph_count);
  auto_vec<cgraph_node *> noreorder;
  auto_vec<varpool_node *> varpool_order;
  struct cgraph_node *node;
  int64_t original_total_size, total_size = 0;
  int64_t partition_size;
  ltrans_partition partition;
  int last_visited_node = 0;
  varpool_node *vnode;
  int64_t cost = 0, internal = 0;
  unsigned int best_n_nodes = 0, best_i = 0;
  int64_t best_cost = -1, best_internal = 0, best_size = 0;
  int npartitions;
  int current_order = -1;
  int noreorder_pos = 0;

  FOR_EACH_VARIABLE (vnode)
    gcc_assert (!vnode->aux);

  FOR_EACH_DEFINED_FUNCTION (node)
    if (node->get_partitioning_class () == SYMBOL_PARTITION)
      {
	if (node->no_reorder)
	  noreorder.safe_push (node);
	else
	  order.safe_push (node);
	if (!node->alias)
	  total_size += ipa_size_summaries->get (node)->size;
      }

  original_total_size = total_size;

  /* Streaming works best when the source units do not cross partition
     boundaries much.  This is because importing function from a source
     unit tends to import a lot of global trees defined there.  We should
     get better about minimizing the function bounday, but until that
     things works smoother if we order in source order.  */
  order.qsort (tp_first_run_node_cmp);
  noreorder.qsort (node_cmp);

  if (dump_file)
    {
      for (unsigned i = 0; i < order.length (); i++)
	fprintf (dump_file, "Balanced map symbol order:%s:%u\n",
		 order[i]->dump_name (), order[i]->tp_first_run);
      for (unsigned i = 0; i < noreorder.length (); i++)
	fprintf (dump_file, "Balanced map symbol no_reorder:%s:%u\n",
		 noreorder[i]->dump_name (), noreorder[i]->tp_first_run);
    }

  /* Collect all variables that should not be reordered.  */
  FOR_EACH_VARIABLE (vnode)
    if (vnode->get_partitioning_class () == SYMBOL_PARTITION
	&& vnode->no_reorder)
      varpool_order.safe_push (vnode);
  n_varpool_nodes = varpool_order.length ();
  varpool_order.qsort (node_cmp);

  /* Compute partition size and create the first partition.  */
  if (param_min_partition_size > max_partition_size)
    fatal_error (input_location, "min partition size cannot be greater "
		 "than max partition size");

  partition_size = total_size / n_lto_partitions;
  if (partition_size < param_min_partition_size)
    partition_size = param_min_partition_size;
  npartitions = 1;
  partition = new_partition ("");
  if (dump_file)
    fprintf (dump_file, "Total unit size: %" PRId64 ", partition size: %" PRId64 "\n",
	     total_size, partition_size);

  auto_vec<symtab_node *> next_nodes;

  for (unsigned i = 0; i < order.length (); i++)
    {
      if (symbol_partitioned_p (order[i]))
	continue;

      current_order = order[i]->order;

      /* Output noreorder and varpool in program order first.  */
      next_nodes.truncate (0);
      while (varpool_pos < n_varpool_nodes
	     && varpool_order[varpool_pos]->order < current_order)
	next_nodes.safe_push (varpool_order[varpool_pos++]);
      while (noreorder_pos < (int)noreorder.length ()
	     && noreorder[noreorder_pos]->order < current_order)
	next_nodes.safe_push (noreorder[noreorder_pos++]);
      add_sorted_nodes (next_nodes, partition);

      if (!symbol_partitioned_p (order[i]))
        add_symbol_to_partition (partition, order[i]);


      /* Once we added a new node to the partition, we also want to add
         all referenced variables unless they was already added into some
         earlier partition.
	 add_symbol_to_partition adds possibly multiple nodes and
	 variables that are needed to satisfy needs of ORDER[i].
         We remember last visited cgraph and varpool node from last iteration
         of outer loop that allows us to process every new addition.

	 At the same time we compute size of the boundary into COST.  Every
         callgraph or IPA reference edge leaving the partition contributes into
         COST.  Every edge inside partition was earlier computed as one leaving
	 it and thus we need to subtract it from COST.  */
      while (last_visited_node < lto_symtab_encoder_size (partition->encoder))
	{
	  int j;
	  struct ipa_ref *ref = NULL;
	  symtab_node *snode = lto_symtab_encoder_deref (partition->encoder,
							last_visited_node);

	  if (cgraph_node *node = dyn_cast <cgraph_node *> (snode))
	    {
	      struct cgraph_edge *edge;


	      last_visited_node++;

	      gcc_assert (node->definition || node->weakref
			  || node->declare_variant_alt);

	      /* Compute boundary cost of callgraph edges.  */
	      for (edge = node->callees; edge; edge = edge->next_callee)
		/* Inline edges will always end up local.  */
		if (edge->inline_failed
		    && account_reference_p (node, edge->callee))
		  {
		    int edge_cost = edge->frequency ();
		    int index;

		    if (!edge_cost)
		      edge_cost = 1;
		    gcc_assert (edge_cost > 0);
		    index = lto_symtab_encoder_lookup (partition->encoder,
						       edge->callee);
		    if (index != LCC_NOT_FOUND
		        && index < last_visited_node - 1)
		      cost -= edge_cost, internal += edge_cost;
		    else
		      cost += edge_cost;
		  }
	      for (edge = node->callers; edge; edge = edge->next_caller)
		if (edge->inline_failed
		    && account_reference_p (edge->caller, node))
		{
		  int edge_cost = edge->frequency ();
		  int index;

		  gcc_assert (edge->caller->definition);
		  if (!edge_cost)
		    edge_cost = 1;
		  gcc_assert (edge_cost > 0);
		  index = lto_symtab_encoder_lookup (partition->encoder,
						     edge->caller);
		  if (index != LCC_NOT_FOUND
		      && index < last_visited_node - 1)
		    cost -= edge_cost, internal += edge_cost;
		  else
		    cost += edge_cost;
		}
	    }
	  else
	    last_visited_node++;

	  /* Compute boundary cost of IPA REF edges and at the same time look into
	     variables referenced from current partition and try to add them.  */
	  for (j = 0; snode->iterate_reference (j, ref); j++)
	    if (!account_reference_p (snode, ref->referred))
	      ;
	    else if (is_a <varpool_node *> (ref->referred))
	      {
		int index;

		vnode = dyn_cast <varpool_node *> (ref->referred);
		if (!symbol_partitioned_p (vnode)
		    && !vnode->no_reorder
		    && vnode->get_partitioning_class () == SYMBOL_PARTITION)
		  add_symbol_to_partition (partition, vnode);
		index = lto_symtab_encoder_lookup (partition->encoder,
						   vnode);
		if (index != LCC_NOT_FOUND
		    && index < last_visited_node - 1)
		  cost--, internal++;
		else
		  cost++;
	      }
	    else
	      {
		int index;

		node = dyn_cast <cgraph_node *> (ref->referred);
		index = lto_symtab_encoder_lookup (partition->encoder,
						   node);
		if (index != LCC_NOT_FOUND
		    && index < last_visited_node - 1)
		  cost--, internal++;
		else
		  cost++;
	      }
	  for (j = 0; snode->iterate_referring (j, ref); j++)
	    if (!account_reference_p (ref->referring, snode))
	      ;
	    else if (is_a <varpool_node *> (ref->referring))
	      {
		int index;

		vnode = dyn_cast <varpool_node *> (ref->referring);
		gcc_assert (vnode->definition);
		/* It is better to couple variables with their users,
		   because it allows them to be removed.  Coupling
		   with objects they refer to only helps to reduce
		   number of symbols promoted to hidden.  */
		if (!symbol_partitioned_p (vnode)
		    && !vnode->no_reorder
		    && !vnode->can_remove_if_no_refs_p ()
		    && vnode->get_partitioning_class () == SYMBOL_PARTITION)
		  add_symbol_to_partition (partition, vnode);
		index = lto_symtab_encoder_lookup (partition->encoder,
						   vnode);
		if (index != LCC_NOT_FOUND
		    && index < last_visited_node - 1)
		  cost--, internal++;
		else
		  cost++;
	      }
	    else
	      {
		int index;

		node = dyn_cast <cgraph_node *> (ref->referring);
		gcc_assert (node->definition || node->declare_variant_alt);
		index = lto_symtab_encoder_lookup (partition->encoder,
						   node);
		if (index != LCC_NOT_FOUND
		    && index < last_visited_node - 1)
		  cost--, internal++;
		else
		  cost++;
	      }
	}

      gcc_assert (cost >= 0 && internal >= 0);

      /* If the partition is large enough, start looking for smallest boundary cost.
         If partition still seems too small (less than 7/8 of target weight) accept
	 any cost.  If partition has right size, optimize for highest internal/cost.
	 Later we stop building partition if its size is 9/8 of the target wight.  */
      if (partition->insns < partition_size * 7 / 8
	  || best_cost == -1
	  || (!cost
	      || ((sreal)best_internal * (sreal) cost
		  < ((sreal) internal * (sreal)best_cost))))
	{
	  best_cost = cost;
	  best_internal = internal;
	  best_size = partition->insns;
	  best_i = i;
	  best_n_nodes = lto_symtab_encoder_size (partition->encoder);
	  best_varpool_pos = varpool_pos;
	  best_noreorder_pos = noreorder_pos;
	}
      if (dump_file)
	fprintf (dump_file, "Step %i: added %s, size %i, "
		 "cost %" PRId64 "/%" PRId64 " "
		 "best %" PRId64 "/%" PRId64", step %i\n", i,
		 order[i]->dump_name (),
		 partition->insns, cost, internal,
		 best_cost, best_internal, best_i);
      /* Partition is too large, unwind into step when best cost was reached and
	 start new partition.  */
      if (partition->insns > 9 * partition_size / 8
	  || partition->insns > max_partition_size)
	{
	  if (best_i != i)
	    {
	      if (dump_file)
		fprintf (dump_file, "Unwinding %i insertions to step %i\n",
			 i - best_i, best_i);
	      undo_partition (partition, best_n_nodes);
	      varpool_pos = best_varpool_pos;
	      noreorder_pos = best_noreorder_pos;
	    }
	  gcc_assert (best_size == partition->insns);
	  i = best_i;
	  if (dump_file)
	    fprintf (dump_file,
		     "Partition insns: %i (want %" PRId64 ")\n",
		     partition->insns, partition_size);
 	  /* When we are finished, avoid creating empty partition.  */
	  while (i < order.length () - 1 && symbol_partitioned_p (order[i + 1]))
	    i++;
	  if (i == order.length () - 1)
	    break;
	  total_size -= partition->insns;
	  partition = new_partition ("");
	  last_visited_node = 0;
	  cost = 0;

	  if (dump_file)
	    fprintf (dump_file, "New partition\n");
	  best_n_nodes = 0;
	  best_cost = -1;

	  /* Since the size of partitions is just approximate, update the size after
	     we finished current one.  */
	  if (npartitions < n_lto_partitions)
	    partition_size = total_size / (n_lto_partitions - npartitions);
	  else
	    /* Watch for overflow.  */
	    partition_size = INT_MAX / 16;

	  if (dump_file)
	    fprintf (dump_file,
		     "Total size: %" PRId64 " partition_size: %" PRId64 "\n",
		     total_size, partition_size);
	  if (partition_size < param_min_partition_size)
	    partition_size = param_min_partition_size;
	  npartitions ++;
	}
    }

  next_nodes.truncate (0);

  /* Varables that are not reachable from the code go into last partition.  */
  FOR_EACH_VARIABLE (vnode)
    if (vnode->get_partitioning_class () == SYMBOL_PARTITION
	&& !symbol_partitioned_p (vnode))
      next_nodes.safe_push (vnode);

  /* Output remaining ordered symbols.  */
  while (varpool_pos < n_varpool_nodes)
    next_nodes.safe_push (varpool_order[varpool_pos++]);
  while (noreorder_pos < (int)noreorder.length ())
    next_nodes.safe_push (noreorder[noreorder_pos++]);
  /* For one partition the cost of boundary should be 0 unless we added final
     symbols here (these are not accounted) or we have accounting bug.  */
  gcc_assert (next_nodes.length () || npartitions != 1 || !best_cost || best_cost == -1);
  add_sorted_nodes (next_nodes, partition);

  if (dump_file)
    {
      fprintf (dump_file, "\nPartition sizes:\n");
      unsigned partitions = ltrans_partitions.length ();

      for (unsigned i = 0; i < partitions ; i++)
	{
	  ltrans_partition p = ltrans_partitions[i];
	  fprintf (dump_file, "partition %d contains %d (%2.2f%%)"
		   " symbols and %d (%2.2f%%) insns\n", i, p->symbols,
		   100.0 * p->symbols / order.length (), p->insns,
		   100.0 * p->insns / original_total_size);
	}

      fprintf (dump_file, "\n");
    }
}

/* Return true if we must not change the name of the NODE.  The name as
   extracted from the corresponding decl should be passed in NAME.  */

static bool
must_not_rename (symtab_node *node, const char *name)
{
  /* Our renaming machinery do not handle more than one change of assembler name.
     We should not need more than one anyway.  */
  if (node->lto_file_data
      && lto_get_decl_name_mapping (node->lto_file_data, name) != name)
    {
      if (dump_file)
	fprintf (dump_file,
		 "Not privatizing symbol name: %s. It privatized already.\n",
		 name);
      return true;
    }
  /* Avoid mangling of already mangled clones.
     ???  should have a flag whether a symbol has a 'private' name already,
     since we produce some symbols like that i.e. for global constructors
     that are not really clones.
     ???  it is what unique_name means.  We only need to set it when doing
     private symbols.  */
  if (node->unique_name)
    {
      if (dump_file)
	fprintf (dump_file,
		 "Not privatizing symbol name: %s. Has unique name.\n",
		 name);
      return true;
    }
  return false;
}

/* If we are an offload compiler, we may have to rewrite symbols to be
   valid on this target.  Return either PTR or a modified version of it.  */

static const char *
maybe_rewrite_identifier (const char *ptr)
{
#if defined ACCEL_COMPILER && (defined NO_DOT_IN_LABEL || defined NO_DOLLAR_IN_LABEL)
#ifndef NO_DOT_IN_LABEL
  char valid = '.';
  const char reject[] = "$";
#elif !defined NO_DOLLAR_IN_LABEL
  char valid = '$';
  const char reject[] = ".";
#else
  char valid = '_';
  const char reject[] = ".$";
#endif

  char *copy = NULL;
  const char *match = ptr;
  for (;;)
    {
      size_t off = strcspn (match, reject);
      if (match[off] == '\0')
	break;
      if (copy == NULL)
	{
	  copy = xstrdup (ptr);
	  match = copy;
	}
      copy[off] = valid;
    }
  if (copy)
    {
      match = IDENTIFIER_POINTER (get_identifier (copy));
      free (copy);
    }
  return match;
#else
  return ptr;
#endif
}

/* Ensure that the symbol in NODE is valid for the target, and if not,
   rewrite it.  */

static void
validize_symbol_for_target (symtab_node *node)
{
  tree decl = node->decl;
  const char *name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));

  if (must_not_rename (node, name))
    return;

  const char *name2 = maybe_rewrite_identifier (name);
  if (name2 != name)
    {
      symtab->change_decl_assembler_name (decl, get_identifier (name2));
      if (node->lto_file_data)
	lto_record_renamed_decl (node->lto_file_data, name, name2);
    }
}

/* Maps symbol names to unique lto clone counters.  */
static hash_map<const char *, unsigned> *lto_clone_numbers;

/* Helper for privatize_symbol_name.  Mangle NODE symbol name
   represented by DECL.  */

static bool
privatize_symbol_name_1 (symtab_node *node, tree decl)
{
  const char *name0 = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));

  if (must_not_rename (node, name0))
    return false;

  const char *name = maybe_rewrite_identifier (name0);
  unsigned &clone_number = lto_clone_numbers->get_or_insert (name);
  symtab->change_decl_assembler_name (decl,
				      clone_function_name (
					  name, "lto_priv", clone_number));
  clone_number++;

  if (node->lto_file_data)
    lto_record_renamed_decl (node->lto_file_data, name0,
			     IDENTIFIER_POINTER
			     (DECL_ASSEMBLER_NAME (decl)));

  if (dump_file)
    fprintf (dump_file,
	     "Privatizing symbol name: %s -> %s\n",
	     name, IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)));

  return true;
}

/* Mangle NODE symbol name into a local name.
   This is necessary to do
   1) if two or more static vars of same assembler name
      are merged into single ltrans unit.
   2) if previously static var was promoted hidden to avoid possible conflict
      with symbols defined out of the LTO world.  */

static bool
privatize_symbol_name (symtab_node *node)
{
  if (!privatize_symbol_name_1 (node, node->decl))
    return false;

  return true;
}

/* Promote variable VNODE to be static.  */

static void
promote_symbol (symtab_node *node)
{
  /* We already promoted ... */
  if (DECL_VISIBILITY (node->decl) == VISIBILITY_HIDDEN
      && DECL_VISIBILITY_SPECIFIED (node->decl)
      && TREE_PUBLIC (node->decl))
    {
      validize_symbol_for_target (node);
      return;
    }

  gcc_checking_assert (!TREE_PUBLIC (node->decl)
		       && !DECL_EXTERNAL (node->decl));
  /* Be sure that newly public symbol does not conflict with anything already
     defined by the non-LTO part.  */
  privatize_symbol_name (node);
  TREE_PUBLIC (node->decl) = 1;
  /* After privatization the node should not conflict with any other symbol,
     so it is prevailing.  This is important to keep binds_to_current_def_p
     to work across partitions.  */
  node->resolution = LDPR_PREVAILING_DEF_IRONLY;
  node->semantic_interposition = false;
  DECL_VISIBILITY (node->decl) = VISIBILITY_HIDDEN;
  DECL_VISIBILITY_SPECIFIED (node->decl) = true;
  if (dump_file)
    fprintf (dump_file,
	     "Promoting as hidden: %s (%s)\n", node->dump_name (),
	     IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (node->decl)));

  /* Promoting a symbol also promotes all transparent aliases with exception
     of weakref where the visibility flags are always wrong and set to
     !PUBLIC.  */
  ipa_ref *ref;
  for (unsigned i = 0; node->iterate_direct_aliases (i, ref); i++)
    {
      struct symtab_node *alias = ref->referring;
      if (alias->transparent_alias && !alias->weakref)
	{
	  TREE_PUBLIC (alias->decl) = 1;
	  DECL_VISIBILITY (alias->decl) = VISIBILITY_HIDDEN;
	  DECL_VISIBILITY_SPECIFIED (alias->decl) = true;
	  if (dump_file)
	    fprintf (dump_file,
		     "Promoting alias as hidden: %s\n",
		     IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (node->decl)));
	}
      gcc_assert (!alias->weakref || TREE_PUBLIC (alias->decl));
    }
}

/* Return true if NODE needs named section even if it won't land in
   the partition symbol table.

   FIXME: we should really not use named sections for master clones.  */

static bool
may_need_named_section_p (lto_symtab_encoder_t encoder, symtab_node *node)
{
  struct cgraph_node *cnode = dyn_cast <cgraph_node *> (node);
  /* We do not need to handle variables since we never clone them.  */
  if (!cnode)
    return false;
  /* Only master clones will have bodies streamed.  */
  if (cnode->clone_of)
    return false;
  if (node->real_symbol_p ())
    return false;
  return (!encoder
	  || (lto_symtab_encoder_lookup (encoder, node) != LCC_NOT_FOUND
              && lto_symtab_encoder_encode_body_p (encoder,
				                   cnode)));
}

/* If NODE represents a static variable.  See if there are other variables
   of the same name in partition ENCODER (or in whole compilation unit if
   ENCODER is NULL) and if so, mangle the statics.  Always mangle all
   conflicting statics, so we reduce changes of silently miscompiling
   asm statements referring to them by symbol name.  */

static void
rename_statics (lto_symtab_encoder_t encoder, symtab_node *node)
{
  tree decl = node->decl;
  symtab_node *s;
  tree name = DECL_ASSEMBLER_NAME (decl);

  /* See if this is static symbol. */
  if (((node->externally_visible && !node->weakref)
      /* FIXME: externally_visible is somewhat illogically not set for
	 external symbols (i.e. those not defined).  Remove this test
	 once this is fixed.  */
        || DECL_EXTERNAL (node->decl)
	|| !node->real_symbol_p ())
       && !may_need_named_section_p (encoder, node))
    return;

  /* Now walk symbols sharing the same name and see if there are any conflicts.
     (all types of symbols counts here, since we cannot have static of the
     same name as external or public symbol.)  */
  for (s = symtab_node::get_for_asmname (name);
       s; s = s->next_sharing_asm_name)
    if ((s->real_symbol_p () || may_need_named_section_p (encoder, s))
	&& s->decl != node->decl
	&& (!encoder
	    || lto_symtab_encoder_lookup (encoder, s) != LCC_NOT_FOUND))
       break;

  /* OK, no confict, so we have nothing to do.  */
  if (!s)
    return;

  if (dump_file)
    fprintf (dump_file,
	    "Renaming statics with asm name: %s\n", node->dump_name ());

  /* Assign every symbol in the set that shares the same ASM name an unique
     mangled name.  */
  for (s = symtab_node::get_for_asmname (name); s;)
    if ((!s->externally_visible || s->weakref)
	/* Transparent aliases having same name as target are renamed at a
	   time their target gets new name.  Transparent aliases that use
	   separate assembler name require the name to be unique.  */
	&& (!s->transparent_alias || !s->definition || s->weakref
	    || !symbol_table::assembler_names_equal_p
		 (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (s->decl)),
		  IDENTIFIER_POINTER
		    (DECL_ASSEMBLER_NAME (s->get_alias_target()->decl))))
	&& ((s->real_symbol_p ()
             && !DECL_EXTERNAL (s->decl)
	     && !TREE_PUBLIC (s->decl))
 	    || may_need_named_section_p (encoder, s))
	&& (!encoder
	    || lto_symtab_encoder_lookup (encoder, s) != LCC_NOT_FOUND))
      {
        if (privatize_symbol_name (s))
	  /* Re-start from beginning since we do not know how many
	     symbols changed a name.  */
	  s = symtab_node::get_for_asmname (name);
        else s = s->next_sharing_asm_name;
      }
    else s = s->next_sharing_asm_name;
}

/* Find out all static decls that need to be promoted to global because
   of cross file sharing.  This function must be run in the WPA mode after
   all inlinees are added.  */

void
lto_promote_cross_file_statics (void)
{
  unsigned i, n_sets;

  gcc_assert (flag_wpa);

  lto_stream_offload_p = false;
  select_what_to_stream ();

  /* First compute boundaries.  */
  n_sets = ltrans_partitions.length ();
  for (i = 0; i < n_sets; i++)
    {
      ltrans_partition part
	= ltrans_partitions[i];
      part->encoder = compute_ltrans_boundary (part->encoder);
    }

  lto_clone_numbers = new hash_map<const char *, unsigned>;

  /* Look at boundaries and promote symbols as needed.  */
  for (i = 0; i < n_sets; i++)
    {
      lto_symtab_encoder_iterator lsei;
      lto_symtab_encoder_t encoder = ltrans_partitions[i]->encoder;

      for (lsei = lsei_start (encoder); !lsei_end_p (lsei);
	   lsei_next (&lsei))
        {
          symtab_node *node = lsei_node (lsei);

	  /* If symbol is static, rename it if its assembler name
	     clashes with anything else in this unit.  */
	  rename_statics (encoder, node);

	  /* No need to promote if symbol already is externally visible ... */
	  if (node->externally_visible
 	      /* ... or if it is part of current partition ... */
	      || lto_symtab_encoder_in_partition_p (encoder, node)
	      /* ... or if we do not partition it. This mean that it will
		 appear in every partition referencing it.  */
	      || node->get_partitioning_class () != SYMBOL_PARTITION)
	    {
	      validize_symbol_for_target (node);
	      continue;
	    }

          promote_symbol (node);
        }
    }
  delete lto_clone_numbers;
}

/* Rename statics in the whole unit in the case that
   we do -flto-partition=none.  */

void
lto_promote_statics_nonwpa (void)
{
  symtab_node *node;

  lto_clone_numbers = new hash_map<const char *, unsigned>;
  FOR_EACH_SYMBOL (node)
    {
      rename_statics (NULL, node);
      validize_symbol_for_target (node);
    }
  delete lto_clone_numbers;
}
