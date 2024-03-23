/* Predictive commoning.
   Copyright (C) 2005-2024 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* This file implements the predictive commoning optimization.  Predictive
   commoning can be viewed as CSE around a loop, and with some improvements,
   as generalized strength reduction-- i.e., reusing values computed in
   earlier iterations of a loop in the later ones.  So far, the pass only
   handles the most useful case, that is, reusing values of memory references.
   If you think this is all just a special case of PRE, you are sort of right;
   however, concentrating on loops is simpler, and makes it possible to
   incorporate data dependence analysis to detect the opportunities, perform
   loop unrolling to avoid copies together with renaming immediately,
   and if needed, we could also take register pressure into account.

   Let us demonstrate what is done on an example:

   for (i = 0; i < 100; i++)
     {
       a[i+2] = a[i] + a[i+1];
       b[10] = b[10] + i;
       c[i] = c[99 - i];
       d[i] = d[i + 1];
     }

   1) We find data references in the loop, and split them to mutually
      independent groups (i.e., we find components of a data dependence
      graph).  We ignore read-read dependences whose distance is not constant.
      (TODO -- we could also ignore antidependences).  In this example, we
      find the following groups:

      a[i]{read}, a[i+1]{read}, a[i+2]{write}
      b[10]{read}, b[10]{write}
      c[99 - i]{read}, c[i]{write}
      d[i + 1]{read}, d[i]{write}

   2) Inside each of the group, we verify several conditions:
      a) all the references must differ in indices only, and the indices
	 must all have the same step
      b) the references must dominate loop latch (and thus, they must be
	 ordered by dominance relation).
      c) the distance of the indices must be a small multiple of the step
      We are then able to compute the difference of the references (# of
      iterations before they point to the same place as the first of them).
      Also, in case there are writes in the loop, we split the groups into
      chains whose head is the write whose values are used by the reads in
      the same chain.  The chains are then processed independently,
      making the further transformations simpler.  Also, the shorter chains
      need the same number of registers, but may require lower unrolling
      factor in order to get rid of the copies on the loop latch.

      In our example, we get the following chains (the chain for c is invalid).

      a[i]{read,+0}, a[i+1]{read,-1}, a[i+2]{write,-2}
      b[10]{read,+0}, b[10]{write,+0}
      d[i + 1]{read,+0}, d[i]{write,+1}

   3) For each read, we determine the read or write whose value it reuses,
      together with the distance of this reuse.  I.e. we take the last
      reference before it with distance 0, or the last of the references
      with the smallest positive distance to the read.  Then, we remove
      the references that are not used in any of these chains, discard the
      empty groups, and propagate all the links so that they point to the
      single root reference of the chain (adjusting their distance
      appropriately).  Some extra care needs to be taken for references with
      step 0.  In our example (the numbers indicate the distance of the
      reuse),

      a[i] --> (*) 2, a[i+1] --> (*) 1, a[i+2] (*)
      b[10] --> (*) 1, b[10] (*)

   4) The chains are combined together if possible.  If the corresponding
      elements of two chains are always combined together with the same
      operator, we remember just the result of this combination, instead
      of remembering the values separately.  We may need to perform
      reassociation to enable combining, for example

      e[i] + f[i+1] + e[i+1] + f[i]

      can be reassociated as

      (e[i] + f[i]) + (e[i+1] + f[i+1])

      and we can combine the chains for e and f into one chain.

   5) For each root reference (end of the chain) R, let N be maximum distance
      of a reference reusing its value.  Variables R0 up to RN are created,
      together with phi nodes that transfer values from R1 .. RN to
      R0 .. R(N-1).
      Initial values are loaded to R0..R(N-1) (in case not all references
      must necessarily be accessed and they may trap, we may fail here;
      TODO sometimes, the loads could be guarded by a check for the number
      of iterations).  Values loaded/stored in roots are also copied to
      RN.  Other reads are replaced with the appropriate variable Ri.
      Everything is put to SSA form.

      As a small improvement, if R0 is dead after the root (i.e., all uses of
      the value with the maximum distance dominate the root), we can avoid
      creating RN and use R0 instead of it.

      In our example, we get (only the parts concerning a and b are shown):
      for (i = 0; i < 100; i++)
	{
	  f = phi (a[0], s);
	  s = phi (a[1], f);
	  x = phi (b[10], x);

	  f = f + s;
	  a[i+2] = f;
	  x = x + i;
	  b[10] = x;
	}

   6) Factor F for unrolling is determined as the smallest common multiple of
      (N + 1) for each root reference (N for references for that we avoided
      creating RN).  If F and the loop is small enough, loop is unrolled F
      times.  The stores to RN (R0) in the copies of the loop body are
      periodically replaced with R0, R1, ... (R1, R2, ...), so that they can
      be coalesced and the copies can be eliminated.

      TODO -- copy propagation and other optimizations may change the live
      ranges of the temporary registers and prevent them from being coalesced;
      this may increase the register pressure.

      In our case, F = 2 and the (main loop of the) result is

      for (i = 0; i < ...; i += 2)
        {
          f = phi (a[0], f);
          s = phi (a[1], s);
          x = phi (b[10], x);

          f = f + s;
          a[i+2] = f;
          x = x + i;
          b[10] = x;

          s = s + f;
          a[i+3] = s;
          x = x + i;
          b[10] = x;
       }

   Apart from predictive commoning on Load-Load and Store-Load chains, we
   also support Store-Store chains -- stores killed by other store can be
   eliminated.  Given below example:

     for (i = 0; i < n; i++)
       {
	 a[i] = 1;
	 a[i+2] = 2;
       }

   It can be replaced with:

     t0 = a[0];
     t1 = a[1];
     for (i = 0; i < n; i++)
       {
	 a[i] = 1;
	 t2 = 2;
	 t0 = t1;
	 t1 = t2;
       }
     a[n] = t0;
     a[n+1] = t1;

   If the loop runs more than 1 iterations, it can be further simplified into:

     for (i = 0; i < n; i++)
       {
	 a[i] = 1;
       }
     a[n] = 2;
     a[n+1] = 2;

   The interesting part is this can be viewed either as general store motion
   or general dead store elimination in either intra/inter-iterations way.

   With trivial effort, we also support load inside Store-Store chains if the
   load is dominated by a store statement in the same iteration of loop.  You
   can see this as a restricted Store-Mixed-Load-Store chain.

   TODO: For now, we don't support store-store chains in multi-exit loops.  We
   force to not unroll in case of store-store chain even if other chains might
   ask for unroll.

   Predictive commoning can be generalized for arbitrary computations (not
   just memory loads), and also nontrivial transfer functions (e.g., replacing
   i * i with ii_last + 2 * i + 1), to generalize strength reduction.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "predict.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "alias.h"
#include "fold-const.h"
#include "cfgloop.h"
#include "tree-eh.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "tree-ssa-loop-ivopts.h"
#include "tree-ssa-loop-manip.h"
#include "tree-ssa-loop-niter.h"
#include "tree-ssa-loop.h"
#include "tree-into-ssa.h"
#include "tree-dfa.h"
#include "tree-ssa.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "tree-affine.h"
#include "builtins.h"
#include "opts.h"

/* The maximum number of iterations between the considered memory
   references.  */

#define MAX_DISTANCE (target_avail_regs < 16 ? 4 : 8)

/* Data references (or phi nodes that carry data reference values across
   loop iterations).  */

typedef class dref_d
{
public:
  /* The reference itself.  */
  struct data_reference *ref;

  /* The statement in that the reference appears.  */
  gimple *stmt;

  /* In case that STMT is a phi node, this field is set to the SSA name
     defined by it in replace_phis_by_defined_names (in order to avoid
     pointing to phi node that got reallocated in the meantime).  */
  tree name_defined_by_phi;

  /* Distance of the reference from the root of the chain (in number of
     iterations of the loop).  */
  unsigned distance;

  /* Number of iterations offset from the first reference in the component.  */
  widest_int offset;

  /* Number of the reference in a component, in dominance ordering.  */
  unsigned pos;

  /* True if the memory reference is always accessed when the loop is
     entered.  */
  unsigned always_accessed : 1;
} *dref;


/* Type of the chain of the references.  */

enum chain_type
{
  /* The addresses of the references in the chain are constant.  */
  CT_INVARIANT,

  /* There are only loads in the chain.  */
  CT_LOAD,

  /* Root of the chain is store, the rest are loads.  */
  CT_STORE_LOAD,

  /* There are only stores in the chain.  */
  CT_STORE_STORE,

  /* A combination of two chains.  */
  CT_COMBINATION
};

/* Chains of data references.  */

typedef struct chain
{
  chain (chain_type t) : type (t), op (ERROR_MARK), rslt_type (NULL_TREE),
    ch1 (NULL), ch2 (NULL), length (0), init_seq (NULL), fini_seq (NULL),
    has_max_use_after (false), all_always_accessed (false), combined (false),
    inv_store_elimination (false) {}

  /* Type of the chain.  */
  enum chain_type type;

  /* For combination chains, the operator and the two chains that are
     combined, and the type of the result.  */
  enum tree_code op;
  tree rslt_type;
  struct chain *ch1, *ch2;

  /* The references in the chain.  */
  auto_vec<dref> refs;

  /* The maximum distance of the reference in the chain from the root.  */
  unsigned length;

  /* The variables used to copy the value throughout iterations.  */
  auto_vec<tree> vars;

  /* Initializers for the variables.  */
  auto_vec<tree> inits;

  /* Finalizers for the eliminated stores.  */
  auto_vec<tree> finis;

  /* gimple stmts intializing the initial variables of the chain.  */
  gimple_seq init_seq;

  /* gimple stmts finalizing the eliminated stores of the chain.  */
  gimple_seq fini_seq;

  /* True if there is a use of a variable with the maximal distance
     that comes after the root in the loop.  */
  unsigned has_max_use_after : 1;

  /* True if all the memory references in the chain are always accessed.  */
  unsigned all_always_accessed : 1;

  /* True if this chain was combined together with some other chain.  */
  unsigned combined : 1;

  /* True if this is store elimination chain and eliminated stores store
     loop invariant value into memory.  */
  unsigned inv_store_elimination : 1;
} *chain_p;


/* Describes the knowledge about the step of the memory references in
   the component.  */

enum ref_step_type
{
  /* The step is zero.  */
  RS_INVARIANT,

  /* The step is nonzero.  */
  RS_NONZERO,

  /* The step may or may not be nonzero.  */
  RS_ANY
};

/* Components of the data dependence graph.  */

struct component
{
  component (bool es) : comp_step (RS_ANY), eliminate_store_p (es),
    next (NULL) {}

  /* The references in the component.  */
  auto_vec<dref> refs;

  /* What we know about the step of the references in the component.  */
  enum ref_step_type comp_step;

  /* True if all references in component are stores and we try to do
     intra/inter loop iteration dead store elimination.  */
  bool eliminate_store_p;

  /* Next component in the list.  */
  struct component *next;
};

/* A class to encapsulate the global states used for predictive
   commoning work on top of one given LOOP.  */

class pcom_worker
{
public:
  pcom_worker (loop_p l) : m_loop (l), m_cache (NULL) {}

  ~pcom_worker ()
  {
    free_data_refs (m_datarefs);
    free_dependence_relations (m_dependences);
    free_affine_expand_cache (&m_cache);
    release_chains ();
  }

  pcom_worker (const pcom_worker &) = delete;
  pcom_worker &operator= (const pcom_worker &) = delete;

  /* Performs predictive commoning.  */
  unsigned tree_predictive_commoning_loop (bool allow_unroll_p);

  /* Perform the predictive commoning optimization for chains, make this
     public for being called in callback execute_pred_commoning_cbck.  */
  void execute_pred_commoning (bitmap tmp_vars);

private:
  /* The pointer to the given loop.  */
  loop_p m_loop;

  /* All data references.  */
  auto_vec<data_reference_p, 10> m_datarefs;

  /* All data dependences.  */
  auto_vec<ddr_p, 10> m_dependences;

  /* All chains.  */
  auto_vec<chain_p> m_chains;

  /* Bitmap of ssa names defined by looparound phi nodes covered by chains.  */
  auto_bitmap m_looparound_phis;

  typedef hash_map<tree, name_expansion *> tree_expand_map_t;
  /* Cache used by tree_to_aff_combination_expand.  */
  tree_expand_map_t *m_cache;

  /* Splits dependence graph to components.  */
  struct component *split_data_refs_to_components ();

  /* Check the conditions on references inside each of components COMPS,
     and remove the unsuitable components from the list.  */
  struct component *filter_suitable_components (struct component *comps);

  /* Find roots of the values and determine distances in components COMPS,
     and separates the references to chains.  */
  void determine_roots (struct component *comps);

  /* Prepare initializers for chains, and free chains that cannot
     be used because the initializers might trap.  */
  void prepare_initializers ();

  /* Generates finalizer memory reference for chains.  Returns true if
     finalizer code generation for chains breaks loop closed ssa form.  */
  bool prepare_finalizers ();

  /* Try to combine the chains.  */
  void try_combine_chains ();

  /* Frees CHAINS.  */
  void release_chains ();

  /* Frees a chain CHAIN.  */
  void release_chain (chain_p chain);

  /* Prepare initializers for CHAIN.  Returns false if this is impossible
     because one of these initializers may trap, true otherwise.  */
  bool prepare_initializers_chain (chain_p chain);

  /* Generates finalizer memory references for CHAIN.  Returns true
     if finalizer code for CHAIN can be generated, otherwise false.  */
  bool prepare_finalizers_chain (chain_p chain);

  /* Stores DR_OFFSET (DR) + DR_INIT (DR) to OFFSET.  */
  void aff_combination_dr_offset (struct data_reference *dr, aff_tree *offset);

  /* Determines number of iterations of the innermost enclosing loop before
     B refers to exactly the same location as A and stores it to OFF.  */
  bool determine_offset (struct data_reference *a, struct data_reference *b,
			 poly_widest_int *off);

  /* Returns true if the component COMP satisfies the conditions
     described in 2) at the beginning of this file.  */
  bool suitable_component_p (struct component *comp);

  /* Returns true if REF is a valid initializer for ROOT with given
     DISTANCE (in iterations of the innermost enclosing loop).  */
  bool valid_initializer_p (struct data_reference *ref, unsigned distance,
			    struct data_reference *root);

  /* Finds looparound phi node of loop that copies the value of REF.  */
  gphi *find_looparound_phi (dref ref, dref root);

  /* Find roots of the values and determine distances in the component
     COMP.  The references are redistributed into chains.  */
  void determine_roots_comp (struct component *comp);

  /* For references in CHAIN that are copied around the loop, add the
     results of such copies to the chain.  */
  void add_looparound_copies (chain_p chain);

  /* Returns the single statement in that NAME is used, excepting
     the looparound phi nodes contained in one of the chains.  */
  gimple *single_nonlooparound_use (tree name);

  /* Remove statement STMT, as well as the chain of assignments in that
     it is used.  */
  void remove_stmt (gimple *stmt);

  /* Perform the predictive commoning optimization for a chain CHAIN.  */
  void execute_pred_commoning_chain (chain_p chain, bitmap tmp_vars);

  /* Returns the modify statement that uses NAME.  */
  gimple *find_use_stmt (tree *name);

  /* If the operation used in STMT is associative and commutative, go
     through the tree of the same operations and returns its root.  */
  gimple *find_associative_operation_root (gimple *stmt, unsigned *distance);

  /* Returns the common statement in that NAME1 and NAME2 have a use.  */
  gimple *find_common_use_stmt (tree *name1, tree *name2);

  /* Checks whether R1 and R2 are combined together using CODE, with the
     result in RSLT_TYPE, in order R1 CODE R2 if SWAP is false and in order
     R2 CODE R1 if it is true.  */
  bool combinable_refs_p (dref r1, dref r2, enum tree_code *code, bool *swap,
			  tree *rslt_type);

  /* Reassociates the expression in that NAME1 and NAME2 are used so that
     they are combined in a single statement, and returns this statement.  */
  gimple *reassociate_to_the_same_stmt (tree name1, tree name2);

  /* Returns the statement that combines references R1 and R2.  */
  gimple *stmt_combining_refs (dref r1, dref r2);

  /* Tries to combine chains CH1 and CH2 together.  */
  chain_p combine_chains (chain_p ch1, chain_p ch2);
};

/* Dumps data reference REF to FILE.  */

extern void dump_dref (FILE *, dref);
void
dump_dref (FILE *file, dref ref)
{
  if (ref->ref)
    {
      fprintf (file, "    ");
      print_generic_expr (file, DR_REF (ref->ref), TDF_SLIM);
      fprintf (file, " (id %u%s)\n", ref->pos,
	       DR_IS_READ (ref->ref) ? "" : ", write");

      fprintf (file, "      offset ");
      print_decs (ref->offset, file);
      fprintf (file, "\n");

      fprintf (file, "      distance %u\n", ref->distance);
    }
  else
    {
      if (gimple_code (ref->stmt) == GIMPLE_PHI)
	fprintf (file, "    looparound ref\n");
      else
	fprintf (file, "    combination ref\n");
      fprintf (file, "      in statement ");
      print_gimple_stmt (file, ref->stmt, 0, TDF_SLIM);
      fprintf (file, "\n");
      fprintf (file, "      distance %u\n", ref->distance);
    }

}

/* Dumps CHAIN to FILE.  */

extern void dump_chain (FILE *, chain_p);
void
dump_chain (FILE *file, chain_p chain)
{
  dref a;
  const char *chain_type;
  unsigned i;
  tree var;

  switch (chain->type)
    {
    case CT_INVARIANT:
      chain_type = "Load motion";
      break;

    case CT_LOAD:
      chain_type = "Loads-only";
      break;

    case CT_STORE_LOAD:
      chain_type = "Store-loads";
      break;

    case CT_STORE_STORE:
      chain_type = "Store-stores";
      break;

    case CT_COMBINATION:
      chain_type = "Combination";
      break;

    default:
      gcc_unreachable ();
    }

  fprintf (file, "%s chain %p%s\n", chain_type, (void *) chain,
	   chain->combined ? " (combined)" : "");
  if (chain->type != CT_INVARIANT)
    fprintf (file, "  max distance %u%s\n", chain->length,
	     chain->has_max_use_after ? "" : ", may reuse first");

  if (chain->type == CT_COMBINATION)
    {
      fprintf (file, "  equal to %p %s %p in type ",
	       (void *) chain->ch1, op_symbol_code (chain->op),
	       (void *) chain->ch2);
      print_generic_expr (file, chain->rslt_type, TDF_SLIM);
      fprintf (file, "\n");
    }

  if (chain->vars.exists ())
    {
      fprintf (file, "  vars");
      FOR_EACH_VEC_ELT (chain->vars, i, var)
	{
	  fprintf (file, " ");
	  print_generic_expr (file, var, TDF_SLIM);
	}
      fprintf (file, "\n");
    }

  if (chain->inits.exists ())
    {
      fprintf (file, "  inits");
      FOR_EACH_VEC_ELT (chain->inits, i, var)
	{
	  fprintf (file, " ");
	  print_generic_expr (file, var, TDF_SLIM);
	}
      fprintf (file, "\n");
    }

  fprintf (file, "  references:\n");
  FOR_EACH_VEC_ELT (chain->refs, i, a)
    dump_dref (file, a);

  fprintf (file, "\n");
}

/* Dumps CHAINS to FILE.  */

void
dump_chains (FILE *file, const vec<chain_p> &chains)
{
  chain_p chain;
  unsigned i;

  FOR_EACH_VEC_ELT (chains, i, chain)
    dump_chain (file, chain);
}

/* Dumps COMP to FILE.  */

extern void dump_component (FILE *, struct component *);
void
dump_component (FILE *file, struct component *comp)
{
  dref a;
  unsigned i;

  fprintf (file, "Component%s:\n",
	   comp->comp_step == RS_INVARIANT ? " (invariant)" : "");
  FOR_EACH_VEC_ELT (comp->refs, i, a)
    dump_dref (file, a);
  fprintf (file, "\n");
}

/* Dumps COMPS to FILE.  */

extern void dump_components (FILE *, struct component *);
void
dump_components (FILE *file, struct component *comps)
{
  struct component *comp;

  for (comp = comps; comp; comp = comp->next)
    dump_component (file, comp);
}

/* Frees a chain CHAIN.  */

void
pcom_worker::release_chain (chain_p chain)
{
  dref ref;
  unsigned i;

  if (chain == NULL)
    return;

  FOR_EACH_VEC_ELT (chain->refs, i, ref)
    free (ref);

  if (chain->init_seq)
    gimple_seq_discard (chain->init_seq);

  if (chain->fini_seq)
    gimple_seq_discard (chain->fini_seq);

  delete chain;
}

/* Frees CHAINS.  */

void
pcom_worker::release_chains ()
{
  unsigned i;
  chain_p chain;

  FOR_EACH_VEC_ELT (m_chains, i, chain)
    release_chain (chain);
}

/* Frees list of components COMPS.  */

static void
release_components (struct component *comps)
{
  struct component *act, *next;

  for (act = comps; act; act = next)
    {
      next = act->next;
      delete act;
    }
}

/* Finds a root of tree given by FATHERS containing A, and performs path
   shortening.  */

static unsigned
component_of (vec<unsigned> &fathers, unsigned a)
{
  unsigned root, n;

  for (root = a; root != fathers[root]; root = fathers[root])
    continue;

  for (; a != root; a = n)
    {
      n = fathers[a];
      fathers[a] = root;
    }

  return root;
}

/* Join operation for DFU.  FATHERS gives the tree, SIZES are sizes of the
   components, A and B are components to merge.  */

static void
merge_comps (vec<unsigned> &fathers, vec<unsigned> &sizes,
	     unsigned a, unsigned b)
{
  unsigned ca = component_of (fathers, a);
  unsigned cb = component_of (fathers, b);

  if (ca == cb)
    return;

  if (sizes[ca] < sizes[cb])
    {
      sizes[cb] += sizes[ca];
      fathers[ca] = cb;
    }
  else
    {
      sizes[ca] += sizes[cb];
      fathers[cb] = ca;
    }
}

/* Returns true if A is a reference that is suitable for predictive commoning
   in the innermost loop that contains it.  REF_STEP is set according to the
   step of the reference A.  */

static bool
suitable_reference_p (struct data_reference *a, enum ref_step_type *ref_step)
{
  tree ref = DR_REF (a), step = DR_STEP (a);

  if (!step
      || TREE_THIS_VOLATILE (ref)
      || !is_gimple_reg_type (TREE_TYPE (ref))
      || tree_could_throw_p (ref))
    return false;

  if (integer_zerop (step))
    *ref_step = RS_INVARIANT;
  else if (integer_nonzerop (step))
    *ref_step = RS_NONZERO;
  else
    *ref_step = RS_ANY;

  return true;
}

/* Stores DR_OFFSET (DR) + DR_INIT (DR) to OFFSET.  */

void
pcom_worker::aff_combination_dr_offset (struct data_reference *dr,
					aff_tree *offset)
{
  tree type = TREE_TYPE (DR_OFFSET (dr));
  aff_tree delta;

  tree_to_aff_combination_expand (DR_OFFSET (dr), type, offset, &m_cache);
  aff_combination_const (&delta, type, wi::to_poly_widest (DR_INIT (dr)));
  aff_combination_add (offset, &delta);
}

/* Determines number of iterations of the innermost enclosing loop before B
   refers to exactly the same location as A and stores it to OFF.  If A and
   B do not have the same step, they never meet, or anything else fails,
   returns false, otherwise returns true.  Both A and B are assumed to
   satisfy suitable_reference_p.  */

bool
pcom_worker::determine_offset (struct data_reference *a,
			       struct data_reference *b, poly_widest_int *off)
{
  aff_tree diff, baseb, step;
  tree typea, typeb;

  /* Check that both the references access the location in the same type.  */
  typea = TREE_TYPE (DR_REF (a));
  typeb = TREE_TYPE (DR_REF (b));
  if (!useless_type_conversion_p (typeb, typea))
    return false;

  /* Check whether the base address and the step of both references is the
     same.  */
  if (!operand_equal_p (DR_STEP (a), DR_STEP (b), 0)
      || !operand_equal_p (DR_BASE_ADDRESS (a), DR_BASE_ADDRESS (b), 0))
    return false;

  if (integer_zerop (DR_STEP (a)))
    {
      /* If the references have loop invariant address, check that they access
	 exactly the same location.  */
      *off = 0;
      return (operand_equal_p (DR_OFFSET (a), DR_OFFSET (b), 0)
	      && operand_equal_p (DR_INIT (a), DR_INIT (b), 0));
    }

  /* Compare the offsets of the addresses, and check whether the difference
     is a multiple of step.  */
  aff_combination_dr_offset (a, &diff);
  aff_combination_dr_offset (b, &baseb);
  aff_combination_scale (&baseb, -1);
  aff_combination_add (&diff, &baseb);

  tree_to_aff_combination_expand (DR_STEP (a), TREE_TYPE (DR_STEP (a)),
				  &step, &m_cache);
  return aff_combination_constant_multiple_p (&diff, &step, off);
}

/* Returns the last basic block in LOOP for that we are sure that
   it is executed whenever the loop is entered.  */

static basic_block
last_always_executed_block (class loop *loop)
{
  unsigned i;
  auto_vec<edge> exits = get_loop_exit_edges (loop);
  edge ex;
  basic_block last = loop->latch;

  FOR_EACH_VEC_ELT (exits, i, ex)
    last = nearest_common_dominator (CDI_DOMINATORS, last, ex->src);

  return last;
}

/* Splits dependence graph on DATAREFS described by DEPENDENCES to
   components.  */

struct component *
pcom_worker::split_data_refs_to_components ()
{
  unsigned i, n = m_datarefs.length ();
  unsigned ca, ia, ib, bad;
  struct data_reference *dr, *dra, *drb;
  struct data_dependence_relation *ddr;
  struct component *comp_list = NULL, *comp;
  dref dataref;
  /* Don't do store elimination if loop has multiple exit edges.  */
  bool eliminate_store_p = single_exit (m_loop) != NULL;
  basic_block last_always_executed = last_always_executed_block (m_loop);
  auto_bitmap no_store_store_comps;
  auto_vec<unsigned> comp_father (n + 1);
  auto_vec<unsigned> comp_size (n + 1);
  comp_father.quick_grow (n + 1);
  comp_size.quick_grow (n + 1);

  FOR_EACH_VEC_ELT (m_datarefs, i, dr)
    {
      if (!DR_REF (dr))
	  /* A fake reference for call or asm_expr that may clobber memory;
	     just fail.  */
	  return NULL;
      /* predcom pass isn't prepared to handle calls with data references.  */
      if (is_gimple_call (DR_STMT (dr)))
	return NULL;
      dr->aux = (void *) (size_t) i;
      comp_father[i] = i;
      comp_size[i] = 1;
    }

  /* A component reserved for the "bad" data references.  */
  comp_father[n] = n;
  comp_size[n] = 1;

  FOR_EACH_VEC_ELT (m_datarefs, i, dr)
    {
      enum ref_step_type dummy;

      if (!suitable_reference_p (dr, &dummy))
	{
	  ia = (unsigned) (size_t) dr->aux;
	  merge_comps (comp_father, comp_size, n, ia);
	}
    }

  FOR_EACH_VEC_ELT (m_dependences, i, ddr)
    {
      poly_widest_int dummy_off;

      if (DDR_ARE_DEPENDENT (ddr) == chrec_known)
	continue;

      dra = DDR_A (ddr);
      drb = DDR_B (ddr);

      /* Don't do store elimination if there is any unknown dependence for
	 any store data reference.  */
      if ((DR_IS_WRITE (dra) || DR_IS_WRITE (drb))
	  && (DDR_ARE_DEPENDENT (ddr) == chrec_dont_know
	      || DDR_NUM_DIST_VECTS (ddr) == 0))
	eliminate_store_p = false;

      ia = component_of (comp_father, (unsigned) (size_t) dra->aux);
      ib = component_of (comp_father, (unsigned) (size_t) drb->aux);
      if (ia == ib)
	continue;

      bad = component_of (comp_father, n);

      /* If both A and B are reads, we may ignore unsuitable dependences.  */
      if (DR_IS_READ (dra) && DR_IS_READ (drb))
	{
	  if (ia == bad || ib == bad
	      || !determine_offset (dra, drb, &dummy_off))
	    continue;
	}
      /* If A is read and B write or vice versa and there is unsuitable
	 dependence, instead of merging both components into a component
	 that will certainly not pass suitable_component_p, just put the
	 read into bad component, perhaps at least the write together with
	 all the other data refs in it's component will be optimizable.  */
      else if (DR_IS_READ (dra) && ib != bad)
	{
	  if (ia == bad)
	    {
	      bitmap_set_bit (no_store_store_comps, ib);
	      continue;
	    }
	  else if (!determine_offset (dra, drb, &dummy_off))
	    {
	      bitmap_set_bit (no_store_store_comps, ib);
	      merge_comps (comp_father, comp_size, bad, ia);
	      continue;
	    }
	}
      else if (DR_IS_READ (drb) && ia != bad)
	{
	  if (ib == bad)
	    {
	      bitmap_set_bit (no_store_store_comps, ia);
	      continue;
	    }
	  else if (!determine_offset (dra, drb, &dummy_off))
	    {
	      bitmap_set_bit (no_store_store_comps, ia);
	      merge_comps (comp_father, comp_size, bad, ib);
	      continue;
	    }
	}
      else if (DR_IS_WRITE (dra) && DR_IS_WRITE (drb)
	       && ia != bad && ib != bad
	       && !determine_offset (dra, drb, &dummy_off))
	{
	  merge_comps (comp_father, comp_size, bad, ia);
	  merge_comps (comp_father, comp_size, bad, ib);
	  continue;
	}

      merge_comps (comp_father, comp_size, ia, ib);
    }

  if (eliminate_store_p)
    {
      tree niters = number_of_latch_executions (m_loop);

      /* Don't do store elimination if niters info is unknown because stores
	 in the last iteration can't be eliminated and we need to recover it
	 after loop.  */
      eliminate_store_p = (niters != NULL_TREE && niters != chrec_dont_know);
    }

  auto_vec<struct component *> comps;
  comps.safe_grow_cleared (n, true);
  bad = component_of (comp_father, n);
  FOR_EACH_VEC_ELT (m_datarefs, i, dr)
    {
      ia = (unsigned) (size_t) dr->aux;
      ca = component_of (comp_father, ia);
      if (ca == bad)
	continue;

      comp = comps[ca];
      if (!comp)
	{
	  comp = new component (eliminate_store_p);
	  comp->refs.reserve_exact (comp_size[ca]);
	  comps[ca] = comp;
	}

      dataref = XCNEW (class dref_d);
      dataref->ref = dr;
      dataref->stmt = DR_STMT (dr);
      dataref->offset = 0;
      dataref->distance = 0;

      dataref->always_accessed
	      = dominated_by_p (CDI_DOMINATORS, last_always_executed,
				gimple_bb (dataref->stmt));
      dataref->pos = comp->refs.length ();
      comp->refs.quick_push (dataref);
    }

  if (eliminate_store_p)
    {
      bitmap_iterator bi;
      EXECUTE_IF_SET_IN_BITMAP (no_store_store_comps, 0, ia, bi)
	{
	  ca = component_of (comp_father, ia);
	  if (ca != bad)
	    comps[ca]->eliminate_store_p = false;
	}
    }

  for (i = 0; i < n; i++)
    {
      comp = comps[i];
      if (comp)
	{
	  comp->next = comp_list;
	  comp_list = comp;
	}
    }
  return comp_list;
}

/* Returns true if the component COMP satisfies the conditions
   described in 2) at the beginning of this file.  */

bool
pcom_worker::suitable_component_p (struct component *comp)
{
  unsigned i;
  dref a, first;
  basic_block ba, bp = m_loop->header;
  bool ok, has_write = false;

  FOR_EACH_VEC_ELT (comp->refs, i, a)
    {
      ba = gimple_bb (a->stmt);

      if (!just_once_each_iteration_p (m_loop, ba))
	return false;

      gcc_assert (dominated_by_p (CDI_DOMINATORS, ba, bp));
      bp = ba;

      if (DR_IS_WRITE (a->ref))
	has_write = true;
    }

  first = comp->refs[0];
  ok = suitable_reference_p (first->ref, &comp->comp_step);
  gcc_assert (ok);
  first->offset = 0;

  FOR_EACH_VEC_ELT (comp->refs, i, a)
    {
      if (has_write && comp->comp_step == RS_NONZERO)
	{
	  /* Punt for non-invariant references where step isn't a multiple
	     of reference size.  If step is smaller than reference size,
	     it overlaps the access in next iteration, if step is larger,
	     but not multiple of the access size, there could be overlap
	     in some later iteration.  There might be more latent issues
	     about this in predcom or data reference analysis.  If the
	     reference is a COMPONENT_REF, also check if step isn't a
	     multiple of the containg aggregate size.  See PR111683.  */
	  tree ref = DR_REF (a->ref);
	  tree step = DR_STEP (a->ref);
	  if (TREE_CODE (ref) == COMPONENT_REF
	      && DECL_BIT_FIELD (TREE_OPERAND (ref, 1)))
	    ref = TREE_OPERAND (ref, 0);
	  do
	    {
	      tree sz = TYPE_SIZE_UNIT (TREE_TYPE (ref));
	      if (TREE_CODE (sz) != INTEGER_CST)
		return false;
	      if (wi::multiple_of_p (wi::to_offset (step),
				     wi::to_offset (sz), SIGNED))
		break;
	      if (TREE_CODE (ref) != COMPONENT_REF)
		return false;
	      ref = TREE_OPERAND (ref, 0);
	    }
	  while (1);
	}
      if (i == 0)
	continue;
      /* Polynomial offsets are no use, since we need to know the
	 gap between iteration numbers at compile time.  */
      poly_widest_int offset;
      if (!determine_offset (first->ref, a->ref, &offset)
	  || !offset.is_constant (&a->offset))
	return false;

      enum ref_step_type a_step;
      gcc_checking_assert (suitable_reference_p (a->ref, &a_step)
			   && a_step == comp->comp_step);
    }

  /* If there is a write inside the component, we must know whether the
     step is nonzero or not -- we would not otherwise be able to recognize
     whether the value accessed by reads comes from the OFFSET-th iteration
     or the previous one.  */
  if (has_write && comp->comp_step == RS_ANY)
    return false;

  return true;
}

/* Check the conditions on references inside each of components COMPS,
   and remove the unsuitable components from the list.  The new list
   of components is returned.  The conditions are described in 2) at
   the beginning of this file.  */

struct component *
pcom_worker::filter_suitable_components (struct component *comps)
{
  struct component **comp, *act;

  for (comp = &comps; *comp; )
    {
      act = *comp;
      if (suitable_component_p (act))
	comp = &act->next;
      else
	{
	  dref ref;
	  unsigned i;

	  *comp = act->next;
	  FOR_EACH_VEC_ELT (act->refs, i, ref)
	    free (ref);
	  delete act;
	}
    }

  return comps;
}

/* Compares two drefs A and B by their offset and position.  Callback for
   qsort.  */

static int
order_drefs (const void *a, const void *b)
{
  const dref *const da = (const dref *) a;
  const dref *const db = (const dref *) b;
  int offcmp = wi::cmps ((*da)->offset, (*db)->offset);

  if (offcmp != 0)
    return offcmp;

  return (*da)->pos - (*db)->pos;
}

/* Compares two drefs A and B by their position.  Callback for qsort.  */

static int
order_drefs_by_pos (const void *a, const void *b)
{
  const dref *const da = (const dref *) a;
  const dref *const db = (const dref *) b;

  return (*da)->pos - (*db)->pos;
}

/* Returns root of the CHAIN.  */

static inline dref
get_chain_root (chain_p chain)
{
  return chain->refs[0];
}

/* Given CHAIN, returns the last write ref at DISTANCE, or NULL if it doesn't
   exist.  */

static inline dref
get_chain_last_write_at (chain_p chain, unsigned distance)
{
  for (unsigned i = chain->refs.length (); i > 0; i--)
    if (DR_IS_WRITE (chain->refs[i - 1]->ref)
	&& distance == chain->refs[i - 1]->distance)
      return chain->refs[i - 1];

  return NULL;
}

/* Given CHAIN, returns the last write ref with the same distance before load
   at index LOAD_IDX, or NULL if it doesn't exist.  */

static inline dref
get_chain_last_write_before_load (chain_p chain, unsigned load_idx)
{
  gcc_assert (load_idx < chain->refs.length ());

  unsigned distance = chain->refs[load_idx]->distance;

  for (unsigned i = load_idx; i > 0; i--)
    if (DR_IS_WRITE (chain->refs[i - 1]->ref)
	&& distance == chain->refs[i - 1]->distance)
      return chain->refs[i - 1];

  return NULL;
}

/* Adds REF to the chain CHAIN.  */

static void
add_ref_to_chain (chain_p chain, dref ref)
{
  dref root = get_chain_root (chain);

  gcc_assert (wi::les_p (root->offset, ref->offset));
  widest_int dist = ref->offset - root->offset;
  gcc_assert (wi::fits_uhwi_p (dist));

  chain->refs.safe_push (ref);

  ref->distance = dist.to_uhwi ();

  if (ref->distance >= chain->length)
    {
      chain->length = ref->distance;
      chain->has_max_use_after = false;
    }

  /* Promote this chain to CT_STORE_STORE if it has multiple stores.  */
  if (DR_IS_WRITE (ref->ref))
    chain->type = CT_STORE_STORE;

  /* Don't set the flag for store-store chain since there is no use.  */
  if (chain->type != CT_STORE_STORE
      && ref->distance == chain->length
      && ref->pos > root->pos)
    chain->has_max_use_after = true;

  chain->all_always_accessed &= ref->always_accessed;
}

/* Returns the chain for invariant component COMP.  */

static chain_p
make_invariant_chain (struct component *comp)
{
  chain_p chain = new struct chain (CT_INVARIANT);
  unsigned i;
  dref ref;

  chain->all_always_accessed = true;

  FOR_EACH_VEC_ELT (comp->refs, i, ref)
    {
      chain->refs.safe_push (ref);
      chain->all_always_accessed &= ref->always_accessed;
    }

  chain->inits = vNULL;
  chain->finis = vNULL;

  return chain;
}

/* Make a new chain of type TYPE rooted at REF.  */

static chain_p
make_rooted_chain (dref ref, enum chain_type type)
{
  chain_p chain = new struct chain (type);

  chain->refs.safe_push (ref);
  chain->all_always_accessed = ref->always_accessed;
  ref->distance = 0;

  chain->inits = vNULL;
  chain->finis = vNULL;

  return chain;
}

/* Returns true if CHAIN is not trivial.  */

static bool
nontrivial_chain_p (chain_p chain)
{
  return chain != NULL && chain->refs.length () > 1;
}

/* Returns the ssa name that contains the value of REF, or NULL_TREE if there
   is no such name.  */

static tree
name_for_ref (dref ref)
{
  tree name;

  if (is_gimple_assign (ref->stmt))
    {
      if (!ref->ref || DR_IS_READ (ref->ref))
	name = gimple_assign_lhs (ref->stmt);
      else
	name = gimple_assign_rhs1 (ref->stmt);
    }
  else
    name = PHI_RESULT (ref->stmt);

  return (TREE_CODE (name) == SSA_NAME ? name : NULL_TREE);
}

/* Returns true if REF is a valid initializer for ROOT with given DISTANCE (in
   iterations of the innermost enclosing loop).  */

bool
pcom_worker::valid_initializer_p (struct data_reference *ref, unsigned distance,
				  struct data_reference *root)
{
  aff_tree diff, base, step;
  poly_widest_int off;

  /* Both REF and ROOT must be accessing the same object.  */
  if (!operand_equal_p (DR_BASE_ADDRESS (ref), DR_BASE_ADDRESS (root), 0))
    return false;

  /* The initializer is defined outside of loop, hence its address must be
     invariant inside the loop.  */
  gcc_assert (integer_zerop (DR_STEP (ref)));

  /* If the address of the reference is invariant, initializer must access
     exactly the same location.  */
  if (integer_zerop (DR_STEP (root)))
    return (operand_equal_p (DR_OFFSET (ref), DR_OFFSET (root), 0)
	    && operand_equal_p (DR_INIT (ref), DR_INIT (root), 0));

  /* Verify that this index of REF is equal to the root's index at
     -DISTANCE-th iteration.  */
  aff_combination_dr_offset (root, &diff);
  aff_combination_dr_offset (ref, &base);
  aff_combination_scale (&base, -1);
  aff_combination_add (&diff, &base);

  tree_to_aff_combination_expand (DR_STEP (root), TREE_TYPE (DR_STEP (root)),
				  &step, &m_cache);
  if (!aff_combination_constant_multiple_p (&diff, &step, &off))
    return false;

  if (maybe_ne (off, distance))
    return false;

  return true;
}

/* Finds looparound phi node of loop that copies the value of REF, and if its
   initial value is correct (equal to initial value of REF shifted by one
   iteration), returns the phi node.  Otherwise, NULL_TREE is returned.  ROOT
   is the root of the current chain.  */

gphi *
pcom_worker::find_looparound_phi (dref ref, dref root)
{
  tree name, init, init_ref;
  gimple *init_stmt;
  edge latch = loop_latch_edge (m_loop);
  struct data_reference init_dr;
  gphi_iterator psi;

  if (is_gimple_assign (ref->stmt))
    {
      if (DR_IS_READ (ref->ref))
	name = gimple_assign_lhs (ref->stmt);
      else
	name = gimple_assign_rhs1 (ref->stmt);
    }
  else
    name = PHI_RESULT (ref->stmt);
  if (!name)
    return NULL;

  tree entry_vuse = NULL_TREE;
  gphi *phi = NULL;
  for (psi = gsi_start_phis (m_loop->header); !gsi_end_p (psi); gsi_next (&psi))
    {
      gphi *p = psi.phi ();
      if (PHI_ARG_DEF_FROM_EDGE (p, latch) == name)
	phi = p;
      else if (virtual_operand_p (gimple_phi_result (p)))
	entry_vuse = PHI_ARG_DEF_FROM_EDGE (p, loop_preheader_edge (m_loop));
      if (phi && entry_vuse)
	break;
    }
  if (!phi || !entry_vuse)
    return NULL;

  init = PHI_ARG_DEF_FROM_EDGE (phi, loop_preheader_edge (m_loop));
  if (TREE_CODE (init) != SSA_NAME)
    return NULL;
  init_stmt = SSA_NAME_DEF_STMT (init);
  if (gimple_code (init_stmt) != GIMPLE_ASSIGN)
    return NULL;
  gcc_assert (gimple_assign_lhs (init_stmt) == init);

  init_ref = gimple_assign_rhs1 (init_stmt);
  if (!REFERENCE_CLASS_P (init_ref)
      && !DECL_P (init_ref))
    return NULL;

  /* Analyze the behavior of INIT_REF with respect to LOOP (innermost
     loop enclosing PHI).  */
  memset (&init_dr, 0, sizeof (struct data_reference));
  DR_REF (&init_dr) = init_ref;
  DR_STMT (&init_dr) = phi;
  if (!dr_analyze_innermost (&DR_INNERMOST (&init_dr), init_ref, m_loop,
			     init_stmt))
    return NULL;

  if (!valid_initializer_p (&init_dr, ref->distance + 1, root->ref))
    return NULL;

  /* Make sure nothing clobbers the location we re-use the initial value
     from.  */
  if (entry_vuse != gimple_vuse (init_stmt))
    {
      ao_ref ref;
      ao_ref_init (&ref, init_ref);
      unsigned limit = param_sccvn_max_alias_queries_per_access;
      tree vdef = entry_vuse;
      do
	{
	  gimple *def = SSA_NAME_DEF_STMT (vdef);
	  if (limit-- == 0 || gimple_code (def) == GIMPLE_PHI)
	    return NULL;
	  if (stmt_may_clobber_ref_p_1 (def, &ref))
	    /* When the stmt is an assign to init_ref we could in theory
	       use its RHS for the initial value of the looparound PHI
	       we replace in prepare_initializers_chain, but we have
	       no convenient place to store this info at the moment.  */
	    return NULL;
	  vdef = gimple_vuse (def);
	}
      while (vdef != gimple_vuse (init_stmt));
    }

  return phi;
}

/* Adds a reference for the looparound copy of REF in PHI to CHAIN.  */

static void
insert_looparound_copy (chain_p chain, dref ref, gphi *phi)
{
  dref nw = XCNEW (class dref_d), aref;
  unsigned i;

  nw->stmt = phi;
  nw->distance = ref->distance + 1;
  nw->always_accessed = 1;

  FOR_EACH_VEC_ELT (chain->refs, i, aref)
    if (aref->distance >= nw->distance)
      break;
  chain->refs.safe_insert (i, nw);

  if (nw->distance > chain->length)
    {
      chain->length = nw->distance;
      chain->has_max_use_after = false;
    }
}

/* For references in CHAIN that are copied around the loop (created previously
   by PRE, or by user), add the results of such copies to the chain.  This
   enables us to remove the copies by unrolling, and may need less registers
   (also, it may allow us to combine chains together).  */

void
pcom_worker::add_looparound_copies (chain_p chain)
{
  unsigned i;
  dref ref, root = get_chain_root (chain);
  gphi *phi;

  if (chain->type == CT_STORE_STORE)
    return;

  FOR_EACH_VEC_ELT (chain->refs, i, ref)
    {
      phi = find_looparound_phi (ref, root);
      if (!phi)
	continue;

      bitmap_set_bit (m_looparound_phis, SSA_NAME_VERSION (PHI_RESULT (phi)));
      insert_looparound_copy (chain, ref, phi);
    }
}

/* Find roots of the values and determine distances in the component COMP.
   The references are redistributed into chains.  */

void
pcom_worker::determine_roots_comp (struct component *comp)
{
  unsigned i;
  dref a;
  chain_p chain = NULL;
  widest_int last_ofs = 0;
  enum chain_type type;

  /* Invariants are handled specially.  */
  if (comp->comp_step == RS_INVARIANT)
    {
      chain = make_invariant_chain (comp);
      m_chains.safe_push (chain);
      return;
    }

  /* Trivial component.  */
  if (comp->refs.length () <= 1)
    {
      if (comp->refs.length () == 1)
	{
	  free (comp->refs[0]);
	  comp->refs.truncate (0);
	}
      return;
    }

  comp->refs.qsort (order_drefs);

  /* For Store-Store chain, we only support load if it is dominated by a
     store statement in the same iteration of loop.  */
  if (comp->eliminate_store_p)
    for (a = NULL, i = 0; i < comp->refs.length (); i++)
      {
	if (DR_IS_WRITE (comp->refs[i]->ref))
	  a = comp->refs[i];
	else if (a == NULL || a->offset != comp->refs[i]->offset)
	  {
	    /* If there is load that is not dominated by a store in the
	       same iteration of loop, clear the flag so no Store-Store
	       chain is generated for this component.  */
	    comp->eliminate_store_p = false;
	    break;
	  }
      }

  /* Determine roots and create chains for components.  */
  FOR_EACH_VEC_ELT (comp->refs, i, a)
    {
      if (!chain
	  || (chain->type == CT_LOAD && DR_IS_WRITE (a->ref))
	  || (!comp->eliminate_store_p && DR_IS_WRITE (a->ref))
	  || wi::leu_p (MAX_DISTANCE, a->offset - last_ofs))
	{
	  if (nontrivial_chain_p (chain))
	    {
	      add_looparound_copies (chain);
	      m_chains.safe_push (chain);
	    }
	  else
	    release_chain (chain);

	  /* Determine type of the chain.  If the root reference is a load,
	     this can only be a CT_LOAD chain; other chains are intialized
	     to CT_STORE_LOAD and might be promoted to CT_STORE_STORE when
	     new reference is added.  */
	  type = DR_IS_READ (a->ref) ? CT_LOAD : CT_STORE_LOAD;
	  chain = make_rooted_chain (a, type);
	  last_ofs = a->offset;
	  continue;
	}

      add_ref_to_chain (chain, a);
    }

  if (nontrivial_chain_p (chain))
    {
      add_looparound_copies (chain);
      m_chains.safe_push (chain);
    }
  else
    release_chain (chain);
}

/* Find roots of the values and determine distances in components COMPS, and
   separates the references to chains.  */

void
pcom_worker::determine_roots (struct component *comps)
{
  struct component *comp;

  for (comp = comps; comp; comp = comp->next)
    determine_roots_comp (comp);
}

/* Replace the reference in statement STMT with temporary variable
   NEW_TREE.  If SET is true, NEW_TREE is instead initialized to the value of
   the reference in the statement.  IN_LHS is true if the reference
   is in the lhs of STMT, false if it is in rhs.  */

static void
replace_ref_with (gimple *stmt, tree new_tree, bool set, bool in_lhs)
{
  tree val;
  gassign *new_stmt;
  gimple_stmt_iterator bsi, psi;

  if (gimple_code (stmt) == GIMPLE_PHI)
    {
      gcc_assert (!in_lhs && !set);

      val = PHI_RESULT (stmt);
      bsi = gsi_after_labels (gimple_bb (stmt));
      psi = gsi_for_stmt (stmt);
      remove_phi_node (&psi, false);

      /* Turn the phi node into GIMPLE_ASSIGN.  */
      new_stmt = gimple_build_assign (val, new_tree);
      gsi_insert_before (&bsi, new_stmt, GSI_NEW_STMT);
      return;
    }

  /* Since the reference is of gimple_reg type, it should only
     appear as lhs or rhs of modify statement.  */
  gcc_assert (is_gimple_assign (stmt));

  bsi = gsi_for_stmt (stmt);

  /* If we do not need to initialize NEW_TREE, just replace the use of OLD.  */
  if (!set)
    {
      gcc_assert (!in_lhs);
      gimple_assign_set_rhs_from_tree (&bsi, new_tree);
      stmt = gsi_stmt (bsi);
      update_stmt (stmt);
      return;
    }

  if (in_lhs)
    {
      /* We have statement

	 OLD = VAL

	 If OLD is a memory reference, then VAL is gimple_val, and we transform
	 this to

	 OLD = VAL
	 NEW = VAL

	 Otherwise, we are replacing a combination chain,
	 VAL is the expression that performs the combination, and OLD is an
	 SSA name.  In this case, we transform the assignment to

	 OLD = VAL
	 NEW = OLD

	 */

      val = gimple_assign_lhs (stmt);
      if (TREE_CODE (val) != SSA_NAME)
	{
	  val = gimple_assign_rhs1 (stmt);
	  gcc_assert (gimple_assign_single_p (stmt));
	  if (TREE_CLOBBER_P (val))
	    val = get_or_create_ssa_default_def (cfun, SSA_NAME_VAR (new_tree));
	  else
	    gcc_assert (gimple_assign_copy_p (stmt));
	}
    }
  else
    {
      /* VAL = OLD

	 is transformed to

	 VAL = OLD
	 NEW = VAL  */

      val = gimple_assign_lhs (stmt);
    }

  new_stmt = gimple_build_assign (new_tree, unshare_expr (val));
  gsi_insert_after (&bsi, new_stmt, GSI_NEW_STMT);
}

/* Returns a memory reference to DR in the (NITERS + ITER)-th iteration
   of the loop it was analyzed in.  Append init stmts to STMTS.  */

static tree
ref_at_iteration (data_reference_p dr, int iter,
		  gimple_seq *stmts, tree niters = NULL_TREE)
{
  tree off = DR_OFFSET (dr);
  tree coff = DR_INIT (dr);
  tree ref = DR_REF (dr);
  enum tree_code ref_code = ERROR_MARK;
  tree ref_type = NULL_TREE;
  tree ref_op1 = NULL_TREE;
  tree ref_op2 = NULL_TREE;
  tree new_offset;

  if (iter != 0)
    {
      new_offset = size_binop (MULT_EXPR, DR_STEP (dr), ssize_int (iter));
      if (TREE_CODE (new_offset) == INTEGER_CST)
	coff = size_binop (PLUS_EXPR, coff, new_offset);
      else
	off = size_binop (PLUS_EXPR, off, new_offset);
    }

  if (niters != NULL_TREE)
    {
      niters = fold_convert (ssizetype, niters);
      new_offset = size_binop (MULT_EXPR, DR_STEP (dr), niters);
      if (TREE_CODE (niters) == INTEGER_CST)
	coff = size_binop (PLUS_EXPR, coff, new_offset);
      else
	off = size_binop (PLUS_EXPR, off, new_offset);
    }

  /* While data-ref analysis punts on bit offsets it still handles
     bitfield accesses at byte boundaries.  Cope with that.  Note that
     if the bitfield object also starts at a byte-boundary we can simply
     replicate the COMPONENT_REF, but we have to subtract the component's
     byte-offset from the MEM_REF address first.
     Otherwise we simply build a BIT_FIELD_REF knowing that the bits
     start at offset zero.  */
  if (TREE_CODE (ref) == COMPONENT_REF
      && DECL_BIT_FIELD (TREE_OPERAND (ref, 1)))
    {
      unsigned HOST_WIDE_INT boff;
      tree field = TREE_OPERAND (ref, 1);
      tree offset = component_ref_field_offset (ref);
      ref_type = TREE_TYPE (ref);
      boff = tree_to_uhwi (DECL_FIELD_BIT_OFFSET (field));
      /* This can occur in Ada.  See the comment in get_bit_range.  */
      if (boff % BITS_PER_UNIT != 0
	  || !tree_fits_uhwi_p (offset))
	{
	  ref_code = BIT_FIELD_REF;
	  ref_op1 = DECL_SIZE (field);
	  ref_op2 = bitsize_zero_node;
	}
      else
	{
	  boff >>= LOG2_BITS_PER_UNIT;
	  boff += tree_to_uhwi (offset);
	  coff = size_binop (MINUS_EXPR, coff, ssize_int (boff));
	  ref_code = COMPONENT_REF;
	  ref_op1 = field;
	  ref_op2 = TREE_OPERAND (ref, 2);
	  ref = TREE_OPERAND (ref, 0);
	}
    }
  /* We may not associate the constant offset across the pointer plus
     expression because that might form a pointer to before the object
     then.  But for some cases we can retain that to allow tree_could_trap_p
     to return false - see gcc.dg/tree-ssa/predcom-1.c  */
  tree addr, alias_ptr;
  if (integer_zerop  (off))
    {
      alias_ptr = fold_convert (reference_alias_ptr_type (ref), coff);
      addr = DR_BASE_ADDRESS (dr);
    }
  else
    {
      alias_ptr = build_zero_cst (reference_alias_ptr_type (ref));
      off = size_binop (PLUS_EXPR, off, coff);
      addr = fold_build_pointer_plus (DR_BASE_ADDRESS (dr), off);
    }
  addr = force_gimple_operand_1 (unshare_expr (addr), stmts,
				 is_gimple_mem_ref_addr, NULL_TREE);
  tree type = build_aligned_type (TREE_TYPE (ref),
				  get_object_alignment (ref));
  ref = build2 (MEM_REF, type, addr, alias_ptr);
  if (ref_type)
    ref = build3 (ref_code, ref_type, ref, ref_op1, ref_op2);
  return ref;
}

/* Get the initialization expression for the INDEX-th temporary variable
   of CHAIN.  */

static tree
get_init_expr (chain_p chain, unsigned index)
{
  if (chain->type == CT_COMBINATION)
    {
      tree e1 = get_init_expr (chain->ch1, index);
      tree e2 = get_init_expr (chain->ch2, index);

      return fold_build2 (chain->op, chain->rslt_type, e1, e2);
    }
  else
    return chain->inits[index];
}

/* Returns a new temporary variable used for the I-th variable carrying
   value of REF.  The variable's uid is marked in TMP_VARS.  */

static tree
predcom_tmp_var (tree ref, unsigned i, bitmap tmp_vars)
{
  tree type = TREE_TYPE (ref);
  /* We never access the components of the temporary variable in predictive
     commoning.  */
  tree var = create_tmp_reg (type, get_lsm_tmp_name (ref, i));
  bitmap_set_bit (tmp_vars, DECL_UID (var));
  return var;
}

/* Creates the variables for CHAIN, as well as phi nodes for them and
   initialization on entry to LOOP.  Uids of the newly created
   temporary variables are marked in TMP_VARS.  */

static void
initialize_root_vars (class loop *loop, chain_p chain, bitmap tmp_vars)
{
  unsigned i;
  unsigned n = chain->length;
  dref root = get_chain_root (chain);
  bool reuse_first = !chain->has_max_use_after;
  tree ref, init, var, next;
  gphi *phi;
  gimple_seq stmts;
  edge entry = loop_preheader_edge (loop), latch = loop_latch_edge (loop);

  /* If N == 0, then all the references are within the single iteration.  And
     since this is an nonempty chain, reuse_first cannot be true.  */
  gcc_assert (n > 0 || !reuse_first);

  chain->vars.create (n + 1);

  if (chain->type == CT_COMBINATION)
    ref = gimple_assign_lhs (root->stmt);
  else
    ref = DR_REF (root->ref);

  for (i = 0; i < n + (reuse_first ? 0 : 1); i++)
    {
      var = predcom_tmp_var (ref, i, tmp_vars);
      chain->vars.quick_push (var);
    }
  if (reuse_first)
    chain->vars.quick_push (chain->vars[0]);

  FOR_EACH_VEC_ELT (chain->vars, i, var)
    chain->vars[i] = make_ssa_name (var);

  for (i = 0; i < n; i++)
    {
      var = chain->vars[i];
      next = chain->vars[i + 1];
      init = get_init_expr (chain, i);

      init = force_gimple_operand (init, &stmts, true, NULL_TREE);
      if (stmts)
	gsi_insert_seq_on_edge_immediate (entry, stmts);

      phi = create_phi_node (var, loop->header);
      add_phi_arg (phi, init, entry, UNKNOWN_LOCATION);
      add_phi_arg (phi, next, latch, UNKNOWN_LOCATION);
    }
}

/* For inter-iteration store elimination CHAIN in LOOP, returns true if
   all stores to be eliminated store loop invariant values into memory.
   In this case, we can use these invariant values directly after LOOP.  */

static bool
is_inv_store_elimination_chain (class loop *loop, chain_p chain)
{
  if (chain->length == 0 || chain->type != CT_STORE_STORE)
    return false;

  gcc_assert (!chain->has_max_use_after);

  /* If loop iterates for unknown times or fewer times than chain->length,
     we still need to setup root variable and propagate it with PHI node.  */
  tree niters = number_of_latch_executions (loop);
  if (TREE_CODE (niters) != INTEGER_CST
      || wi::leu_p (wi::to_wide (niters), chain->length))
    return false;

  /* Check stores in chain for elimination if they only store loop invariant
     values.  */
  for (unsigned i = 0; i < chain->length; i++)
    {
      dref a = get_chain_last_write_at (chain, i);
      if (a == NULL)
	continue;

      gimple *def_stmt, *stmt = a->stmt;
      if (!gimple_assign_single_p (stmt))
	return false;

      tree val = gimple_assign_rhs1 (stmt);
      if (TREE_CLOBBER_P (val))
	return false;

      if (CONSTANT_CLASS_P (val))
	continue;

      if (TREE_CODE (val) != SSA_NAME)
	return false;

      def_stmt = SSA_NAME_DEF_STMT (val);
      if (gimple_nop_p (def_stmt))
	continue;

      if (flow_bb_inside_loop_p (loop, gimple_bb (def_stmt)))
	return false;
    }
  return true;
}

/* Creates root variables for store elimination CHAIN in which stores for
   elimination only store loop invariant values.  In this case, we neither
   need to load root variables before loop nor propagate it with PHI nodes.  */

static void
initialize_root_vars_store_elim_1 (chain_p chain)
{
  tree var;
  unsigned i, n = chain->length;

  chain->vars.create (n);
  chain->vars.safe_grow_cleared (n, true);

  /* Initialize root value for eliminated stores at each distance.  */
  for (i = 0; i < n; i++)
    {
      dref a = get_chain_last_write_at (chain, i);
      if (a == NULL)
	continue;

      var = gimple_assign_rhs1 (a->stmt);
      chain->vars[a->distance] = var;
    }

  /* We don't propagate values with PHI nodes, so manually propagate value
     to bubble positions.  */
  var = chain->vars[0];
  for (i = 1; i < n; i++)
    {
      if (chain->vars[i] != NULL_TREE)
	{
	  var = chain->vars[i];
	  continue;
	}
      chain->vars[i] = var;
    }

  /* Revert the vector.  */
  for (i = 0; i < n / 2; i++)
    std::swap (chain->vars[i], chain->vars[n - i - 1]);
}

/* Creates root variables for store elimination CHAIN in which stores for
   elimination store loop variant values.  In this case, we may need to
   load root variables before LOOP and propagate it with PHI nodes.  Uids
   of the newly created root variables are marked in TMP_VARS.  */

static void
initialize_root_vars_store_elim_2 (class loop *loop,
				   chain_p chain, bitmap tmp_vars)
{
  unsigned i, n = chain->length;
  tree ref, init, var, next, val, phi_result;
  gimple *stmt;
  gimple_seq stmts;

  chain->vars.create (n);

  ref = DR_REF (get_chain_root (chain)->ref);
  for (i = 0; i < n; i++)
    {
      var = predcom_tmp_var (ref, i, tmp_vars);
      chain->vars.quick_push (var);
    }

  FOR_EACH_VEC_ELT (chain->vars, i, var)
    chain->vars[i] = make_ssa_name (var);

  /* Root values are either rhs operand of stores to be eliminated, or
     loaded from memory before loop.  */
  auto_vec<tree> vtemps;
  vtemps.safe_grow_cleared (n, true);
  for (i = 0; i < n; i++)
    {
      init = get_init_expr (chain, i);
      if (init == NULL_TREE)
	{
	  /* Root value is rhs operand of the store to be eliminated if
	     it isn't loaded from memory before loop.  */
	  dref a = get_chain_last_write_at (chain, i);
	  val = gimple_assign_rhs1 (a->stmt);
	  if (TREE_CLOBBER_P (val))
	    {
	      val = get_or_create_ssa_default_def (cfun, SSA_NAME_VAR (var));
	      gimple_assign_set_rhs1 (a->stmt, val);
	    }

	  vtemps[n - i - 1] = val;
	}
      else
	{
	  edge latch = loop_latch_edge (loop);
	  edge entry = loop_preheader_edge (loop);

	  /* Root value is loaded from memory before loop, we also need
	     to add PHI nodes to propagate the value across iterations.  */
	  init = force_gimple_operand (init, &stmts, true, NULL_TREE);
	  if (stmts)
	    gsi_insert_seq_on_edge_immediate (entry, stmts);

	  next = chain->vars[n - i];
	  phi_result = copy_ssa_name (next);
	  gphi *phi = create_phi_node (phi_result, loop->header);
	  add_phi_arg (phi, init, entry, UNKNOWN_LOCATION);
	  add_phi_arg (phi, next, latch, UNKNOWN_LOCATION);
	  vtemps[n - i - 1] = phi_result;
	}
    }

  /* Find the insertion position.  */
  dref last = get_chain_root (chain);
  for (i = 0; i < chain->refs.length (); i++)
    {
      if (chain->refs[i]->pos > last->pos)
	last = chain->refs[i];
    }

  gimple_stmt_iterator gsi = gsi_for_stmt (last->stmt);

  /* Insert statements copying root value to root variable.  */
  for (i = 0; i < n; i++)
    {
      var = chain->vars[i];
      val = vtemps[i];
      stmt = gimple_build_assign (var, val);
      gsi_insert_after (&gsi, stmt, GSI_NEW_STMT);
    }
}

/* Generates stores for CHAIN's eliminated stores in LOOP's last
   (CHAIN->length - 1) iterations.  */

static void
finalize_eliminated_stores (class loop *loop, chain_p chain)
{
  unsigned i, n = chain->length;

  for (i = 0; i < n; i++)
    {
      tree var = chain->vars[i];
      tree fini = chain->finis[n - i - 1];
      gimple *stmt = gimple_build_assign (fini, var);

      gimple_seq_add_stmt_without_update (&chain->fini_seq, stmt);
    }

  if (chain->fini_seq)
    {
      gsi_insert_seq_on_edge_immediate (single_exit (loop), chain->fini_seq);
      chain->fini_seq = NULL;
    }
}

/* Initializes a variable for load motion for ROOT and prepares phi nodes and
   initialization on entry to LOOP if necessary.  The ssa name for the variable
   is stored in VARS.  If WRITTEN is true, also a phi node to copy its value
   around the loop is created.  Uid of the newly created temporary variable
   is marked in TMP_VARS.  INITS is the list containing the (single)
   initializer.  */

static void
initialize_root_vars_lm (class loop *loop, dref root, bool written,
			 vec<tree> *vars, const vec<tree> &inits,
			 bitmap tmp_vars)
{
  unsigned i;
  tree ref = DR_REF (root->ref), init, var, next;
  gimple_seq stmts;
  gphi *phi;
  edge entry = loop_preheader_edge (loop), latch = loop_latch_edge (loop);

  /* Find the initializer for the variable, and check that it cannot
     trap.  */
  init = inits[0];

  vars->create (written ? 2 : 1);
  var = predcom_tmp_var (ref, 0, tmp_vars);
  vars->quick_push (var);
  if (written)
    vars->quick_push ((*vars)[0]);

  FOR_EACH_VEC_ELT (*vars, i, var)
    (*vars)[i] = make_ssa_name (var);

  var = (*vars)[0];

  init = force_gimple_operand (init, &stmts, written, NULL_TREE);
  if (stmts)
    gsi_insert_seq_on_edge_immediate (entry, stmts);

  if (written)
    {
      next = (*vars)[1];
      phi = create_phi_node (var, loop->header);
      add_phi_arg (phi, init, entry, UNKNOWN_LOCATION);
      add_phi_arg (phi, next, latch, UNKNOWN_LOCATION);
    }
  else
    {
      gassign *init_stmt = gimple_build_assign (var, init);
      gsi_insert_on_edge_immediate (entry, init_stmt);
    }
}


/* Execute load motion for references in chain CHAIN.  Uids of the newly
   created temporary variables are marked in TMP_VARS.  */

static void
execute_load_motion (class loop *loop, chain_p chain, bitmap tmp_vars)
{
  auto_vec<tree> vars;
  dref a;
  unsigned n_writes = 0, ridx, i;
  tree var;

  gcc_assert (chain->type == CT_INVARIANT);
  gcc_assert (!chain->combined);
  FOR_EACH_VEC_ELT (chain->refs, i, a)
    if (DR_IS_WRITE (a->ref))
      n_writes++;

  /* If there are no reads in the loop, there is nothing to do.  */
  if (n_writes == chain->refs.length ())
    return;

  initialize_root_vars_lm (loop, get_chain_root (chain), n_writes > 0,
			   &vars, chain->inits, tmp_vars);

  ridx = 0;
  FOR_EACH_VEC_ELT (chain->refs, i, a)
    {
      bool is_read = DR_IS_READ (a->ref);

      if (DR_IS_WRITE (a->ref))
	{
	  n_writes--;
	  if (n_writes)
	    {
	      var = vars[0];
	      var = make_ssa_name (SSA_NAME_VAR (var));
	      vars[0] = var;
	    }
	  else
	    ridx = 1;
	}

      replace_ref_with (a->stmt, vars[ridx],
			!is_read, !is_read);
    }
}

/* Returns the single statement in that NAME is used, excepting
   the looparound phi nodes contained in one of the chains.  If there is no
   such statement, or more statements, NULL is returned.  */

gimple *
pcom_worker::single_nonlooparound_use (tree name)
{
  use_operand_p use;
  imm_use_iterator it;
  gimple *stmt, *ret = NULL;

  FOR_EACH_IMM_USE_FAST (use, it, name)
    {
      stmt = USE_STMT (use);

      if (gimple_code (stmt) == GIMPLE_PHI)
	{
	  /* Ignore uses in looparound phi nodes.  Uses in other phi nodes
	     could not be processed anyway, so just fail for them.  */
	  if (bitmap_bit_p (m_looparound_phis,
			    SSA_NAME_VERSION (PHI_RESULT (stmt))))
	    continue;

	  return NULL;
	}
      else if (is_gimple_debug (stmt))
	continue;
      else if (ret != NULL)
	return NULL;
      else
	ret = stmt;
    }

  return ret;
}

/* Remove statement STMT, as well as the chain of assignments in that it is
   used.  */

void
pcom_worker::remove_stmt (gimple *stmt)
{
  tree name;
  gimple *next;
  gimple_stmt_iterator psi;

  if (gimple_code (stmt) == GIMPLE_PHI)
    {
      name = PHI_RESULT (stmt);
      next = single_nonlooparound_use (name);
      reset_debug_uses (stmt);
      psi = gsi_for_stmt (stmt);
      remove_phi_node (&psi, true);

      if (!next
	  || !gimple_assign_ssa_name_copy_p (next)
	  || gimple_assign_rhs1 (next) != name)
	return;

      stmt = next;
    }

  while (1)
    {
      gimple_stmt_iterator bsi;

      bsi = gsi_for_stmt (stmt);

      name = gimple_assign_lhs (stmt);
      if (TREE_CODE (name) == SSA_NAME)
	{
	  next = single_nonlooparound_use (name);
	  reset_debug_uses (stmt);
	}
      else
	{
	  /* This is a store to be eliminated.  */
	  gcc_assert (gimple_vdef (stmt) != NULL);
	  next = NULL;
	}

      unlink_stmt_vdef (stmt);
      gsi_remove (&bsi, true);
      release_defs (stmt);

      if (!next
	  || !gimple_assign_ssa_name_copy_p (next)
	  || gimple_assign_rhs1 (next) != name)
	return;

      stmt = next;
    }
}

/* Perform the predictive commoning optimization for a chain CHAIN.
   Uids of the newly created temporary variables are marked in TMP_VARS.*/

void
pcom_worker::execute_pred_commoning_chain (chain_p chain,
			      bitmap tmp_vars)
{
  unsigned i;
  dref a;
  tree var;
  bool in_lhs;

  if (chain->combined)
    {
      /* For combined chains, just remove the statements that are used to
	 compute the values of the expression (except for the root one).
	 We delay this until after all chains are processed.  */
    }
  else if (chain->type == CT_STORE_STORE)
    {
      if (chain->length > 0)
	{
	  if (chain->inv_store_elimination)
	    {
	      /* If dead stores in this chain only store loop invariant
		 values, we can simply record the invariant value and use
		 it directly after loop.  */
	      initialize_root_vars_store_elim_1 (chain);
	    }
	  else
	    {
	      /* If dead stores in this chain store loop variant values,
		 we need to set up the variables by loading from memory
		 before loop and propagating it with PHI nodes.  */
	      initialize_root_vars_store_elim_2 (m_loop, chain, tmp_vars);
	    }

	  /* For inter-iteration store elimination chain, stores at each
	     distance in loop's last (chain->length - 1) iterations can't
	     be eliminated, because there is no following killing store.
	     We need to generate these stores after loop.  */
	  finalize_eliminated_stores (m_loop, chain);
	}

      bool last_store_p = true;
      for (i = chain->refs.length (); i > 0; i--)
	{
	  a = chain->refs[i - 1];
	  /* Preserve the last store of the chain.  Eliminate other stores
	     which are killed by the last one.  */
	  if (DR_IS_WRITE (a->ref))
	    {
	      if (last_store_p)
		last_store_p = false;
	      else
		remove_stmt (a->stmt);

	      continue;
	    }

	  /* Any load in Store-Store chain must be dominated by a previous
	     store, we replace the load reference with rhs of the store.  */
	  dref b = get_chain_last_write_before_load (chain, i - 1);
	  gcc_assert (b != NULL);
	  var = gimple_assign_rhs1 (b->stmt);
	  replace_ref_with (a->stmt, var, false, false);
	}
    }
  else
    {
      /* For non-combined chains, set up the variables that hold its value.  */
      initialize_root_vars (m_loop, chain, tmp_vars);
      a = get_chain_root (chain);
      in_lhs = (chain->type == CT_STORE_LOAD
		|| chain->type == CT_COMBINATION);
      replace_ref_with (a->stmt, chain->vars[chain->length], true, in_lhs);

      /* Replace the uses of the original references by these variables.  */
      for (i = 1; chain->refs.iterate (i, &a); i++)
	{
	  var = chain->vars[chain->length - a->distance];
	  replace_ref_with (a->stmt, var, false, false);
	}
    }
}

/* Determines the unroll factor necessary to remove as many temporary variable
   copies as possible.  CHAINS is the list of chains that will be
   optimized.  */

static unsigned
determine_unroll_factor (const vec<chain_p> &chains)
{
  chain_p chain;
  unsigned factor = 1, af, nfactor, i;
  unsigned max = param_max_unroll_times;

  FOR_EACH_VEC_ELT (chains, i, chain)
    {
      if (chain->type == CT_INVARIANT)
	continue;
      /* For now we can't handle unrolling when eliminating stores.  */
      else if (chain->type == CT_STORE_STORE)
	return 1;

      if (chain->combined)
	{
	  /* For combined chains, we can't handle unrolling if we replace
	     looparound PHIs.  */
	  dref a;
	  unsigned j;
	  for (j = 1; chain->refs.iterate (j, &a); j++)
	    if (gimple_code (a->stmt) == GIMPLE_PHI)
	      return 1;
	  continue;
	}

      /* The best unroll factor for this chain is equal to the number of
	 temporary variables that we create for it.  */
      af = chain->length;
      if (chain->has_max_use_after)
	af++;

      nfactor = factor * af / gcd (factor, af);
      if (nfactor <= max)
	factor = nfactor;
    }

  return factor;
}

/* Perform the predictive commoning optimization for chains.
   Uids of the newly created temporary variables are marked in TMP_VARS.  */

void
pcom_worker::execute_pred_commoning (bitmap tmp_vars)
{
  chain_p chain;
  unsigned i;

  FOR_EACH_VEC_ELT (m_chains, i, chain)
    {
      if (chain->type == CT_INVARIANT)
	execute_load_motion (m_loop, chain, tmp_vars);
      else
	execute_pred_commoning_chain (chain, tmp_vars);
    }

  FOR_EACH_VEC_ELT (m_chains, i, chain)
    {
      if (chain->type == CT_INVARIANT)
	;
      else if (chain->combined)
	{
	  /* For combined chains, just remove the statements that are used to
	     compute the values of the expression (except for the root one).  */
	  dref a;
	  unsigned j;
	  for (j = 1; chain->refs.iterate (j, &a); j++)
	    remove_stmt (a->stmt);
	}
    }
}

/* For each reference in CHAINS, if its defining statement is
   phi node, record the ssa name that is defined by it.  */

static void
replace_phis_by_defined_names (vec<chain_p> &chains)
{
  chain_p chain;
  dref a;
  unsigned i, j;

  FOR_EACH_VEC_ELT (chains, i, chain)
    FOR_EACH_VEC_ELT (chain->refs, j, a)
      {
	if (gimple_code (a->stmt) == GIMPLE_PHI)
	  {
	    a->name_defined_by_phi = PHI_RESULT (a->stmt);
	    a->stmt = NULL;
	  }
      }
}

/* For each reference in CHAINS, if name_defined_by_phi is not
   NULL, use it to set the stmt field.  */

static void
replace_names_by_phis (vec<chain_p> chains)
{
  chain_p chain;
  dref a;
  unsigned i, j;

  FOR_EACH_VEC_ELT (chains, i, chain)
    FOR_EACH_VEC_ELT (chain->refs, j, a)
      if (a->stmt == NULL)
	{
	  a->stmt = SSA_NAME_DEF_STMT (a->name_defined_by_phi);
	  gcc_assert (gimple_code (a->stmt) == GIMPLE_PHI);
	  a->name_defined_by_phi = NULL_TREE;
	}
}

/* Wrapper over execute_pred_commoning, to pass it as a callback
   to tree_transform_and_unroll_loop.  */

struct epcc_data
{
  vec<chain_p> chains;
  bitmap tmp_vars;
  pcom_worker *worker;
};

static void
execute_pred_commoning_cbck (class loop *loop ATTRIBUTE_UNUSED, void *data)
{
  struct epcc_data *const dta = (struct epcc_data *) data;
  pcom_worker *worker = dta->worker;

  /* Restore phi nodes that were replaced by ssa names before
     tree_transform_and_unroll_loop (see detailed description in
     tree_predictive_commoning_loop).  */
  replace_names_by_phis (dta->chains);
  worker->execute_pred_commoning (dta->tmp_vars);
}

/* Base NAME and all the names in the chain of phi nodes that use it
   on variable VAR.  The phi nodes are recognized by being in the copies of
   the header of the LOOP.  */

static void
base_names_in_chain_on (class loop *loop, tree name, tree var)
{
  gimple *stmt, *phi;
  imm_use_iterator iter;

  replace_ssa_name_symbol (name, var);

  while (1)
    {
      phi = NULL;
      FOR_EACH_IMM_USE_STMT (stmt, iter, name)
	{
	  if (gimple_code (stmt) == GIMPLE_PHI
	      && flow_bb_inside_loop_p (loop, gimple_bb (stmt)))
	    {
	      phi = stmt;
	      break;
	    }
	}
      if (!phi)
	return;

      name = PHI_RESULT (phi);
      replace_ssa_name_symbol (name, var);
    }
}

/* Given an unrolled LOOP after predictive commoning, remove the
   register copies arising from phi nodes by changing the base
   variables of SSA names.  TMP_VARS is the set of the temporary variables
   for those we want to perform this.  */

static void
eliminate_temp_copies (class loop *loop, bitmap tmp_vars)
{
  edge e;
  gphi *phi;
  gimple *stmt;
  tree name, use, var;
  gphi_iterator psi;

  e = loop_latch_edge (loop);
  for (psi = gsi_start_phis (loop->header); !gsi_end_p (psi); gsi_next (&psi))
    {
      phi = psi.phi ();
      name = PHI_RESULT (phi);
      var = SSA_NAME_VAR (name);
      if (!var || !bitmap_bit_p (tmp_vars, DECL_UID (var)))
	continue;
      use = PHI_ARG_DEF_FROM_EDGE (phi, e);
      gcc_assert (TREE_CODE (use) == SSA_NAME);

      /* Base all the ssa names in the ud and du chain of NAME on VAR.  */
      stmt = SSA_NAME_DEF_STMT (use);
      while (gimple_code (stmt) == GIMPLE_PHI
	     /* In case we could not unroll the loop enough to eliminate
		all copies, we may reach the loop header before the defining
		statement (in that case, some register copies will be present
		in loop latch in the final code, corresponding to the newly
		created looparound phi nodes).  */
	     && gimple_bb (stmt) != loop->header)
	{
	  gcc_assert (single_pred_p (gimple_bb (stmt)));
	  use = PHI_ARG_DEF (stmt, 0);
	  stmt = SSA_NAME_DEF_STMT (use);
	}

      base_names_in_chain_on (loop, use, var);
    }
}

/* Returns true if CHAIN is suitable to be combined.  */

static bool
chain_can_be_combined_p (chain_p chain)
{
  return (!chain->combined
	  && (chain->type == CT_LOAD || chain->type == CT_COMBINATION));
}

/* Returns the modify statement that uses NAME.  Skips over assignment
   statements, NAME is replaced with the actual name used in the returned
   statement.  */

gimple *
pcom_worker::find_use_stmt (tree *name)
{
  gimple *stmt;
  tree rhs, lhs;

  /* Skip over assignments.  */
  while (1)
    {
      stmt = single_nonlooparound_use (*name);
      if (!stmt)
	return NULL;

      if (gimple_code (stmt) != GIMPLE_ASSIGN)
	return NULL;

      lhs = gimple_assign_lhs (stmt);
      if (TREE_CODE (lhs) != SSA_NAME)
	return NULL;

      if (gimple_assign_copy_p (stmt))
	{
	  rhs = gimple_assign_rhs1 (stmt);
	  if (rhs != *name)
	    return NULL;

	  *name = lhs;
	}
      else if (get_gimple_rhs_class (gimple_assign_rhs_code (stmt))
	       == GIMPLE_BINARY_RHS)
	return stmt;
      else
	return NULL;
    }
}

/* Returns true if we may perform reassociation for operation CODE in TYPE.  */

static bool
may_reassociate_p (tree type, enum tree_code code)
{
  if (FLOAT_TYPE_P (type)
      && !flag_unsafe_math_optimizations)
    return false;

  return (commutative_tree_code (code)
	  && associative_tree_code (code));
}

/* If the operation used in STMT is associative and commutative, go through the
   tree of the same operations and returns its root.  Distance to the root
   is stored in DISTANCE.  */

gimple *
pcom_worker::find_associative_operation_root (gimple *stmt, unsigned *distance)
{
  tree lhs;
  gimple *next;
  enum tree_code code = gimple_assign_rhs_code (stmt);
  tree type = TREE_TYPE (gimple_assign_lhs (stmt));
  unsigned dist = 0;

  if (!may_reassociate_p (type, code))
    return NULL;

  while (1)
    {
      lhs = gimple_assign_lhs (stmt);
      gcc_assert (TREE_CODE (lhs) == SSA_NAME);

      next = find_use_stmt (&lhs);
      if (!next
	  || gimple_assign_rhs_code (next) != code)
	break;

      stmt = next;
      dist++;
    }

  if (distance)
    *distance = dist;
  return stmt;
}

/* Returns the common statement in that NAME1 and NAME2 have a use.  If there
   is no such statement, returns NULL_TREE.  In case the operation used on
   NAME1 and NAME2 is associative and commutative, returns the root of the
   tree formed by this operation instead of the statement that uses NAME1 or
   NAME2.  */

gimple *
pcom_worker::find_common_use_stmt (tree *name1, tree *name2)
{
  gimple *stmt1, *stmt2;

  stmt1 = find_use_stmt (name1);
  if (!stmt1)
    return NULL;

  stmt2 = find_use_stmt (name2);
  if (!stmt2)
    return NULL;

  if (stmt1 == stmt2)
    return stmt1;

  stmt1 = find_associative_operation_root (stmt1, NULL);
  if (!stmt1)
    return NULL;
  stmt2 = find_associative_operation_root (stmt2, NULL);
  if (!stmt2)
    return NULL;

  return (stmt1 == stmt2 ? stmt1 : NULL);
}

/* Checks whether R1 and R2 are combined together using CODE, with the result
   in RSLT_TYPE, in order R1 CODE R2 if SWAP is false and in order R2 CODE R1
   if it is true.  If CODE is ERROR_MARK, set these values instead.  */

bool
pcom_worker::combinable_refs_p (dref r1, dref r2,
		   enum tree_code *code, bool *swap, tree *rslt_type)
{
  enum tree_code acode;
  bool aswap;
  tree atype;
  tree name1, name2;
  gimple *stmt;

  name1 = name_for_ref (r1);
  name2 = name_for_ref (r2);
  gcc_assert (name1 != NULL_TREE && name2 != NULL_TREE);

  stmt = find_common_use_stmt (&name1, &name2);

  if (!stmt
      /* A simple post-dominance check - make sure the combination
         is executed under the same condition as the references.  */
      || (gimple_bb (stmt) != gimple_bb (r1->stmt)
	  && gimple_bb (stmt) != gimple_bb (r2->stmt)))
    return false;

  acode = gimple_assign_rhs_code (stmt);
  aswap = (!commutative_tree_code (acode)
	   && gimple_assign_rhs1 (stmt) != name1);
  atype = TREE_TYPE (gimple_assign_lhs (stmt));

  if (*code == ERROR_MARK)
    {
      *code = acode;
      *swap = aswap;
      *rslt_type = atype;
      return true;
    }

  return (*code == acode
	  && *swap == aswap
	  && *rslt_type == atype);
}

/* Remove OP from the operation on rhs of STMT, and replace STMT with
   an assignment of the remaining operand.  */

static void
remove_name_from_operation (gimple *stmt, tree op)
{
  tree other_op;
  gimple_stmt_iterator si;

  gcc_assert (is_gimple_assign (stmt));

  if (gimple_assign_rhs1 (stmt) == op)
    other_op = gimple_assign_rhs2 (stmt);
  else
    other_op = gimple_assign_rhs1 (stmt);

  si = gsi_for_stmt (stmt);
  gimple_assign_set_rhs_from_tree (&si, other_op);

  /* We should not have reallocated STMT.  */
  gcc_assert (gsi_stmt (si) == stmt);

  update_stmt (stmt);
}

/* Reassociates the expression in that NAME1 and NAME2 are used so that they
   are combined in a single statement, and returns this statement.  */

gimple *
pcom_worker::reassociate_to_the_same_stmt (tree name1, tree name2)
{
  gimple *stmt1, *stmt2, *root1, *root2, *s1, *s2;
  gassign *new_stmt, *tmp_stmt;
  tree new_name, tmp_name, var, r1, r2;
  unsigned dist1, dist2;
  enum tree_code code;
  tree type = TREE_TYPE (name1);
  gimple_stmt_iterator bsi;

  stmt1 = find_use_stmt (&name1);
  stmt2 = find_use_stmt (&name2);
  root1 = find_associative_operation_root (stmt1, &dist1);
  root2 = find_associative_operation_root (stmt2, &dist2);
  code = gimple_assign_rhs_code (stmt1);

  gcc_assert (root1 && root2 && root1 == root2
	      && code == gimple_assign_rhs_code (stmt2));

  /* Find the root of the nearest expression in that both NAME1 and NAME2
     are used.  */
  r1 = name1;
  s1 = stmt1;
  r2 = name2;
  s2 = stmt2;

  while (dist1 > dist2)
    {
      s1 = find_use_stmt (&r1);
      r1 = gimple_assign_lhs (s1);
      dist1--;
    }
  while (dist2 > dist1)
    {
      s2 = find_use_stmt (&r2);
      r2 = gimple_assign_lhs (s2);
      dist2--;
    }

  while (s1 != s2)
    {
      s1 = find_use_stmt (&r1);
      r1 = gimple_assign_lhs (s1);
      s2 = find_use_stmt (&r2);
      r2 = gimple_assign_lhs (s2);
    }

  /* Remove NAME1 and NAME2 from the statements in that they are used
     currently.  */
  remove_name_from_operation (stmt1, name1);
  remove_name_from_operation (stmt2, name2);

  /* Insert the new statement combining NAME1 and NAME2 before S1, and
     combine it with the rhs of S1.  */
  var = create_tmp_reg (type, "predreastmp");
  new_name = make_ssa_name (var);
  new_stmt = gimple_build_assign (new_name, code, name1, name2);

  var = create_tmp_reg (type, "predreastmp");
  tmp_name = make_ssa_name (var);

  /* Rhs of S1 may now be either a binary expression with operation
     CODE, or gimple_val (in case that stmt1 == s1 or stmt2 == s1,
     so that name1 or name2 was removed from it).  */
  tmp_stmt = gimple_build_assign (tmp_name, gimple_assign_rhs_code (s1),
				  gimple_assign_rhs1 (s1),
				  gimple_assign_rhs2 (s1));

  bsi = gsi_for_stmt (s1);
  gimple_assign_set_rhs_with_ops (&bsi, code, new_name, tmp_name);
  s1 = gsi_stmt (bsi);
  update_stmt (s1);

  gsi_insert_before (&bsi, new_stmt, GSI_SAME_STMT);
  gsi_insert_before (&bsi, tmp_stmt, GSI_SAME_STMT);

  return new_stmt;
}

/* Returns the statement that combines references R1 and R2.  In case R1
   and R2 are not used in the same statement, but they are used with an
   associative and commutative operation in the same expression, reassociate
   the expression so that they are used in the same statement.  */

gimple *
pcom_worker::stmt_combining_refs (dref r1, dref r2)
{
  gimple *stmt1, *stmt2;
  tree name1 = name_for_ref (r1);
  tree name2 = name_for_ref (r2);

  stmt1 = find_use_stmt (&name1);
  stmt2 = find_use_stmt (&name2);
  if (stmt1 == stmt2)
    return stmt1;

  return reassociate_to_the_same_stmt (name1, name2);
}

/* Tries to combine chains CH1 and CH2 together.  If this succeeds, the
   description of the new chain is returned, otherwise we return NULL.  */

chain_p
pcom_worker::combine_chains (chain_p ch1, chain_p ch2)
{
  dref r1, r2, nw;
  enum tree_code op = ERROR_MARK;
  bool swap = false;
  chain_p new_chain;
  unsigned i;
  tree rslt_type = NULL_TREE;

  if (ch1 == ch2)
    return NULL;
  if (ch1->length != ch2->length)
    return NULL;

  if (ch1->refs.length () != ch2->refs.length ())
    return NULL;

  for (i = 0; (ch1->refs.iterate (i, &r1)
	       && ch2->refs.iterate (i, &r2)); i++)
    {
      if (r1->distance != r2->distance)
	return NULL;

      if (!combinable_refs_p (r1, r2, &op, &swap, &rslt_type))
	return NULL;
    }

  if (swap)
    std::swap (ch1, ch2);

  new_chain = new struct chain (CT_COMBINATION);
  new_chain->op = op;
  new_chain->ch1 = ch1;
  new_chain->ch2 = ch2;
  new_chain->rslt_type = rslt_type;
  new_chain->length = ch1->length;

  for (i = 0; (ch1->refs.iterate (i, &r1)
	       && ch2->refs.iterate (i, &r2)); i++)
    {
      nw = XCNEW (class dref_d);
      nw->stmt = stmt_combining_refs (r1, r2);
      nw->distance = r1->distance;

      new_chain->refs.safe_push (nw);
    }

  ch1->combined = true;
  ch2->combined = true;
  return new_chain;
}

/* Recursively update position information of all offspring chains to ROOT
   chain's position information.  */

static void
update_pos_for_combined_chains (chain_p root)
{
  chain_p ch1 = root->ch1, ch2 = root->ch2;
  dref ref, ref1, ref2;
  for (unsigned j = 0; (root->refs.iterate (j, &ref)
			&& ch1->refs.iterate (j, &ref1)
			&& ch2->refs.iterate (j, &ref2)); ++j)
    ref1->pos = ref2->pos = ref->pos;

  if (ch1->type == CT_COMBINATION)
    update_pos_for_combined_chains (ch1);
  if (ch2->type == CT_COMBINATION)
    update_pos_for_combined_chains (ch2);
}

/* Returns true if statement S1 dominates statement S2.  */

static bool
pcom_stmt_dominates_stmt_p (gimple *s1, gimple *s2)
{
  basic_block bb1 = gimple_bb (s1), bb2 = gimple_bb (s2);

  if (!bb1 || s1 == s2)
    return true;

  if (bb1 == bb2)
    return gimple_uid (s1) < gimple_uid (s2);

  return dominated_by_p (CDI_DOMINATORS, bb2, bb1);
}

/* Try to combine the chains.  */

void
pcom_worker::try_combine_chains ()
{
  unsigned i, j;
  chain_p ch1, ch2, cch;
  auto_vec<chain_p> worklist;
  bool combined_p = false;

  FOR_EACH_VEC_ELT (m_chains, i, ch1)
    if (chain_can_be_combined_p (ch1))
      worklist.safe_push (ch1);

  while (!worklist.is_empty ())
    {
      ch1 = worklist.pop ();
      if (!chain_can_be_combined_p (ch1))
	continue;

      FOR_EACH_VEC_ELT (m_chains, j, ch2)
	{
	  if (!chain_can_be_combined_p (ch2))
	    continue;

	  cch = combine_chains (ch1, ch2);
	  if (cch)
	    {
	      worklist.safe_push (cch);
	      m_chains.safe_push (cch);
	      combined_p = true;
	      break;
	    }
	}
    }
  if (!combined_p)
    return;

  /* Setup UID for all statements in dominance order.  */
  basic_block *bbs = get_loop_body_in_dom_order (m_loop);
  renumber_gimple_stmt_uids_in_blocks (bbs, m_loop->num_nodes);
  free (bbs);

  /* Re-association in combined chains may generate statements different to
     order of references of the original chain.  We need to keep references
     of combined chain in dominance order so that all uses will be inserted
     after definitions.  Note:
       A) This is necessary for all combined chains.
       B) This is only necessary for ZERO distance references because other
	  references inherit value from loop carried PHIs.

     We first update position information for all combined chains.  */
  dref ref;
  for (i = 0; m_chains.iterate (i, &ch1); ++i)
    {
      if (ch1->type != CT_COMBINATION || ch1->combined)
	continue;

      for (j = 0; ch1->refs.iterate (j, &ref); ++j)
	ref->pos = gimple_uid (ref->stmt);

      update_pos_for_combined_chains (ch1);
    }
  /* Then sort references according to newly updated position information.  */
  for (i = 0; m_chains.iterate (i, &ch1); ++i)
    {
      if (ch1->type != CT_COMBINATION && !ch1->combined)
	continue;

      /* Find the first reference with non-ZERO distance.  */
      if (ch1->length == 0)
	j = ch1->refs.length();
      else
	{
	  for (j = 0; ch1->refs.iterate (j, &ref); ++j)
	    if (ref->distance != 0)
	      break;
	}

      /* Sort all ZERO distance references by position.  */
      qsort (&ch1->refs[0], j, sizeof (ch1->refs[0]), order_drefs_by_pos);

      if (ch1->combined)
	continue;

      /* For ZERO length chain, has_max_use_after must be true since root
	 combined stmt must dominates others.  */
      if (ch1->length == 0)
	{
	  ch1->has_max_use_after = true;
	  continue;
	}
      /* Check if there is use at max distance after root for combined chains
	 and set flag accordingly.  */
      ch1->has_max_use_after = false;
      gimple *root_stmt = get_chain_root (ch1)->stmt;
      for (j = 1; ch1->refs.iterate (j, &ref); ++j)
	{
	  if (ref->distance == ch1->length
	      && !pcom_stmt_dominates_stmt_p (ref->stmt, root_stmt))
	    {
	      ch1->has_max_use_after = true;
	      break;
	    }
	}
    }
}

/* Prepare initializers for store elimination CHAIN in LOOP.  Returns false
   if this is impossible because one of these initializers may trap, true
   otherwise.  */

static bool
prepare_initializers_chain_store_elim (class loop *loop, chain_p chain)
{
  unsigned i, n = chain->length;

  /* For now we can't eliminate stores if some of them are conditional
     executed.  */
  if (!chain->all_always_accessed)
    return false;

  /* Nothing to intialize for intra-iteration store elimination.  */
  if (n == 0 && chain->type == CT_STORE_STORE)
    return true;

  /* For store elimination chain, there is nothing to initialize if stores
     to be eliminated only store loop invariant values into memory.  */
  if (chain->type == CT_STORE_STORE
      && is_inv_store_elimination_chain (loop, chain))
    {
      chain->inv_store_elimination = true;
      return true;
    }

  chain->inits.create (n);
  chain->inits.safe_grow_cleared (n, true);

  /* For store eliminatin chain like below:

     for (i = 0; i < len; i++)
       {
	 a[i] = 1;
	 // a[i + 1] = ...
	 a[i + 2] = 3;
       }

     store to a[i + 1] is missed in loop body, it acts like bubbles.  The
     content of a[i + 1] remain the same if the loop iterates fewer times
     than chain->length.  We need to set up root variables for such stores
     by loading from memory before loop.  Note we only need to load bubble
     elements because loop body is guaranteed to be executed at least once
     after loop's preheader edge.  */
  auto_vec<bool> bubbles;
  bubbles.safe_grow_cleared (n + 1, true);
  for (i = 0; i < chain->refs.length (); i++)
    bubbles[chain->refs[i]->distance] = true;

  struct data_reference *dr = get_chain_root (chain)->ref;
  for (i = 0; i < n; i++)
    {
      if (bubbles[i])
	continue;

      gimple_seq stmts = NULL;

      tree init = ref_at_iteration (dr, (int) 0 - i, &stmts);
      if (stmts)
	gimple_seq_add_seq_without_update (&chain->init_seq, stmts);

      chain->inits[i] = init;
    }

  return true;
}

/* Prepare initializers for CHAIN.  Returns false if this is impossible
   because one of these initializers may trap, true otherwise.  */

bool
pcom_worker::prepare_initializers_chain (chain_p chain)
{
  unsigned i, n = (chain->type == CT_INVARIANT) ? 1 : chain->length;
  struct data_reference *dr = get_chain_root (chain)->ref;
  tree init;
  dref laref;
  edge entry = loop_preheader_edge (m_loop);

  if (chain->type == CT_STORE_STORE)
    return prepare_initializers_chain_store_elim (m_loop, chain);

  /* Find the initializers for the variables, and check that they cannot
     trap.  */
  chain->inits.create (n);
  for (i = 0; i < n; i++)
    chain->inits.quick_push (NULL_TREE);

  /* If we have replaced some looparound phi nodes, use their initializers
     instead of creating our own.  */
  FOR_EACH_VEC_ELT (chain->refs, i, laref)
    {
      if (gimple_code (laref->stmt) != GIMPLE_PHI)
	continue;

      gcc_assert (laref->distance > 0);
      chain->inits[n - laref->distance] 
	= PHI_ARG_DEF_FROM_EDGE (laref->stmt, entry);
    }

  for (i = 0; i < n; i++)
    {
      gimple_seq stmts = NULL;

      if (chain->inits[i] != NULL_TREE)
	continue;

      init = ref_at_iteration (dr, (int) i - n, &stmts);
      if (!chain->all_always_accessed && tree_could_trap_p (init))
	{
	  gimple_seq_discard (stmts);
	  return false;
	}

      if (stmts)
	gimple_seq_add_seq_without_update (&chain->init_seq, stmts);

      chain->inits[i] = init;
    }

  return true;
}

/* Prepare initializers for chains, and free chains that cannot
   be used because the initializers might trap.  */

void
pcom_worker::prepare_initializers ()
{
  chain_p chain;
  unsigned i;

  for (i = 0; i < m_chains.length (); )
    {
      chain = m_chains[i];
      if (prepare_initializers_chain (chain))
	i++;
      else
	{
	  release_chain (chain);
	  m_chains.unordered_remove (i);
	}
    }
}

/* Generates finalizer memory references for CHAIN.  Returns true
   if finalizer code for CHAIN can be generated, otherwise false.  */

bool
pcom_worker::prepare_finalizers_chain (chain_p chain)
{
  unsigned i, n = chain->length;
  struct data_reference *dr = get_chain_root (chain)->ref;
  tree fini, niters = number_of_latch_executions (m_loop);

  /* For now we can't eliminate stores if some of them are conditional
     executed.  */
  if (!chain->all_always_accessed)
    return false;

  chain->finis.create (n);
  for (i = 0; i < n; i++)
    chain->finis.quick_push (NULL_TREE);

  /* We never use looparound phi node for store elimination chains.  */

  /* Find the finalizers for the variables, and check that they cannot
     trap.  */
  for (i = 0; i < n; i++)
    {
      gimple_seq stmts = NULL;
      gcc_assert (chain->finis[i] == NULL_TREE);

      if (TREE_CODE (niters) != INTEGER_CST && TREE_CODE (niters) != SSA_NAME)
	{
	  niters = unshare_expr (niters);
	  niters = force_gimple_operand (niters, &stmts, true, NULL);
	  if (stmts)
	    {
	      gimple_seq_add_seq_without_update (&chain->fini_seq, stmts);
	      stmts = NULL;
	    }
	}
      fini = ref_at_iteration (dr, (int) 0 - i, &stmts, niters);
      if (stmts)
	gimple_seq_add_seq_without_update (&chain->fini_seq, stmts);

      chain->finis[i] = fini;
    }

  return true;
}

/* Generates finalizer memory reference for chains.  Returns true if
   finalizer code generation for chains breaks loop closed ssa form.  */

bool
pcom_worker::prepare_finalizers ()
{
  chain_p chain;
  unsigned i;
  bool loop_closed_ssa = false;

  for (i = 0; i < m_chains.length ();)
    {
      chain = m_chains[i];

      /* Finalizer is only necessary for inter-iteration store elimination
	 chains.  */
      if (chain->length == 0 || chain->type != CT_STORE_STORE)
	{
	  i++;
	  continue;
	}

      if (prepare_finalizers_chain (chain))
	{
	  i++;
	  /* Be conservative, assume loop closed ssa form is corrupted
	     by store-store chain.  Though it's not always the case if
	     eliminated stores only store loop invariant values into
	     memory.  */
	  loop_closed_ssa = true;
	}
      else
	{
	  release_chain (chain);
	  m_chains.unordered_remove (i);
	}
    }
  return loop_closed_ssa;
}

/* Insert all initializing gimple stmts into LOOP's entry edge.  */

static void
insert_init_seqs (class loop *loop, vec<chain_p> &chains)
{
  unsigned i;
  edge entry = loop_preheader_edge (loop);

  for (i = 0; i < chains.length (); ++i)
    if (chains[i]->init_seq)
      {
	gsi_insert_seq_on_edge_immediate (entry, chains[i]->init_seq);
	chains[i]->init_seq = NULL;
      }
}

/* Performs predictive commoning for LOOP.  Sets bit 1<<1 of return value
   if LOOP was unrolled; Sets bit 1<<2 of return value if loop closed ssa
   form was corrupted.  Non-zero return value indicates some changes were
   applied to this loop.  */

unsigned
pcom_worker::tree_predictive_commoning_loop (bool allow_unroll_p)
{
  struct component *components;
  unsigned unroll_factor = 0;
  class tree_niter_desc desc;
  bool unroll = false, loop_closed_ssa = false;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Processing loop %d\n", m_loop->num);

  /* Nothing for predicitive commoning if loop only iterates 1 time.  */
  if (get_max_loop_iterations_int (m_loop) == 0)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Loop iterates only 1 time, nothing to do.\n");

      return 0;
    }

  /* Find the data references and split them into components according to their
     dependence relations.  */
  auto_vec<loop_p, 3> loop_nest;
  if (!compute_data_dependences_for_loop (m_loop, true, &loop_nest, &m_datarefs,
					  &m_dependences))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Cannot analyze data dependencies\n");
      return 0;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_data_dependence_relations (dump_file, m_dependences);

  components = split_data_refs_to_components ();

  loop_nest.release ();
  if (!components)
    return 0;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Initial state:\n\n");
      dump_components (dump_file, components);
    }

  /* Find the suitable components and split them into chains.  */
  components = filter_suitable_components (components);

  auto_bitmap tmp_vars;
  determine_roots (components);
  release_components (components);

  if (!m_chains.exists ())
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Predictive commoning failed: no suitable chains\n");
      return 0;
    }

  prepare_initializers ();
  loop_closed_ssa = prepare_finalizers ();

  /* Try to combine the chains that are always worked with together.  */
  try_combine_chains ();

  insert_init_seqs (m_loop, m_chains);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Before commoning:\n\n");
      dump_chains (dump_file, m_chains);
    }

  if (allow_unroll_p)
    /* Determine the unroll factor, and if the loop should be unrolled, ensure
       that its number of iterations is divisible by the factor.  */
    unroll_factor = determine_unroll_factor (m_chains);

  if (unroll_factor > 1)
    unroll = can_unroll_loop_p (m_loop, unroll_factor, &desc);

  /* Execute the predictive commoning transformations, and possibly unroll the
     loop.  */
  if (unroll)
    {
      struct epcc_data dta;

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Unrolling %u times.\n", unroll_factor);

      dta.tmp_vars = tmp_vars;
      dta.chains = m_chains.to_vec_legacy ();
      dta.worker = this;

      /* Cfg manipulations performed in tree_transform_and_unroll_loop before
	 execute_pred_commoning_cbck is called may cause phi nodes to be
	 reallocated, which is a problem since CHAINS may point to these
	 statements.  To fix this, we store the ssa names defined by the
	 phi nodes here instead of the phi nodes themselves, and restore
	 the phi nodes in execute_pred_commoning_cbck.  A bit hacky.  */
      replace_phis_by_defined_names (m_chains);

      tree_transform_and_unroll_loop (m_loop, unroll_factor, &desc,
				      execute_pred_commoning_cbck, &dta);
      eliminate_temp_copies (m_loop, tmp_vars);
    }
  else
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Executing predictive commoning without unrolling.\n");
      execute_pred_commoning (tmp_vars);
    }

  return (unroll ? 2 : 1) | (loop_closed_ssa ? 4 : 1);
}

/* Runs predictive commoning.  */

unsigned
tree_predictive_commoning (bool allow_unroll_p)
{
  unsigned ret = 0, changed = 0;

  initialize_original_copy_tables ();
  for (auto loop : loops_list (cfun, LI_ONLY_INNERMOST))
    if (optimize_loop_for_speed_p (loop))
      {
	pcom_worker w(loop);
	changed |= w.tree_predictive_commoning_loop (allow_unroll_p);
      }
  free_original_copy_tables ();

  if (changed > 0)
    {
      ret = TODO_update_ssa_only_virtuals;

      /* Some loop(s) got unrolled.  */
      if (changed > 1)
	{
	  scev_reset ();

	  /* Need to fix up loop closed SSA.  */
	  if (changed >= 4)
	    rewrite_into_loop_closed_ssa (NULL, TODO_update_ssa);

	  ret |= TODO_cleanup_cfg;
	}
    }

  return ret;
}

/* Predictive commoning Pass.  */

static unsigned
run_tree_predictive_commoning (struct function *fun, bool allow_unroll_p)
{
  if (number_of_loops (fun) <= 1)
    return 0;

  return tree_predictive_commoning (allow_unroll_p);
}

namespace {

const pass_data pass_data_predcom =
{
  GIMPLE_PASS, /* type */
  "pcom", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  TV_PREDCOM, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_predcom : public gimple_opt_pass
{
public:
  pass_predcom (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_predcom, ctxt)
  {}

  /* opt_pass methods: */
  bool
  gate (function *) final override
  {
    if (flag_predictive_commoning != 0)
      return true;
    /* Loop vectorization enables predictive commoning implicitly
       only if predictive commoning isn't set explicitly, and it
       doesn't allow unrolling.  */
    if (flag_tree_loop_vectorize
	&& !OPTION_SET_P (flag_predictive_commoning))
      return true;

    return false;
  }

  unsigned int
  execute (function *fun) final override
  {
    bool allow_unroll_p = flag_predictive_commoning != 0;
    return run_tree_predictive_commoning (fun, allow_unroll_p);
  }

}; // class pass_predcom

} // anon namespace

gimple_opt_pass *
make_pass_predcom (gcc::context *ctxt)
{
  return new pass_predcom (ctxt);
}
