/* Predictive commoning.
   Copyright (C) 2005-2013 Free Software Foundation, Inc.

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

   TODO -- stores killing other stores can be taken into account, e.g.,
   for (i = 0; i < n; i++)
     {
       a[i] = 1;
       a[i+2] = 2;
     }

   can be replaced with

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

   The interesting part is that this would generalize store motion; still, since
   sm is performed elsewhere, it does not seem that important.

   Predictive commoning can be generalized for arbitrary computations (not
   just memory loads), and also nontrivial transfer functions (e.g., replacing
   i * i with ii_last + 2 * i + 1), to generalize strength reduction.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "tm_p.h"
#include "cfgloop.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "tree-eh.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "gimple-ssa.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "stringpool.h"
#include "tree-ssanames.h"
#include "tree-ssa-loop-ivopts.h"
#include "tree-ssa-loop-manip.h"
#include "tree-ssa-loop-niter.h"
#include "tree-ssa-loop.h"
#include "tree-into-ssa.h"
#include "expr.h"
#include "tree-dfa.h"
#include "tree-ssa.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "tree-chrec.h"
#include "params.h"
#include "gimple-pretty-print.h"
#include "tree-pass.h"
#include "tree-affine.h"
#include "tree-inline.h"

/* The maximum number of iterations between the considered memory
   references.  */

#define MAX_DISTANCE (target_avail_regs < 16 ? 4 : 8)

/* Data references (or phi nodes that carry data reference values across
   loop iterations).  */

typedef struct dref_d
{
  /* The reference itself.  */
  struct data_reference *ref;

  /* The statement in that the reference appears.  */
  gimple stmt;

  /* In case that STMT is a phi node, this field is set to the SSA name
     defined by it in replace_phis_by_defined_names (in order to avoid
     pointing to phi node that got reallocated in the meantime).  */
  tree name_defined_by_phi;

  /* Distance of the reference from the root of the chain (in number of
     iterations of the loop).  */
  unsigned distance;

  /* Number of iterations offset from the first reference in the component.  */
  double_int offset;

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

  /* A combination of two chains.  */
  CT_COMBINATION
};

/* Chains of data references.  */

typedef struct chain
{
  /* Type of the chain.  */
  enum chain_type type;

  /* For combination chains, the operator and the two chains that are
     combined, and the type of the result.  */
  enum tree_code op;
  tree rslt_type;
  struct chain *ch1, *ch2;

  /* The references in the chain.  */
  vec<dref> refs;

  /* The maximum distance of the reference in the chain from the root.  */
  unsigned length;

  /* The variables used to copy the value throughout iterations.  */
  vec<tree> vars;

  /* Initializers for the variables.  */
  vec<tree> inits;

  /* True if there is a use of a variable with the maximal distance
     that comes after the root in the loop.  */
  unsigned has_max_use_after : 1;

  /* True if all the memory references in the chain are always accessed.  */
  unsigned all_always_accessed : 1;

  /* True if this chain was combined together with some other chain.  */
  unsigned combined : 1;
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
  /* The references in the component.  */
  vec<dref> refs;

  /* What we know about the step of the references in the component.  */
  enum ref_step_type comp_step;

  /* Next component in the list.  */
  struct component *next;
};

/* Bitmap of ssa names defined by looparound phi nodes covered by chains.  */

static bitmap looparound_phis;

/* Cache used by tree_to_aff_combination_expand.  */

static struct pointer_map_t *name_expansions;

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
      dump_double_int (file, ref->offset, false);
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

extern void dump_chains (FILE *, vec<chain_p> );
void
dump_chains (FILE *file, vec<chain_p> chains)
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

static void
release_chain (chain_p chain)
{
  dref ref;
  unsigned i;

  if (chain == NULL)
    return;

  FOR_EACH_VEC_ELT (chain->refs, i, ref)
    free (ref);

  chain->refs.release ();
  chain->vars.release ();
  chain->inits.release ();

  free (chain);
}

/* Frees CHAINS.  */

static void
release_chains (vec<chain_p> chains)
{
  unsigned i;
  chain_p chain;

  FOR_EACH_VEC_ELT (chains, i, chain)
    release_chain (chain);
  chains.release ();
}

/* Frees a component COMP.  */

static void
release_component (struct component *comp)
{
  comp->refs.release ();
  free (comp);
}

/* Frees list of components COMPS.  */

static void
release_components (struct component *comps)
{
  struct component *act, *next;

  for (act = comps; act; act = next)
    {
      next = act->next;
      release_component (act);
    }
}

/* Finds a root of tree given by FATHERS containing A, and performs path
   shortening.  */

static unsigned
component_of (unsigned fathers[], unsigned a)
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
merge_comps (unsigned fathers[], unsigned sizes[], unsigned a, unsigned b)
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

static void
aff_combination_dr_offset (struct data_reference *dr, aff_tree *offset)
{
  tree type = TREE_TYPE (DR_OFFSET (dr));
  aff_tree delta;

  tree_to_aff_combination_expand (DR_OFFSET (dr), type, offset,
				  &name_expansions);
  aff_combination_const (&delta, type, tree_to_double_int (DR_INIT (dr)));
  aff_combination_add (offset, &delta);
}

/* Determines number of iterations of the innermost enclosing loop before B
   refers to exactly the same location as A and stores it to OFF.  If A and
   B do not have the same step, they never meet, or anything else fails,
   returns false, otherwise returns true.  Both A and B are assumed to
   satisfy suitable_reference_p.  */

static bool
determine_offset (struct data_reference *a, struct data_reference *b,
		  double_int *off)
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
      *off = double_int_zero;
      return (operand_equal_p (DR_OFFSET (a), DR_OFFSET (b), 0)
	      && operand_equal_p (DR_INIT (a), DR_INIT (b), 0));
    }

  /* Compare the offsets of the addresses, and check whether the difference
     is a multiple of step.  */
  aff_combination_dr_offset (a, &diff);
  aff_combination_dr_offset (b, &baseb);
  aff_combination_scale (&baseb, double_int_minus_one);
  aff_combination_add (&diff, &baseb);

  tree_to_aff_combination_expand (DR_STEP (a), TREE_TYPE (DR_STEP (a)),
				  &step, &name_expansions);
  return aff_combination_constant_multiple_p (&diff, &step, off);
}

/* Returns the last basic block in LOOP for that we are sure that
   it is executed whenever the loop is entered.  */

static basic_block
last_always_executed_block (struct loop *loop)
{
  unsigned i;
  vec<edge> exits = get_loop_exit_edges (loop);
  edge ex;
  basic_block last = loop->latch;

  FOR_EACH_VEC_ELT (exits, i, ex)
    last = nearest_common_dominator (CDI_DOMINATORS, last, ex->src);
  exits.release ();

  return last;
}

/* Splits dependence graph on DATAREFS described by DEPENDS to components.  */

static struct component *
split_data_refs_to_components (struct loop *loop,
			       vec<data_reference_p> datarefs,
			       vec<ddr_p> depends)
{
  unsigned i, n = datarefs.length ();
  unsigned ca, ia, ib, bad;
  unsigned *comp_father = XNEWVEC (unsigned, n + 1);
  unsigned *comp_size = XNEWVEC (unsigned, n + 1);
  struct component **comps;
  struct data_reference *dr, *dra, *drb;
  struct data_dependence_relation *ddr;
  struct component *comp_list = NULL, *comp;
  dref dataref;
  basic_block last_always_executed = last_always_executed_block (loop);

  FOR_EACH_VEC_ELT (datarefs, i, dr)
    {
      if (!DR_REF (dr))
	{
	  /* A fake reference for call or asm_expr that may clobber memory;
	     just fail.  */
	  goto end;
	}
      /* predcom pass isn't prepared to handle calls with data references.  */
      if (is_gimple_call (DR_STMT (dr)))
	goto end;
      dr->aux = (void *) (size_t) i;
      comp_father[i] = i;
      comp_size[i] = 1;
    }

  /* A component reserved for the "bad" data references.  */
  comp_father[n] = n;
  comp_size[n] = 1;

  FOR_EACH_VEC_ELT (datarefs, i, dr)
    {
      enum ref_step_type dummy;

      if (!suitable_reference_p (dr, &dummy))
	{
	  ia = (unsigned) (size_t) dr->aux;
	  merge_comps (comp_father, comp_size, n, ia);
	}
    }

  FOR_EACH_VEC_ELT (depends, i, ddr)
    {
      double_int dummy_off;

      if (DDR_ARE_DEPENDENT (ddr) == chrec_known)
	continue;

      dra = DDR_A (ddr);
      drb = DDR_B (ddr);
      ia = component_of (comp_father, (unsigned) (size_t) dra->aux);
      ib = component_of (comp_father, (unsigned) (size_t) drb->aux);
      if (ia == ib)
	continue;

      bad = component_of (comp_father, n);

      /* If both A and B are reads, we may ignore unsuitable dependences.  */
      if (DR_IS_READ (dra) && DR_IS_READ (drb)
	  && (ia == bad || ib == bad
	      || !determine_offset (dra, drb, &dummy_off)))
	continue;

      merge_comps (comp_father, comp_size, ia, ib);
    }

  comps = XCNEWVEC (struct component *, n);
  bad = component_of (comp_father, n);
  FOR_EACH_VEC_ELT (datarefs, i, dr)
    {
      ia = (unsigned) (size_t) dr->aux;
      ca = component_of (comp_father, ia);
      if (ca == bad)
	continue;

      comp = comps[ca];
      if (!comp)
	{
	  comp = XCNEW (struct component);
	  comp->refs.create (comp_size[ca]);
	  comps[ca] = comp;
	}

      dataref = XCNEW (struct dref_d);
      dataref->ref = dr;
      dataref->stmt = DR_STMT (dr);
      dataref->offset = double_int_zero;
      dataref->distance = 0;

      dataref->always_accessed
	      = dominated_by_p (CDI_DOMINATORS, last_always_executed,
				gimple_bb (dataref->stmt));
      dataref->pos = comp->refs.length ();
      comp->refs.quick_push (dataref);
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
  free (comps);

end:
  free (comp_father);
  free (comp_size);
  return comp_list;
}

/* Returns true if the component COMP satisfies the conditions
   described in 2) at the beginning of this file.  LOOP is the current
   loop.  */

static bool
suitable_component_p (struct loop *loop, struct component *comp)
{
  unsigned i;
  dref a, first;
  basic_block ba, bp = loop->header;
  bool ok, has_write = false;

  FOR_EACH_VEC_ELT (comp->refs, i, a)
    {
      ba = gimple_bb (a->stmt);

      if (!just_once_each_iteration_p (loop, ba))
	return false;

      gcc_assert (dominated_by_p (CDI_DOMINATORS, ba, bp));
      bp = ba;

      if (DR_IS_WRITE (a->ref))
	has_write = true;
    }

  first = comp->refs[0];
  ok = suitable_reference_p (first->ref, &comp->comp_step);
  gcc_assert (ok);
  first->offset = double_int_zero;

  for (i = 1; comp->refs.iterate (i, &a); i++)
    {
      if (!determine_offset (first->ref, a->ref, &a->offset))
	return false;

#ifdef ENABLE_CHECKING
      {
	enum ref_step_type a_step;
	ok = suitable_reference_p (a->ref, &a_step);
	gcc_assert (ok && a_step == comp->comp_step);
      }
#endif
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
   the beginning of this file.  LOOP is the current loop.  */

static struct component *
filter_suitable_components (struct loop *loop, struct component *comps)
{
  struct component **comp, *act;

  for (comp = &comps; *comp; )
    {
      act = *comp;
      if (suitable_component_p (loop, act))
	comp = &act->next;
      else
	{
	  dref ref;
	  unsigned i;

	  *comp = act->next;
	  FOR_EACH_VEC_ELT (act->refs, i, ref)
	    free (ref);
	  release_component (act);
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
  int offcmp = (*da)->offset.scmp ((*db)->offset);

  if (offcmp != 0)
    return offcmp;

  return (*da)->pos - (*db)->pos;
}

/* Returns root of the CHAIN.  */

static inline dref
get_chain_root (chain_p chain)
{
  return chain->refs[0];
}

/* Adds REF to the chain CHAIN.  */

static void
add_ref_to_chain (chain_p chain, dref ref)
{
  dref root = get_chain_root (chain);
  double_int dist;

  gcc_assert (root->offset.sle (ref->offset));
  dist = ref->offset - root->offset;
  if (double_int::from_uhwi (MAX_DISTANCE).ule (dist))
    {
      free (ref);
      return;
    }
  gcc_assert (dist.fits_uhwi ());

  chain->refs.safe_push (ref);

  ref->distance = dist.to_uhwi ();

  if (ref->distance >= chain->length)
    {
      chain->length = ref->distance;
      chain->has_max_use_after = false;
    }

  if (ref->distance == chain->length
      && ref->pos > root->pos)
    chain->has_max_use_after = true;

  chain->all_always_accessed &= ref->always_accessed;
}

/* Returns the chain for invariant component COMP.  */

static chain_p
make_invariant_chain (struct component *comp)
{
  chain_p chain = XCNEW (struct chain);
  unsigned i;
  dref ref;

  chain->type = CT_INVARIANT;

  chain->all_always_accessed = true;

  FOR_EACH_VEC_ELT (comp->refs, i, ref)
    {
      chain->refs.safe_push (ref);
      chain->all_always_accessed &= ref->always_accessed;
    }

  return chain;
}

/* Make a new chain rooted at REF.  */

static chain_p
make_rooted_chain (dref ref)
{
  chain_p chain = XCNEW (struct chain);

  chain->type = DR_IS_READ (ref->ref) ? CT_LOAD : CT_STORE_LOAD;

  chain->refs.safe_push (ref);
  chain->all_always_accessed = ref->always_accessed;

  ref->distance = 0;

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

static bool
valid_initializer_p (struct data_reference *ref,
		     unsigned distance, struct data_reference *root)
{
  aff_tree diff, base, step;
  double_int off;

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
  aff_combination_scale (&base, double_int_minus_one);
  aff_combination_add (&diff, &base);

  tree_to_aff_combination_expand (DR_STEP (root), TREE_TYPE (DR_STEP (root)),
				  &step, &name_expansions);
  if (!aff_combination_constant_multiple_p (&diff, &step, &off))
    return false;

  if (off != double_int::from_uhwi (distance))
    return false;

  return true;
}

/* Finds looparound phi node of LOOP that copies the value of REF, and if its
   initial value is correct (equal to initial value of REF shifted by one
   iteration), returns the phi node.  Otherwise, NULL_TREE is returned.  ROOT
   is the root of the current chain.  */

static gimple
find_looparound_phi (struct loop *loop, dref ref, dref root)
{
  tree name, init, init_ref;
  gimple phi = NULL, init_stmt;
  edge latch = loop_latch_edge (loop);
  struct data_reference init_dr;
  gimple_stmt_iterator psi;

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

  for (psi = gsi_start_phis (loop->header); !gsi_end_p (psi); gsi_next (&psi))
    {
      phi = gsi_stmt (psi);
      if (PHI_ARG_DEF_FROM_EDGE (phi, latch) == name)
	break;
    }

  if (gsi_end_p (psi))
    return NULL;

  init = PHI_ARG_DEF_FROM_EDGE (phi, loop_preheader_edge (loop));
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
  if (!dr_analyze_innermost (&init_dr, loop))
    return NULL;

  if (!valid_initializer_p (&init_dr, ref->distance + 1, root->ref))
    return NULL;

  return phi;
}

/* Adds a reference for the looparound copy of REF in PHI to CHAIN.  */

static void
insert_looparound_copy (chain_p chain, dref ref, gimple phi)
{
  dref nw = XCNEW (struct dref_d), aref;
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

/* For references in CHAIN that are copied around the LOOP (created previously
   by PRE, or by user), add the results of such copies to the chain.  This
   enables us to remove the copies by unrolling, and may need less registers
   (also, it may allow us to combine chains together).  */

static void
add_looparound_copies (struct loop *loop, chain_p chain)
{
  unsigned i;
  dref ref, root = get_chain_root (chain);
  gimple phi;

  FOR_EACH_VEC_ELT (chain->refs, i, ref)
    {
      phi = find_looparound_phi (loop, ref, root);
      if (!phi)
	continue;

      bitmap_set_bit (looparound_phis, SSA_NAME_VERSION (PHI_RESULT (phi)));
      insert_looparound_copy (chain, ref, phi);
    }
}

/* Find roots of the values and determine distances in the component COMP.
   The references are redistributed into CHAINS.  LOOP is the current
   loop.  */

static void
determine_roots_comp (struct loop *loop,
		      struct component *comp,
		      vec<chain_p> *chains)
{
  unsigned i;
  dref a;
  chain_p chain = NULL;
  double_int last_ofs = double_int_zero;

  /* Invariants are handled specially.  */
  if (comp->comp_step == RS_INVARIANT)
    {
      chain = make_invariant_chain (comp);
      chains->safe_push (chain);
      return;
    }

  comp->refs.qsort (order_drefs);

  FOR_EACH_VEC_ELT (comp->refs, i, a)
    {
      if (!chain || DR_IS_WRITE (a->ref)
	  || double_int::from_uhwi (MAX_DISTANCE).ule (a->offset - last_ofs))
	{
	  if (nontrivial_chain_p (chain))
	    {
	      add_looparound_copies (loop, chain);
	      chains->safe_push (chain);
	    }
	  else
	    release_chain (chain);
	  chain = make_rooted_chain (a);
	  last_ofs = a->offset;
	  continue;
	}

      add_ref_to_chain (chain, a);
    }

  if (nontrivial_chain_p (chain))
    {
      add_looparound_copies (loop, chain);
      chains->safe_push (chain);
    }
  else
    release_chain (chain);
}

/* Find roots of the values and determine distances in components COMPS, and
   separates the references to CHAINS.  LOOP is the current loop.  */

static void
determine_roots (struct loop *loop,
		 struct component *comps, vec<chain_p> *chains)
{
  struct component *comp;

  for (comp = comps; comp; comp = comp->next)
    determine_roots_comp (loop, comp, chains);
}

/* Replace the reference in statement STMT with temporary variable
   NEW_TREE.  If SET is true, NEW_TREE is instead initialized to the value of
   the reference in the statement.  IN_LHS is true if the reference
   is in the lhs of STMT, false if it is in rhs.  */

static void
replace_ref_with (gimple stmt, tree new_tree, bool set, bool in_lhs)
{
  tree val;
  gimple new_stmt;
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

/* Returns a memory reference to DR in the ITER-th iteration of
   the loop it was analyzed in.  Append init stmts to STMTS.  */

static tree 
ref_at_iteration (data_reference_p dr, int iter, gimple_seq *stmts)
{
  tree off = DR_OFFSET (dr);
  tree coff = DR_INIT (dr);
  if (iter == 0)
    ;
  else if (TREE_CODE (DR_STEP (dr)) == INTEGER_CST)
    coff = size_binop (PLUS_EXPR, coff,
		       size_binop (MULT_EXPR, DR_STEP (dr), ssize_int (iter)));
  else
    off = size_binop (PLUS_EXPR, off,
		      size_binop (MULT_EXPR, DR_STEP (dr), ssize_int (iter)));
  tree addr = fold_build_pointer_plus (DR_BASE_ADDRESS (dr), off);
  addr = force_gimple_operand_1 (addr, stmts, is_gimple_mem_ref_addr,
				 NULL_TREE);
  tree alias_ptr = fold_convert (reference_alias_ptr_type (DR_REF (dr)), coff);
  /* While data-ref analysis punts on bit offsets it still handles
     bitfield accesses at byte boundaries.  Cope with that.  Note that
     we cannot simply re-apply the outer COMPONENT_REF because the
     byte-granular portion of it is already applied via DR_INIT and
     DR_OFFSET, so simply build a BIT_FIELD_REF knowing that the bits
     start at offset zero.  */
  if (TREE_CODE (DR_REF (dr)) == COMPONENT_REF
      && DECL_BIT_FIELD (TREE_OPERAND (DR_REF (dr), 1)))
    {
      tree field = TREE_OPERAND (DR_REF (dr), 1);
      return build3 (BIT_FIELD_REF, TREE_TYPE (DR_REF (dr)),
		     build2 (MEM_REF, DECL_BIT_FIELD_TYPE (field),
			     addr, alias_ptr),
		     DECL_SIZE (field), bitsize_zero_node);
    }
  else
    return fold_build2 (MEM_REF, TREE_TYPE (DR_REF (dr)), addr, alias_ptr);
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
initialize_root_vars (struct loop *loop, chain_p chain, bitmap tmp_vars)
{
  unsigned i;
  unsigned n = chain->length;
  dref root = get_chain_root (chain);
  bool reuse_first = !chain->has_max_use_after;
  tree ref, init, var, next;
  gimple phi;
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
    chain->vars[i] = make_ssa_name (var, NULL);

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

/* Create the variables and initialization statement for root of chain
   CHAIN.  Uids of the newly created temporary variables are marked
   in TMP_VARS.  */

static void
initialize_root (struct loop *loop, chain_p chain, bitmap tmp_vars)
{
  dref root = get_chain_root (chain);
  bool in_lhs = (chain->type == CT_STORE_LOAD
		 || chain->type == CT_COMBINATION);

  initialize_root_vars (loop, chain, tmp_vars);
  replace_ref_with (root->stmt,
		    chain->vars[chain->length],
		    true, in_lhs);
}

/* Initializes a variable for load motion for ROOT and prepares phi nodes and
   initialization on entry to LOOP if necessary.  The ssa name for the variable
   is stored in VARS.  If WRITTEN is true, also a phi node to copy its value
   around the loop is created.  Uid of the newly created temporary variable
   is marked in TMP_VARS.  INITS is the list containing the (single)
   initializer.  */

static void
initialize_root_vars_lm (struct loop *loop, dref root, bool written,
			 vec<tree> *vars, vec<tree> inits,
			 bitmap tmp_vars)
{
  unsigned i;
  tree ref = DR_REF (root->ref), init, var, next;
  gimple_seq stmts;
  gimple phi;
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
    (*vars)[i] = make_ssa_name (var, NULL);

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
      gimple init_stmt = gimple_build_assign (var, init);
      gsi_insert_on_edge_immediate (entry, init_stmt);
    }
}


/* Execute load motion for references in chain CHAIN.  Uids of the newly
   created temporary variables are marked in TMP_VARS.  */

static void
execute_load_motion (struct loop *loop, chain_p chain, bitmap tmp_vars)
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
	      var = make_ssa_name (SSA_NAME_VAR (var), NULL);
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

static gimple
single_nonlooparound_use (tree name)
{
  use_operand_p use;
  imm_use_iterator it;
  gimple stmt, ret = NULL;

  FOR_EACH_IMM_USE_FAST (use, it, name)
    {
      stmt = USE_STMT (use);

      if (gimple_code (stmt) == GIMPLE_PHI)
	{
	  /* Ignore uses in looparound phi nodes.  Uses in other phi nodes
	     could not be processed anyway, so just fail for them.  */
	  if (bitmap_bit_p (looparound_phis,
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

static void
remove_stmt (gimple stmt)
{
  tree name;
  gimple next;
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
      gcc_assert (TREE_CODE (name) == SSA_NAME);

      next = single_nonlooparound_use (name);
      reset_debug_uses (stmt);

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

static void
execute_pred_commoning_chain (struct loop *loop, chain_p chain,
			     bitmap tmp_vars)
{
  unsigned i;
  dref a;
  tree var;

  if (chain->combined)
    {
      /* For combined chains, just remove the statements that are used to
	 compute the values of the expression (except for the root one).  */
      for (i = 1; chain->refs.iterate (i, &a); i++)
	remove_stmt (a->stmt);
    }
  else
    {
      /* For non-combined chains, set up the variables that hold its value,
	 and replace the uses of the original references by these
	 variables.  */
      initialize_root (loop, chain, tmp_vars);
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
determine_unroll_factor (vec<chain_p> chains)
{
  chain_p chain;
  unsigned factor = 1, af, nfactor, i;
  unsigned max = PARAM_VALUE (PARAM_MAX_UNROLL_TIMES);

  FOR_EACH_VEC_ELT (chains, i, chain)
    {
      if (chain->type == CT_INVARIANT || chain->combined)
	continue;

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

/* Perform the predictive commoning optimization for CHAINS.
   Uids of the newly created temporary variables are marked in TMP_VARS.  */

static void
execute_pred_commoning (struct loop *loop, vec<chain_p> chains,
			bitmap tmp_vars)
{
  chain_p chain;
  unsigned i;

  FOR_EACH_VEC_ELT (chains, i, chain)
    {
      if (chain->type == CT_INVARIANT)
	execute_load_motion (loop, chain, tmp_vars);
      else
	execute_pred_commoning_chain (loop, chain, tmp_vars);
    }

  update_ssa (TODO_update_ssa_only_virtuals);
}

/* For each reference in CHAINS, if its defining statement is
   phi node, record the ssa name that is defined by it.  */

static void
replace_phis_by_defined_names (vec<chain_p> chains)
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
};

static void
execute_pred_commoning_cbck (struct loop *loop, void *data)
{
  struct epcc_data *const dta = (struct epcc_data *) data;

  /* Restore phi nodes that were replaced by ssa names before
     tree_transform_and_unroll_loop (see detailed description in
     tree_predictive_commoning_loop).  */
  replace_names_by_phis (dta->chains);
  execute_pred_commoning (loop, dta->chains, dta->tmp_vars);
}

/* Base NAME and all the names in the chain of phi nodes that use it
   on variable VAR.  The phi nodes are recognized by being in the copies of
   the header of the LOOP.  */

static void
base_names_in_chain_on (struct loop *loop, tree name, tree var)
{
  gimple stmt, phi;
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
	      BREAK_FROM_IMM_USE_STMT (iter);
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
eliminate_temp_copies (struct loop *loop, bitmap tmp_vars)
{
  edge e;
  gimple phi, stmt;
  tree name, use, var;
  gimple_stmt_iterator psi;

  e = loop_latch_edge (loop);
  for (psi = gsi_start_phis (loop->header); !gsi_end_p (psi); gsi_next (&psi))
    {
      phi = gsi_stmt (psi);
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

static gimple
find_use_stmt (tree *name)
{
  gimple stmt;
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

static gimple
find_associative_operation_root (gimple stmt, unsigned *distance)
{
  tree lhs;
  gimple next;
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

static gimple
find_common_use_stmt (tree *name1, tree *name2)
{
  gimple stmt1, stmt2;

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

static bool
combinable_refs_p (dref r1, dref r2,
		   enum tree_code *code, bool *swap, tree *rslt_type)
{
  enum tree_code acode;
  bool aswap;
  tree atype;
  tree name1, name2;
  gimple stmt;

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
remove_name_from_operation (gimple stmt, tree op)
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

static gimple
reassociate_to_the_same_stmt (tree name1, tree name2)
{
  gimple stmt1, stmt2, root1, root2, s1, s2;
  gimple new_stmt, tmp_stmt;
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
  new_name = make_ssa_name (var, NULL);
  new_stmt = gimple_build_assign_with_ops (code, new_name, name1, name2);

  var = create_tmp_reg (type, "predreastmp");
  tmp_name = make_ssa_name (var, NULL);

  /* Rhs of S1 may now be either a binary expression with operation
     CODE, or gimple_val (in case that stmt1 == s1 or stmt2 == s1,
     so that name1 or name2 was removed from it).  */
  tmp_stmt = gimple_build_assign_with_ops (gimple_assign_rhs_code (s1),
					   tmp_name,
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

static gimple
stmt_combining_refs (dref r1, dref r2)
{
  gimple stmt1, stmt2;
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

static chain_p
combine_chains (chain_p ch1, chain_p ch2)
{
  dref r1, r2, nw;
  enum tree_code op = ERROR_MARK;
  bool swap = false;
  chain_p new_chain;
  unsigned i;
  gimple root_stmt;
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
    {
      chain_p tmp = ch1;
      ch1 = ch2;
      ch2 = tmp;
    }

  new_chain = XCNEW (struct chain);
  new_chain->type = CT_COMBINATION;
  new_chain->op = op;
  new_chain->ch1 = ch1;
  new_chain->ch2 = ch2;
  new_chain->rslt_type = rslt_type;
  new_chain->length = ch1->length;

  for (i = 0; (ch1->refs.iterate (i, &r1)
	       && ch2->refs.iterate (i, &r2)); i++)
    {
      nw = XCNEW (struct dref_d);
      nw->stmt = stmt_combining_refs (r1, r2);
      nw->distance = r1->distance;

      new_chain->refs.safe_push (nw);
    }

  new_chain->has_max_use_after = false;
  root_stmt = get_chain_root (new_chain)->stmt;
  for (i = 1; new_chain->refs.iterate (i, &nw); i++)
    {
      if (nw->distance == new_chain->length
	  && !stmt_dominates_stmt_p (nw->stmt, root_stmt))
	{
	  new_chain->has_max_use_after = true;
	  break;
	}
    }

  ch1->combined = true;
  ch2->combined = true;
  return new_chain;
}

/* Try to combine the CHAINS.  */

static void
try_combine_chains (vec<chain_p> *chains)
{
  unsigned i, j;
  chain_p ch1, ch2, cch;
  auto_vec<chain_p> worklist;

  FOR_EACH_VEC_ELT (*chains, i, ch1)
    if (chain_can_be_combined_p (ch1))
      worklist.safe_push (ch1);

  while (!worklist.is_empty ())
    {
      ch1 = worklist.pop ();
      if (!chain_can_be_combined_p (ch1))
	continue;

      FOR_EACH_VEC_ELT (*chains, j, ch2)
	{
	  if (!chain_can_be_combined_p (ch2))
	    continue;

	  cch = combine_chains (ch1, ch2);
	  if (cch)
	    {
	      worklist.safe_push (cch);
	      chains->safe_push (cch);
	      break;
	    }
	}
    }
}

/* Prepare initializers for CHAIN in LOOP.  Returns false if this is
   impossible because one of these initializers may trap, true otherwise.  */

static bool
prepare_initializers_chain (struct loop *loop, chain_p chain)
{
  unsigned i, n = (chain->type == CT_INVARIANT) ? 1 : chain->length;
  struct data_reference *dr = get_chain_root (chain)->ref;
  tree init;
  gimple_seq stmts;
  dref laref;
  edge entry = loop_preheader_edge (loop);

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
      if (chain->inits[i] != NULL_TREE)
	continue;

      init = ref_at_iteration (dr, (int) i - n, &stmts);
      if (!chain->all_always_accessed && tree_could_trap_p (init))
	return false;

      if (stmts)
	gsi_insert_seq_on_edge_immediate (entry, stmts);

      chain->inits[i] = init;
    }

  return true;
}

/* Prepare initializers for CHAINS in LOOP, and free chains that cannot
   be used because the initializers might trap.  */

static void
prepare_initializers (struct loop *loop, vec<chain_p> chains)
{
  chain_p chain;
  unsigned i;

  for (i = 0; i < chains.length (); )
    {
      chain = chains[i];
      if (prepare_initializers_chain (loop, chain))
	i++;
      else
	{
	  release_chain (chain);
	  chains.unordered_remove (i);
	}
    }
}

/* Performs predictive commoning for LOOP.  Returns true if LOOP was
   unrolled.  */

static bool
tree_predictive_commoning_loop (struct loop *loop)
{
  vec<data_reference_p> datarefs;
  vec<ddr_p> dependences;
  struct component *components;
  vec<chain_p> chains = vNULL;
  unsigned unroll_factor;
  struct tree_niter_desc desc;
  bool unroll = false;
  edge exit;
  bitmap tmp_vars;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Processing loop %d\n",  loop->num);

  /* Find the data references and split them into components according to their
     dependence relations.  */
  stack_vec<loop_p, 3> loop_nest;
  dependences.create (10);
  datarefs.create (10);
  if (! compute_data_dependences_for_loop (loop, true, &loop_nest, &datarefs,
					   &dependences))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Cannot analyze data dependencies\n");
      free_data_refs (datarefs);
      free_dependence_relations (dependences);
      return false;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_data_dependence_relations (dump_file, dependences);

  components = split_data_refs_to_components (loop, datarefs, dependences);
  loop_nest.release ();
  free_dependence_relations (dependences);
  if (!components)
    {
      free_data_refs (datarefs);
      return false;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Initial state:\n\n");
      dump_components (dump_file, components);
    }

  /* Find the suitable components and split them into chains.  */
  components = filter_suitable_components (loop, components);

  tmp_vars = BITMAP_ALLOC (NULL);
  looparound_phis = BITMAP_ALLOC (NULL);
  determine_roots (loop, components, &chains);
  release_components (components);

  if (!chains.exists ())
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Predictive commoning failed: no suitable chains\n");
      goto end;
    }
  prepare_initializers (loop, chains);

  /* Try to combine the chains that are always worked with together.  */
  try_combine_chains (&chains);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Before commoning:\n\n");
      dump_chains (dump_file, chains);
    }

  /* Determine the unroll factor, and if the loop should be unrolled, ensure
     that its number of iterations is divisible by the factor.  */
  unroll_factor = determine_unroll_factor (chains);
  scev_reset ();
  unroll = (unroll_factor > 1
	    && can_unroll_loop_p (loop, unroll_factor, &desc));
  exit = single_dom_exit (loop);

  /* Execute the predictive commoning transformations, and possibly unroll the
     loop.  */
  if (unroll)
    {
      struct epcc_data dta;

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Unrolling %u times.\n", unroll_factor);

      dta.chains = chains;
      dta.tmp_vars = tmp_vars;

      update_ssa (TODO_update_ssa_only_virtuals);

      /* Cfg manipulations performed in tree_transform_and_unroll_loop before
	 execute_pred_commoning_cbck is called may cause phi nodes to be
	 reallocated, which is a problem since CHAINS may point to these
	 statements.  To fix this, we store the ssa names defined by the
	 phi nodes here instead of the phi nodes themselves, and restore
	 the phi nodes in execute_pred_commoning_cbck.  A bit hacky.  */
      replace_phis_by_defined_names (chains);

      tree_transform_and_unroll_loop (loop, unroll_factor, exit, &desc,
				      execute_pred_commoning_cbck, &dta);
      eliminate_temp_copies (loop, tmp_vars);
    }
  else
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Executing predictive commoning without unrolling.\n");
      execute_pred_commoning (loop, chains, tmp_vars);
    }

end: ;
  release_chains (chains);
  free_data_refs (datarefs);
  BITMAP_FREE (tmp_vars);
  BITMAP_FREE (looparound_phis);

  free_affine_expand_cache (&name_expansions);

  return unroll;
}

/* Runs predictive commoning.  */

unsigned
tree_predictive_commoning (void)
{
  bool unrolled = false;
  struct loop *loop;
  unsigned ret = 0;

  initialize_original_copy_tables ();
  FOR_EACH_LOOP (loop, LI_ONLY_INNERMOST)
    if (optimize_loop_for_speed_p (loop))
      {
	unrolled |= tree_predictive_commoning_loop (loop);
      }

  if (unrolled)
    {
      scev_reset ();
      ret = TODO_cleanup_cfg;
    }
  free_original_copy_tables ();

  return ret;
}

/* Predictive commoning Pass.  */

static unsigned
run_tree_predictive_commoning (void)
{
  if (!current_loops)
    return 0;

  return tree_predictive_commoning ();
}

static bool
gate_tree_predictive_commoning (void)
{
  return flag_predictive_commoning != 0;
}

namespace {

const pass_data pass_data_predcom =
{
  GIMPLE_PASS, /* type */
  "pcom", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_PREDCOM, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa_only_virtuals, /* todo_flags_finish */
};

class pass_predcom : public gimple_opt_pass
{
public:
  pass_predcom (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_predcom, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_tree_predictive_commoning (); }
  unsigned int execute () { return run_tree_predictive_commoning (); }

}; // class pass_predcom

} // anon namespace

gimple_opt_pass *
make_pass_predcom (gcc::context *ctxt)
{
  return new pass_predcom (ctxt);
}


