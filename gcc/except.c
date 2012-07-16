/* Implements exception handling.
   Copyright (C) 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,
   2011, 2012 Free Software Foundation, Inc.
   Contributed by Mike Stump <mrs@cygnus.com>.

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


/* An exception is an event that can be "thrown" from within a
   function.  This event can then be "caught" by the callers of
   the function.

   The representation of exceptions changes several times during
   the compilation process:

   In the beginning, in the front end, we have the GENERIC trees
   TRY_CATCH_EXPR, TRY_FINALLY_EXPR, WITH_CLEANUP_EXPR,
   CLEANUP_POINT_EXPR, CATCH_EXPR, and EH_FILTER_EXPR.

   During initial gimplification (gimplify.c) these are lowered
   to the GIMPLE_TRY, GIMPLE_CATCH, and GIMPLE_EH_FILTER nodes.
   The WITH_CLEANUP_EXPR and CLEANUP_POINT_EXPR nodes are converted
   into GIMPLE_TRY_FINALLY nodes; the others are a more direct 1-1
   conversion.

   During pass_lower_eh (tree-eh.c) we record the nested structure
   of the TRY nodes in EH_REGION nodes in CFUN->EH->REGION_TREE.
   We expand the eh_protect_cleanup_actions langhook into MUST_NOT_THROW
   regions at this time.  We can then flatten the statements within
   the TRY nodes to straight-line code.  Statements that had been within
   TRY nodes that can throw are recorded within CFUN->EH->THROW_STMT_TABLE,
   so that we may remember what action is supposed to be taken if
   a given statement does throw.  During this lowering process,
   we create an EH_LANDING_PAD node for each EH_REGION that has
   some code within the function that needs to be executed if a
   throw does happen.  We also create RESX statements that are
   used to transfer control from an inner EH_REGION to an outer
   EH_REGION.  We also create EH_DISPATCH statements as placeholders
   for a runtime type comparison that should be made in order to
   select the action to perform among different CATCH and EH_FILTER
   regions.

   During pass_lower_eh_dispatch (tree-eh.c), which is run after
   all inlining is complete, we are able to run assign_filter_values,
   which allows us to map the set of types manipulated by all of the
   CATCH and EH_FILTER regions to a set of integers.  This set of integers
   will be how the exception runtime communicates with the code generated
   within the function.  We then expand the GIMPLE_EH_DISPATCH statements
   to a switch or conditional branches that use the argument provided by
   the runtime (__builtin_eh_filter) and the set of integers we computed
   in assign_filter_values.

   During pass_lower_resx (tree-eh.c), which is run near the end
   of optimization, we expand RESX statements.  If the eh region
   that is outer to the RESX statement is a MUST_NOT_THROW, then
   the RESX expands to some form of abort statement.  If the eh
   region that is outer to the RESX statement is within the current
   function, then the RESX expands to a bookkeeping call
   (__builtin_eh_copy_values) and a goto.  Otherwise, the next
   handler for the exception must be within a function somewhere
   up the call chain, so we call back into the exception runtime
   (__builtin_unwind_resume).

   During pass_expand (cfgexpand.c), we generate REG_EH_REGION notes
   that create an rtl to eh_region mapping that corresponds to the
   gimple to eh_region mapping that had been recorded in the
   THROW_STMT_TABLE.

   Then, via finish_eh_generation, we generate the real landing pads
   to which the runtime will actually transfer control.  These new
   landing pads perform whatever bookkeeping is needed by the target
   backend in order to resume execution within the current function.
   Each of these new landing pads falls through into the post_landing_pad
   label which had been used within the CFG up to this point.  All
   exception edges within the CFG are redirected to the new landing pads.
   If the target uses setjmp to implement exceptions, the various extra
   calls into the runtime to register and unregister the current stack
   frame are emitted at this time.

   During pass_convert_to_eh_region_ranges (except.c), we transform
   the REG_EH_REGION notes attached to individual insns into
   non-overlapping ranges of insns bounded by NOTE_INSN_EH_REGION_BEG
   and NOTE_INSN_EH_REGION_END.  Each insn within such ranges has the
   same associated action within the exception region tree, meaning
   that (1) the exception is caught by the same landing pad within the
   current function, (2) the exception is blocked by the runtime with
   a MUST_NOT_THROW region, or (3) the exception is not handled at all
   within the current function.

   Finally, during assembly generation, we call
   output_function_exception_table (except.c) to emit the tables with
   which the exception runtime can determine if a given stack frame
   handles a given exception, and if so what filter value to provide
   to the function when the non-local control transfer is effected.
   If the target uses dwarf2 unwinding to implement exceptions, then
   output_call_frame_info (dwarf2out.c) emits the required unwind data.  */


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tree.h"
#include "flags.h"
#include "function.h"
#include "expr.h"
#include "libfuncs.h"
#include "insn-config.h"
#include "except.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "output.h"
#include "dwarf2asm.h"
#include "dwarf2out.h"
#include "dwarf2.h"
#include "toplev.h"
#include "hashtab.h"
#include "intl.h"
#include "ggc.h"
#include "tm_p.h"
#include "target.h"
#include "common/common-target.h"
#include "langhooks.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "tree-pretty-print.h"
#include "tree-pass.h"
#include "tree-flow.h"
#include "cfgloop.h"

/* Provide defaults for stuff that may not be defined when using
   sjlj exceptions.  */
#ifndef EH_RETURN_DATA_REGNO
#define EH_RETURN_DATA_REGNO(N) INVALID_REGNUM
#endif

static GTY(()) int call_site_base;
static GTY ((param_is (union tree_node)))
  htab_t type_to_runtime_map;

/* Describe the SjLj_Function_Context structure.  */
static GTY(()) tree sjlj_fc_type_node;
static int sjlj_fc_call_site_ofs;
static int sjlj_fc_data_ofs;
static int sjlj_fc_personality_ofs;
static int sjlj_fc_lsda_ofs;
static int sjlj_fc_jbuf_ofs;


struct GTY(()) call_site_record_d
{
  rtx landing_pad;
  int action;
};

static bool get_eh_region_and_lp_from_rtx (const_rtx, eh_region *,
					   eh_landing_pad *);

static int t2r_eq (const void *, const void *);
static hashval_t t2r_hash (const void *);

static int ttypes_filter_eq (const void *, const void *);
static hashval_t ttypes_filter_hash (const void *);
static int ehspec_filter_eq (const void *, const void *);
static hashval_t ehspec_filter_hash (const void *);
static int add_ttypes_entry (htab_t, tree);
static int add_ehspec_entry (htab_t, htab_t, tree);
static void dw2_build_landing_pads (void);

static int action_record_eq (const void *, const void *);
static hashval_t action_record_hash (const void *);
static int add_action_record (htab_t, int, int);
static int collect_one_action_chain (htab_t, eh_region);
static int add_call_site (rtx, int, int);

static void push_uleb128 (VEC (uchar, gc) **, unsigned int);
static void push_sleb128 (VEC (uchar, gc) **, int);
#ifndef HAVE_AS_LEB128
static int dw2_size_of_call_site_table (int);
static int sjlj_size_of_call_site_table (void);
#endif
static void dw2_output_call_site_table (int, int);
static void sjlj_output_call_site_table (void);


void
init_eh (void)
{
  if (! flag_exceptions)
    return;

  type_to_runtime_map = htab_create_ggc (31, t2r_hash, t2r_eq, NULL);

  /* Create the SjLj_Function_Context structure.  This should match
     the definition in unwind-sjlj.c.  */
  if (targetm_common.except_unwind_info (&global_options) == UI_SJLJ)
    {
      tree f_jbuf, f_per, f_lsda, f_prev, f_cs, f_data, tmp;

      sjlj_fc_type_node = lang_hooks.types.make_type (RECORD_TYPE);

      f_prev = build_decl (BUILTINS_LOCATION,
			   FIELD_DECL, get_identifier ("__prev"),
			   build_pointer_type (sjlj_fc_type_node));
      DECL_FIELD_CONTEXT (f_prev) = sjlj_fc_type_node;

      f_cs = build_decl (BUILTINS_LOCATION,
			 FIELD_DECL, get_identifier ("__call_site"),
			 integer_type_node);
      DECL_FIELD_CONTEXT (f_cs) = sjlj_fc_type_node;

      tmp = build_index_type (size_int (4 - 1));
      tmp = build_array_type (lang_hooks.types.type_for_mode
				(targetm.unwind_word_mode (), 1),
			      tmp);
      f_data = build_decl (BUILTINS_LOCATION,
			   FIELD_DECL, get_identifier ("__data"), tmp);
      DECL_FIELD_CONTEXT (f_data) = sjlj_fc_type_node;

      f_per = build_decl (BUILTINS_LOCATION,
			  FIELD_DECL, get_identifier ("__personality"),
			  ptr_type_node);
      DECL_FIELD_CONTEXT (f_per) = sjlj_fc_type_node;

      f_lsda = build_decl (BUILTINS_LOCATION,
			   FIELD_DECL, get_identifier ("__lsda"),
			   ptr_type_node);
      DECL_FIELD_CONTEXT (f_lsda) = sjlj_fc_type_node;

#ifdef DONT_USE_BUILTIN_SETJMP
#ifdef JMP_BUF_SIZE
      tmp = size_int (JMP_BUF_SIZE - 1);
#else
      /* Should be large enough for most systems, if it is not,
	 JMP_BUF_SIZE should be defined with the proper value.  It will
	 also tend to be larger than necessary for most systems, a more
	 optimal port will define JMP_BUF_SIZE.  */
      tmp = size_int (FIRST_PSEUDO_REGISTER + 2 - 1);
#endif
#else
      /* builtin_setjmp takes a pointer to 5 words.  */
      tmp = size_int (5 * BITS_PER_WORD / POINTER_SIZE - 1);
#endif
      tmp = build_index_type (tmp);
      tmp = build_array_type (ptr_type_node, tmp);
      f_jbuf = build_decl (BUILTINS_LOCATION,
			   FIELD_DECL, get_identifier ("__jbuf"), tmp);
#ifdef DONT_USE_BUILTIN_SETJMP
      /* We don't know what the alignment requirements of the
	 runtime's jmp_buf has.  Overestimate.  */
      DECL_ALIGN (f_jbuf) = BIGGEST_ALIGNMENT;
      DECL_USER_ALIGN (f_jbuf) = 1;
#endif
      DECL_FIELD_CONTEXT (f_jbuf) = sjlj_fc_type_node;

      TYPE_FIELDS (sjlj_fc_type_node) = f_prev;
      TREE_CHAIN (f_prev) = f_cs;
      TREE_CHAIN (f_cs) = f_data;
      TREE_CHAIN (f_data) = f_per;
      TREE_CHAIN (f_per) = f_lsda;
      TREE_CHAIN (f_lsda) = f_jbuf;

      layout_type (sjlj_fc_type_node);

      /* Cache the interesting field offsets so that we have
	 easy access from rtl.  */
      sjlj_fc_call_site_ofs
	= (tree_low_cst (DECL_FIELD_OFFSET (f_cs), 1)
	   + tree_low_cst (DECL_FIELD_BIT_OFFSET (f_cs), 1) / BITS_PER_UNIT);
      sjlj_fc_data_ofs
	= (tree_low_cst (DECL_FIELD_OFFSET (f_data), 1)
	   + tree_low_cst (DECL_FIELD_BIT_OFFSET (f_data), 1) / BITS_PER_UNIT);
      sjlj_fc_personality_ofs
	= (tree_low_cst (DECL_FIELD_OFFSET (f_per), 1)
	   + tree_low_cst (DECL_FIELD_BIT_OFFSET (f_per), 1) / BITS_PER_UNIT);
      sjlj_fc_lsda_ofs
	= (tree_low_cst (DECL_FIELD_OFFSET (f_lsda), 1)
	   + tree_low_cst (DECL_FIELD_BIT_OFFSET (f_lsda), 1) / BITS_PER_UNIT);
      sjlj_fc_jbuf_ofs
	= (tree_low_cst (DECL_FIELD_OFFSET (f_jbuf), 1)
	   + tree_low_cst (DECL_FIELD_BIT_OFFSET (f_jbuf), 1) / BITS_PER_UNIT);
    }
}

void
init_eh_for_function (void)
{
  cfun->eh = ggc_alloc_cleared_eh_status ();

  /* Make sure zero'th entries are used.  */
  VEC_safe_push (eh_region, gc, cfun->eh->region_array, NULL);
  VEC_safe_push (eh_landing_pad, gc, cfun->eh->lp_array, NULL);
}

/* Routines to generate the exception tree somewhat directly.
   These are used from tree-eh.c when processing exception related
   nodes during tree optimization.  */

static eh_region
gen_eh_region (enum eh_region_type type, eh_region outer)
{
  eh_region new_eh;

  /* Insert a new blank region as a leaf in the tree.  */
  new_eh = ggc_alloc_cleared_eh_region_d ();
  new_eh->type = type;
  new_eh->outer = outer;
  if (outer)
    {
      new_eh->next_peer = outer->inner;
      outer->inner = new_eh;
    }
  else
    {
      new_eh->next_peer = cfun->eh->region_tree;
      cfun->eh->region_tree = new_eh;
    }

  new_eh->index = VEC_length (eh_region, cfun->eh->region_array);
  VEC_safe_push (eh_region, gc, cfun->eh->region_array, new_eh);

  /* Copy the language's notion of whether to use __cxa_end_cleanup.  */
  if (targetm.arm_eabi_unwinder && lang_hooks.eh_use_cxa_end_cleanup)
    new_eh->use_cxa_end_cleanup = true;

  return new_eh;
}

eh_region
gen_eh_region_cleanup (eh_region outer)
{
  return gen_eh_region (ERT_CLEANUP, outer);
}

eh_region
gen_eh_region_try (eh_region outer)
{
  return gen_eh_region (ERT_TRY, outer);
}

eh_catch
gen_eh_region_catch (eh_region t, tree type_or_list)
{
  eh_catch c, l;
  tree type_list, type_node;

  gcc_assert (t->type == ERT_TRY);

  /* Ensure to always end up with a type list to normalize further
     processing, then register each type against the runtime types map.  */
  type_list = type_or_list;
  if (type_or_list)
    {
      if (TREE_CODE (type_or_list) != TREE_LIST)
	type_list = tree_cons (NULL_TREE, type_or_list, NULL_TREE);

      type_node = type_list;
      for (; type_node; type_node = TREE_CHAIN (type_node))
	add_type_for_runtime (TREE_VALUE (type_node));
    }

  c = ggc_alloc_cleared_eh_catch_d ();
  c->type_list = type_list;
  l = t->u.eh_try.last_catch;
  c->prev_catch = l;
  if (l)
    l->next_catch = c;
  else
    t->u.eh_try.first_catch = c;
  t->u.eh_try.last_catch = c;

  return c;
}

eh_region
gen_eh_region_allowed (eh_region outer, tree allowed)
{
  eh_region region = gen_eh_region (ERT_ALLOWED_EXCEPTIONS, outer);
  region->u.allowed.type_list = allowed;

  for (; allowed ; allowed = TREE_CHAIN (allowed))
    add_type_for_runtime (TREE_VALUE (allowed));

  return region;
}

eh_region
gen_eh_region_must_not_throw (eh_region outer)
{
  return gen_eh_region (ERT_MUST_NOT_THROW, outer);
}

eh_landing_pad
gen_eh_landing_pad (eh_region region)
{
  eh_landing_pad lp = ggc_alloc_cleared_eh_landing_pad_d ();

  lp->next_lp = region->landing_pads;
  lp->region = region;
  lp->index = VEC_length (eh_landing_pad, cfun->eh->lp_array);
  region->landing_pads = lp;

  VEC_safe_push (eh_landing_pad, gc, cfun->eh->lp_array, lp);

  return lp;
}

eh_region
get_eh_region_from_number_fn (struct function *ifun, int i)
{
  return VEC_index (eh_region, ifun->eh->region_array, i);
}

eh_region
get_eh_region_from_number (int i)
{
  return get_eh_region_from_number_fn (cfun, i);
}

eh_landing_pad
get_eh_landing_pad_from_number_fn (struct function *ifun, int i)
{
  return VEC_index (eh_landing_pad, ifun->eh->lp_array, i);
}

eh_landing_pad
get_eh_landing_pad_from_number (int i)
{
  return get_eh_landing_pad_from_number_fn (cfun, i);
}

eh_region
get_eh_region_from_lp_number_fn (struct function *ifun, int i)
{
  if (i < 0)
    return VEC_index (eh_region, ifun->eh->region_array, -i);
  else if (i == 0)
    return NULL;
  else
    {
      eh_landing_pad lp;
      lp = VEC_index (eh_landing_pad, ifun->eh->lp_array, i);
      return lp->region;
    }
}

eh_region
get_eh_region_from_lp_number (int i)
{
  return get_eh_region_from_lp_number_fn (cfun, i);
}

/* Returns true if the current function has exception handling regions.  */

bool
current_function_has_exception_handlers (void)
{
  return cfun->eh->region_tree != NULL;
}

/* A subroutine of duplicate_eh_regions.  Copy the eh_region tree at OLD.
   Root it at OUTER, and apply LP_OFFSET to the lp numbers.  */

struct duplicate_eh_regions_data
{
  duplicate_eh_regions_map label_map;
  void *label_map_data;
  struct pointer_map_t *eh_map;
};

static void
duplicate_eh_regions_1 (struct duplicate_eh_regions_data *data,
			eh_region old_r, eh_region outer)
{
  eh_landing_pad old_lp, new_lp;
  eh_region new_r;
  void **slot;

  new_r = gen_eh_region (old_r->type, outer);
  slot = pointer_map_insert (data->eh_map, (void *)old_r);
  gcc_assert (*slot == NULL);
  *slot = (void *)new_r;

  switch (old_r->type)
    {
    case ERT_CLEANUP:
      break;

    case ERT_TRY:
      {
	eh_catch oc, nc;
	for (oc = old_r->u.eh_try.first_catch; oc ; oc = oc->next_catch)
	  {
	    /* We should be doing all our region duplication before and
	       during inlining, which is before filter lists are created.  */
	    gcc_assert (oc->filter_list == NULL);
	    nc = gen_eh_region_catch (new_r, oc->type_list);
	    nc->label = data->label_map (oc->label, data->label_map_data);
	  }
      }
      break;

    case ERT_ALLOWED_EXCEPTIONS:
      new_r->u.allowed.type_list = old_r->u.allowed.type_list;
      if (old_r->u.allowed.label)
	new_r->u.allowed.label
	    = data->label_map (old_r->u.allowed.label, data->label_map_data);
      else
	new_r->u.allowed.label = NULL_TREE;
      break;

    case ERT_MUST_NOT_THROW:
      new_r->u.must_not_throw = old_r->u.must_not_throw;
      break;
    }

  for (old_lp = old_r->landing_pads; old_lp ; old_lp = old_lp->next_lp)
    {
      /* Don't bother copying unused landing pads.  */
      if (old_lp->post_landing_pad == NULL)
	continue;

      new_lp = gen_eh_landing_pad (new_r);
      slot = pointer_map_insert (data->eh_map, (void *)old_lp);
      gcc_assert (*slot == NULL);
      *slot = (void *)new_lp;

      new_lp->post_landing_pad
	= data->label_map (old_lp->post_landing_pad, data->label_map_data);
      EH_LANDING_PAD_NR (new_lp->post_landing_pad) = new_lp->index;
    }

  /* Make sure to preserve the original use of __cxa_end_cleanup.  */
  new_r->use_cxa_end_cleanup = old_r->use_cxa_end_cleanup;

  for (old_r = old_r->inner; old_r ; old_r = old_r->next_peer)
    duplicate_eh_regions_1 (data, old_r, new_r);
}

/* Duplicate the EH regions from IFUN rooted at COPY_REGION into
   the current function and root the tree below OUTER_REGION.
   The special case of COPY_REGION of NULL means all regions.
   Remap labels using MAP/MAP_DATA callback.  Return a pointer map
   that allows the caller to remap uses of both EH regions and
   EH landing pads.  */

struct pointer_map_t *
duplicate_eh_regions (struct function *ifun,
		      eh_region copy_region, int outer_lp,
		      duplicate_eh_regions_map map, void *map_data)
{
  struct duplicate_eh_regions_data data;
  eh_region outer_region;

#ifdef ENABLE_CHECKING
  verify_eh_tree (ifun);
#endif

  data.label_map = map;
  data.label_map_data = map_data;
  data.eh_map = pointer_map_create ();

  outer_region = get_eh_region_from_lp_number (outer_lp);

  /* Copy all the regions in the subtree.  */
  if (copy_region)
    duplicate_eh_regions_1 (&data, copy_region, outer_region);
  else
    {
      eh_region r;
      for (r = ifun->eh->region_tree; r ; r = r->next_peer)
	duplicate_eh_regions_1 (&data, r, outer_region);
    }

#ifdef ENABLE_CHECKING
  verify_eh_tree (cfun);
#endif

  return data.eh_map;
}

/* Return the region that is outer to both REGION_A and REGION_B in IFUN.  */

eh_region
eh_region_outermost (struct function *ifun, eh_region region_a,
		     eh_region region_b)
{
  sbitmap b_outer;

  gcc_assert (ifun->eh->region_array);
  gcc_assert (ifun->eh->region_tree);

  b_outer = sbitmap_alloc (VEC_length (eh_region, ifun->eh->region_array));
  sbitmap_zero (b_outer);

  do
    {
      SET_BIT (b_outer, region_b->index);
      region_b = region_b->outer;
    }
  while (region_b);

  do
    {
      if (TEST_BIT (b_outer, region_a->index))
	break;
      region_a = region_a->outer;
    }
  while (region_a);

  sbitmap_free (b_outer);
  return region_a;
}

static int
t2r_eq (const void *pentry, const void *pdata)
{
  const_tree const entry = (const_tree) pentry;
  const_tree const data = (const_tree) pdata;

  return TREE_PURPOSE (entry) == data;
}

static hashval_t
t2r_hash (const void *pentry)
{
  const_tree const entry = (const_tree) pentry;
  return TREE_HASH (TREE_PURPOSE (entry));
}

void
add_type_for_runtime (tree type)
{
  tree *slot;

  /* If TYPE is NOP_EXPR, it means that it already is a runtime type.  */
  if (TREE_CODE (type) == NOP_EXPR)
    return;

  slot = (tree *) htab_find_slot_with_hash (type_to_runtime_map, type,
					    TREE_HASH (type), INSERT);
  if (*slot == NULL)
    {
      tree runtime = lang_hooks.eh_runtime_type (type);
      *slot = tree_cons (type, runtime, NULL_TREE);
    }
}

tree
lookup_type_for_runtime (tree type)
{
  tree *slot;

  /* If TYPE is NOP_EXPR, it means that it already is a runtime type.  */
  if (TREE_CODE (type) == NOP_EXPR)
    return type;

  slot = (tree *) htab_find_slot_with_hash (type_to_runtime_map, type,
					    TREE_HASH (type), NO_INSERT);

  /* We should have always inserted the data earlier.  */
  return TREE_VALUE (*slot);
}


/* Represent an entry in @TTypes for either catch actions
   or exception filter actions.  */
struct ttypes_filter {
  tree t;
  int filter;
};

/* Compare ENTRY (a ttypes_filter entry in the hash table) with DATA
   (a tree) for a @TTypes type node we are thinking about adding.  */

static int
ttypes_filter_eq (const void *pentry, const void *pdata)
{
  const struct ttypes_filter *const entry
    = (const struct ttypes_filter *) pentry;
  const_tree const data = (const_tree) pdata;

  return entry->t == data;
}

static hashval_t
ttypes_filter_hash (const void *pentry)
{
  const struct ttypes_filter *entry = (const struct ttypes_filter *) pentry;
  return TREE_HASH (entry->t);
}

/* Compare ENTRY with DATA (both struct ttypes_filter) for a @TTypes
   exception specification list we are thinking about adding.  */
/* ??? Currently we use the type lists in the order given.  Someone
   should put these in some canonical order.  */

static int
ehspec_filter_eq (const void *pentry, const void *pdata)
{
  const struct ttypes_filter *entry = (const struct ttypes_filter *) pentry;
  const struct ttypes_filter *data = (const struct ttypes_filter *) pdata;

  return type_list_equal (entry->t, data->t);
}

/* Hash function for exception specification lists.  */

static hashval_t
ehspec_filter_hash (const void *pentry)
{
  const struct ttypes_filter *entry = (const struct ttypes_filter *) pentry;
  hashval_t h = 0;
  tree list;

  for (list = entry->t; list ; list = TREE_CHAIN (list))
    h = (h << 5) + (h >> 27) + TREE_HASH (TREE_VALUE (list));
  return h;
}

/* Add TYPE (which may be NULL) to cfun->eh->ttype_data, using TYPES_HASH
   to speed up the search.  Return the filter value to be used.  */

static int
add_ttypes_entry (htab_t ttypes_hash, tree type)
{
  struct ttypes_filter **slot, *n;

  slot = (struct ttypes_filter **)
    htab_find_slot_with_hash (ttypes_hash, type, TREE_HASH (type), INSERT);

  if ((n = *slot) == NULL)
    {
      /* Filter value is a 1 based table index.  */

      n = XNEW (struct ttypes_filter);
      n->t = type;
      n->filter = VEC_length (tree, cfun->eh->ttype_data) + 1;
      *slot = n;

      VEC_safe_push (tree, gc, cfun->eh->ttype_data, type);
    }

  return n->filter;
}

/* Add LIST to cfun->eh->ehspec_data, using EHSPEC_HASH and TYPES_HASH
   to speed up the search.  Return the filter value to be used.  */

static int
add_ehspec_entry (htab_t ehspec_hash, htab_t ttypes_hash, tree list)
{
  struct ttypes_filter **slot, *n;
  struct ttypes_filter dummy;

  dummy.t = list;
  slot = (struct ttypes_filter **)
    htab_find_slot (ehspec_hash, &dummy, INSERT);

  if ((n = *slot) == NULL)
    {
      int len;

      if (targetm.arm_eabi_unwinder)
	len = VEC_length (tree, cfun->eh->ehspec_data.arm_eabi);
      else
	len = VEC_length (uchar, cfun->eh->ehspec_data.other);

      /* Filter value is a -1 based byte index into a uleb128 buffer.  */

      n = XNEW (struct ttypes_filter);
      n->t = list;
      n->filter = -(len + 1);
      *slot = n;

      /* Generate a 0 terminated list of filter values.  */
      for (; list ; list = TREE_CHAIN (list))
	{
	  if (targetm.arm_eabi_unwinder)
	    VEC_safe_push (tree, gc, cfun->eh->ehspec_data.arm_eabi,
			   TREE_VALUE (list));
	  else
	    {
	      /* Look up each type in the list and encode its filter
		 value as a uleb128.  */
	      push_uleb128 (&cfun->eh->ehspec_data.other,
			    add_ttypes_entry (ttypes_hash, TREE_VALUE (list)));
	    }
	}
      if (targetm.arm_eabi_unwinder)
	VEC_safe_push (tree, gc, cfun->eh->ehspec_data.arm_eabi, NULL_TREE);
      else
	VEC_safe_push (uchar, gc, cfun->eh->ehspec_data.other, 0);
    }

  return n->filter;
}

/* Generate the action filter values to be used for CATCH and
   ALLOWED_EXCEPTIONS regions.  When using dwarf2 exception regions,
   we use lots of landing pads, and so every type or list can share
   the same filter value, which saves table space.  */

void
assign_filter_values (void)
{
  int i;
  htab_t ttypes, ehspec;
  eh_region r;
  eh_catch c;

  cfun->eh->ttype_data = VEC_alloc (tree, gc, 16);
  if (targetm.arm_eabi_unwinder)
    cfun->eh->ehspec_data.arm_eabi = VEC_alloc (tree, gc, 64);
  else
    cfun->eh->ehspec_data.other = VEC_alloc (uchar, gc, 64);

  ttypes = htab_create (31, ttypes_filter_hash, ttypes_filter_eq, free);
  ehspec = htab_create (31, ehspec_filter_hash, ehspec_filter_eq, free);

  for (i = 1; VEC_iterate (eh_region, cfun->eh->region_array, i, r); ++i)
    {
      if (r == NULL)
	continue;

      switch (r->type)
	{
	case ERT_TRY:
	  for (c = r->u.eh_try.first_catch; c ; c = c->next_catch)
	    {
	      /* Whatever type_list is (NULL or true list), we build a list
		 of filters for the region.  */
	      c->filter_list = NULL_TREE;

	      if (c->type_list != NULL)
		{
		  /* Get a filter value for each of the types caught and store
		     them in the region's dedicated list.  */
		  tree tp_node = c->type_list;

		  for ( ; tp_node; tp_node = TREE_CHAIN (tp_node))
		    {
		      int flt = add_ttypes_entry (ttypes, TREE_VALUE (tp_node));
		      tree flt_node = build_int_cst (integer_type_node, flt);

		      c->filter_list
			= tree_cons (NULL_TREE, flt_node, c->filter_list);
		    }
		}
	      else
		{
		  /* Get a filter value for the NULL list also since it
		     will need an action record anyway.  */
		  int flt = add_ttypes_entry (ttypes, NULL);
		  tree flt_node = build_int_cst (integer_type_node, flt);

		  c->filter_list
		    = tree_cons (NULL_TREE, flt_node, NULL);
		}
	    }
	  break;

	case ERT_ALLOWED_EXCEPTIONS:
	  r->u.allowed.filter
	    = add_ehspec_entry (ehspec, ttypes, r->u.allowed.type_list);
	  break;

	default:
	  break;
	}
    }

  htab_delete (ttypes);
  htab_delete (ehspec);
}

/* Emit SEQ into basic block just before INSN (that is assumed to be
   first instruction of some existing BB and return the newly
   produced block.  */
static basic_block
emit_to_new_bb_before (rtx seq, rtx insn)
{
  rtx last;
  basic_block bb, prev_bb;
  edge e;
  edge_iterator ei;

  /* If there happens to be a fallthru edge (possibly created by cleanup_cfg
     call), we don't want it to go into newly created landing pad or other EH
     construct.  */
  for (ei = ei_start (BLOCK_FOR_INSN (insn)->preds); (e = ei_safe_edge (ei)); )
    if (e->flags & EDGE_FALLTHRU)
      force_nonfallthru (e);
    else
      ei_next (&ei);
  last = emit_insn_before (seq, insn);
  if (BARRIER_P (last))
    last = PREV_INSN (last);
  prev_bb = BLOCK_FOR_INSN (insn)->prev_bb;
  bb = create_basic_block (seq, last, prev_bb);
  update_bb_for_insn (bb);
  bb->flags |= BB_SUPERBLOCK;
  return bb;
}

/* A subroutine of dw2_build_landing_pads, also used for edge splitting
   at the rtl level.  Emit the code required by the target at a landing
   pad for the given region.  */

void
expand_dw2_landing_pad_for_region (eh_region region)
{
#ifdef HAVE_exception_receiver
  if (HAVE_exception_receiver)
    emit_insn (gen_exception_receiver ());
  else
#endif
#ifdef HAVE_nonlocal_goto_receiver
  if (HAVE_nonlocal_goto_receiver)
    emit_insn (gen_nonlocal_goto_receiver ());
  else
#endif
    { /* Nothing */ }

  if (region->exc_ptr_reg)
    emit_move_insn (region->exc_ptr_reg,
		    gen_rtx_REG (ptr_mode, EH_RETURN_DATA_REGNO (0)));
  if (region->filter_reg)
    emit_move_insn (region->filter_reg,
		    gen_rtx_REG (targetm.eh_return_filter_mode (),
				 EH_RETURN_DATA_REGNO (1)));
}

/* Expand the extra code needed at landing pads for dwarf2 unwinding.  */

static void
dw2_build_landing_pads (void)
{
  int i;
  eh_landing_pad lp;
  int e_flags = EDGE_FALLTHRU;

  /* If we're going to partition blocks, we need to be able to add
     new landing pads later, which means that we need to hold on to
     the post-landing-pad block.  Prevent it from being merged away.
     We'll remove this bit after partitioning.  */
  if (flag_reorder_blocks_and_partition)
    e_flags |= EDGE_PRESERVE;

  for (i = 1; VEC_iterate (eh_landing_pad, cfun->eh->lp_array, i, lp); ++i)
    {
      basic_block bb;
      rtx seq;
      edge e;

      if (lp == NULL || lp->post_landing_pad == NULL)
	continue;

      start_sequence ();

      lp->landing_pad = gen_label_rtx ();
      emit_label (lp->landing_pad);
      LABEL_PRESERVE_P (lp->landing_pad) = 1;

      expand_dw2_landing_pad_for_region (lp->region);

      seq = get_insns ();
      end_sequence ();

      bb = emit_to_new_bb_before (seq, label_rtx (lp->post_landing_pad));
      e = make_edge (bb, bb->next_bb, e_flags);
      e->count = bb->count;
      e->probability = REG_BR_PROB_BASE;
      if (current_loops)
	{
	  struct loop *loop = bb->next_bb->loop_father;
	  /* If we created a pre-header block, add the new block to the
	     outer loop, otherwise to the loop itself.  */
	  if (bb->next_bb == loop->header)
	    add_bb_to_loop (bb, loop_outer (loop));
	  else
	    add_bb_to_loop (bb, loop);
	}
    }
}


static VEC (int, heap) *sjlj_lp_call_site_index;

/* Process all active landing pads.  Assign each one a compact dispatch
   index, and a call-site index.  */

static int
sjlj_assign_call_site_values (void)
{
  htab_t ar_hash;
  int i, disp_index;
  eh_landing_pad lp;

  crtl->eh.action_record_data = VEC_alloc (uchar, gc, 64);
  ar_hash = htab_create (31, action_record_hash, action_record_eq, free);

  disp_index = 0;
  call_site_base = 1;
  for (i = 1; VEC_iterate (eh_landing_pad, cfun->eh->lp_array, i, lp); ++i)
    if (lp && lp->post_landing_pad)
      {
	int action, call_site;

	/* First: build the action table.  */
	action = collect_one_action_chain (ar_hash, lp->region);

	/* Next: assign call-site values.  If dwarf2 terms, this would be
	   the region number assigned by convert_to_eh_region_ranges, but
	   handles no-action and must-not-throw differently.  */
	/* Map must-not-throw to otherwise unused call-site index 0.  */
	if (action == -2)
	  call_site = 0;
	/* Map no-action to otherwise unused call-site index -1.  */
	else if (action == -1)
	  call_site = -1;
	/* Otherwise, look it up in the table.  */
	else
	  call_site = add_call_site (GEN_INT (disp_index), action, 0);
	VEC_replace (int, sjlj_lp_call_site_index, i, call_site);

	disp_index++;
      }

  htab_delete (ar_hash);

  return disp_index;
}

/* Emit code to record the current call-site index before every
   insn that can throw.  */

static void
sjlj_mark_call_sites (void)
{
  int last_call_site = -2;
  rtx insn, mem;

  for (insn = get_insns (); insn ; insn = NEXT_INSN (insn))
    {
      eh_landing_pad lp;
      eh_region r;
      bool nothrow;
      int this_call_site;
      rtx before, p;

      /* Reset value tracking at extended basic block boundaries.  */
      if (LABEL_P (insn))
	last_call_site = -2;

      if (! INSN_P (insn))
	continue;

      nothrow = get_eh_region_and_lp_from_rtx (insn, &r, &lp);
      if (nothrow)
	continue;
      if (lp)
	this_call_site = VEC_index (int, sjlj_lp_call_site_index, lp->index);
      else if (r == NULL)
	{
	  /* Calls (and trapping insns) without notes are outside any
	     exception handling region in this function.  Mark them as
	     no action.  */
	  this_call_site = -1;
	}
      else
	{
	  gcc_assert (r->type == ERT_MUST_NOT_THROW);
	  this_call_site = 0;
	}

      if (this_call_site != -1)
	crtl->uses_eh_lsda = 1;

      if (this_call_site == last_call_site)
	continue;

      /* Don't separate a call from it's argument loads.  */
      before = insn;
      if (CALL_P (insn))
	before = find_first_parameter_load (insn, NULL_RTX);

      start_sequence ();
      mem = adjust_address (crtl->eh.sjlj_fc, TYPE_MODE (integer_type_node),
			    sjlj_fc_call_site_ofs);
      emit_move_insn (mem, GEN_INT (this_call_site));
      p = get_insns ();
      end_sequence ();

      emit_insn_before (p, before);
      last_call_site = this_call_site;
    }
}

/* Construct the SjLj_Function_Context.  */

static void
sjlj_emit_function_enter (rtx dispatch_label)
{
  rtx fn_begin, fc, mem, seq;
  bool fn_begin_outside_block;
  rtx personality = get_personality_function (current_function_decl);

  fc = crtl->eh.sjlj_fc;

  start_sequence ();

  /* We're storing this libcall's address into memory instead of
     calling it directly.  Thus, we must call assemble_external_libcall
     here, as we can not depend on emit_library_call to do it for us.  */
  assemble_external_libcall (personality);
  mem = adjust_address (fc, Pmode, sjlj_fc_personality_ofs);
  emit_move_insn (mem, personality);

  mem = adjust_address (fc, Pmode, sjlj_fc_lsda_ofs);
  if (crtl->uses_eh_lsda)
    {
      char buf[20];
      rtx sym;

      ASM_GENERATE_INTERNAL_LABEL (buf, "LLSDA", current_function_funcdef_no);
      sym = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (buf));
      SYMBOL_REF_FLAGS (sym) = SYMBOL_FLAG_LOCAL;
      emit_move_insn (mem, sym);
    }
  else
    emit_move_insn (mem, const0_rtx);

  if (dispatch_label)
    {
#ifdef DONT_USE_BUILTIN_SETJMP
      rtx x, last;
      x = emit_library_call_value (setjmp_libfunc, NULL_RTX, LCT_RETURNS_TWICE,
				   TYPE_MODE (integer_type_node), 1,
				   plus_constant (Pmode, XEXP (fc, 0),
						  sjlj_fc_jbuf_ofs), Pmode);

      emit_cmp_and_jump_insns (x, const0_rtx, NE, 0,
			       TYPE_MODE (integer_type_node), 0,
			       dispatch_label);
      last = get_last_insn ();
      if (JUMP_P (last) && any_condjump_p (last))
	{
	  gcc_assert (!find_reg_note (last, REG_BR_PROB, 0));
	  add_reg_note (last, REG_BR_PROB, GEN_INT (REG_BR_PROB_BASE / 100));
	}
#else
      expand_builtin_setjmp_setup (plus_constant (Pmode, XEXP (fc, 0),
						  sjlj_fc_jbuf_ofs),
				   dispatch_label);
#endif
    }

  emit_library_call (unwind_sjlj_register_libfunc, LCT_NORMAL, VOIDmode,
		     1, XEXP (fc, 0), Pmode);

  seq = get_insns ();
  end_sequence ();

  /* ??? Instead of doing this at the beginning of the function,
     do this in a block that is at loop level 0 and dominates all
     can_throw_internal instructions.  */

  fn_begin_outside_block = true;
  for (fn_begin = get_insns (); ; fn_begin = NEXT_INSN (fn_begin))
    if (NOTE_P (fn_begin))
      {
	if (NOTE_KIND (fn_begin) == NOTE_INSN_FUNCTION_BEG)
	  break;
	else if (NOTE_INSN_BASIC_BLOCK_P (fn_begin))
	  fn_begin_outside_block = false;
      }

  if (fn_begin_outside_block)
    insert_insn_on_edge (seq, single_succ_edge (ENTRY_BLOCK_PTR));
  else
    emit_insn_after (seq, fn_begin);
}

/* Call back from expand_function_end to know where we should put
   the call to unwind_sjlj_unregister_libfunc if needed.  */

void
sjlj_emit_function_exit_after (rtx after)
{
  crtl->eh.sjlj_exit_after = after;
}

static void
sjlj_emit_function_exit (void)
{
  rtx seq, insn;

  start_sequence ();

  emit_library_call (unwind_sjlj_unregister_libfunc, LCT_NORMAL, VOIDmode,
		     1, XEXP (crtl->eh.sjlj_fc, 0), Pmode);

  seq = get_insns ();
  end_sequence ();

  /* ??? Really this can be done in any block at loop level 0 that
     post-dominates all can_throw_internal instructions.  This is
     the last possible moment.  */

  insn = crtl->eh.sjlj_exit_after;
  if (LABEL_P (insn))
    insn = NEXT_INSN (insn);

  emit_insn_after (seq, insn);
}

static void
sjlj_emit_dispatch_table (rtx dispatch_label, int num_dispatch)
{
  enum machine_mode unwind_word_mode = targetm.unwind_word_mode ();
  enum machine_mode filter_mode = targetm.eh_return_filter_mode ();
  eh_landing_pad lp;
  rtx mem, seq, fc, before, exc_ptr_reg, filter_reg;
  rtx first_reachable_label;
  basic_block bb;
  eh_region r;
  edge e;
  int i, disp_index;
  gimple switch_stmt;

  fc = crtl->eh.sjlj_fc;

  start_sequence ();

  emit_label (dispatch_label);

#ifndef DONT_USE_BUILTIN_SETJMP
  expand_builtin_setjmp_receiver (dispatch_label);

  /* The caller of expand_builtin_setjmp_receiver is responsible for
     making sure that the label doesn't vanish.  The only other caller
     is the expander for __builtin_setjmp_receiver, which places this
     label on the nonlocal_goto_label list.  Since we're modeling these
     CFG edges more exactly, we can use the forced_labels list instead.  */
  LABEL_PRESERVE_P (dispatch_label) = 1;
  forced_labels
    = gen_rtx_EXPR_LIST (VOIDmode, dispatch_label, forced_labels);
#endif

  /* Load up exc_ptr and filter values from the function context.  */
  mem = adjust_address (fc, unwind_word_mode, sjlj_fc_data_ofs);
  if (unwind_word_mode != ptr_mode)
    {
#ifdef POINTERS_EXTEND_UNSIGNED
      mem = convert_memory_address (ptr_mode, mem);
#else
      mem = convert_to_mode (ptr_mode, mem, 0);
#endif
    }
  exc_ptr_reg = force_reg (ptr_mode, mem);

  mem = adjust_address (fc, unwind_word_mode,
			sjlj_fc_data_ofs + GET_MODE_SIZE (unwind_word_mode));
  if (unwind_word_mode != filter_mode)
    mem = convert_to_mode (filter_mode, mem, 0);
  filter_reg = force_reg (filter_mode, mem);

  /* Jump to one of the directly reachable regions.  */

  disp_index = 0;
  first_reachable_label = NULL;

  /* If there's exactly one call site in the function, don't bother
     generating a switch statement.  */
  switch_stmt = NULL;
  if (num_dispatch > 1)
    {
      tree disp;

      mem = adjust_address (fc, TYPE_MODE (integer_type_node),
			    sjlj_fc_call_site_ofs);
      disp = make_tree (integer_type_node, mem);

      switch_stmt = gimple_build_switch_nlabels (num_dispatch, disp, NULL);
    }

  for (i = 1; VEC_iterate (eh_landing_pad, cfun->eh->lp_array, i, lp); ++i)
    if (lp && lp->post_landing_pad)
      {
	rtx seq2, label;

	start_sequence ();

	lp->landing_pad = dispatch_label;

	if (num_dispatch > 1)
	  {
	    tree t_label, case_elt, t;

	    t_label = create_artificial_label (UNKNOWN_LOCATION);
	    t = build_int_cst (integer_type_node, disp_index);
	    case_elt = build_case_label (t, NULL, t_label);
	    gimple_switch_set_label (switch_stmt, disp_index, case_elt);

	    label = label_rtx (t_label);
	  }
	else
	  label = gen_label_rtx ();

	if (disp_index == 0)
	  first_reachable_label = label;
	emit_label (label);

	r = lp->region;
	if (r->exc_ptr_reg)
	  emit_move_insn (r->exc_ptr_reg, exc_ptr_reg);
	if (r->filter_reg)
	  emit_move_insn (r->filter_reg, filter_reg);

	seq2 = get_insns ();
	end_sequence ();

	before = label_rtx (lp->post_landing_pad);
	bb = emit_to_new_bb_before (seq2, before);
	e = make_edge (bb, bb->next_bb, EDGE_FALLTHRU);
	e->count = bb->count;
	e->probability = REG_BR_PROB_BASE;
	if (current_loops)
	  {
	    struct loop *loop = bb->next_bb->loop_father;
	    /* If we created a pre-header block, add the new block to the
	       outer loop, otherwise to the loop itself.  */
	    if (bb->next_bb == loop->header)
	      add_bb_to_loop (bb, loop_outer (loop));
	    else
	      add_bb_to_loop (bb, loop);
	    /* ???  For multiple dispatches we will end up with edges
	       from the loop tree root into this loop, making it a
	       multiple-entry loop.  Discard all affected loops.  */
	    if (num_dispatch > 1)
	      {
		for (loop = bb->loop_father;
		     loop_outer (loop); loop = loop_outer (loop))
		  {
		    loop->header = NULL;
		    loop->latch = NULL;
		  }
	      }
	  }

	disp_index++;
      }
  gcc_assert (disp_index == num_dispatch);

  if (num_dispatch > 1)
    {
      expand_case (switch_stmt);
      expand_builtin_trap ();
    }

  seq = get_insns ();
  end_sequence ();

  bb = emit_to_new_bb_before (seq, first_reachable_label);
  if (num_dispatch == 1)
    {
      e = make_edge (bb, bb->next_bb, EDGE_FALLTHRU);
      e->count = bb->count;
      e->probability = REG_BR_PROB_BASE;
      if (current_loops)
	{
	  struct loop *loop = bb->next_bb->loop_father;
	  /* If we created a pre-header block, add the new block to the
	     outer loop, otherwise to the loop itself.  */
	  if (bb->next_bb == loop->header)
	    add_bb_to_loop (bb, loop_outer (loop));
	  else
	    add_bb_to_loop (bb, loop);
	}
    }
  else
    {
      /* We are not wiring up edges here, but as the dispatcher call
         is at function begin simply associate the block with the
	 outermost (non-)loop.  */
      if (current_loops)
	add_bb_to_loop (bb, current_loops->tree_root);
    }
}

static void
sjlj_build_landing_pads (void)
{
  int num_dispatch;

  num_dispatch = VEC_length (eh_landing_pad, cfun->eh->lp_array);
  if (num_dispatch == 0)
    return;
  VEC_safe_grow (int, heap, sjlj_lp_call_site_index, num_dispatch);

  num_dispatch = sjlj_assign_call_site_values ();
  if (num_dispatch > 0)
    {
      rtx dispatch_label = gen_label_rtx ();
      int align = STACK_SLOT_ALIGNMENT (sjlj_fc_type_node,
					TYPE_MODE (sjlj_fc_type_node),
					TYPE_ALIGN (sjlj_fc_type_node));
      crtl->eh.sjlj_fc
	= assign_stack_local (TYPE_MODE (sjlj_fc_type_node),
			      int_size_in_bytes (sjlj_fc_type_node),
			      align);

      sjlj_mark_call_sites ();
      sjlj_emit_function_enter (dispatch_label);
      sjlj_emit_dispatch_table (dispatch_label, num_dispatch);
      sjlj_emit_function_exit ();
    }

  /* If we do not have any landing pads, we may still need to register a
     personality routine and (empty) LSDA to handle must-not-throw regions.  */
  else if (function_needs_eh_personality (cfun) != eh_personality_none)
    {
      int align = STACK_SLOT_ALIGNMENT (sjlj_fc_type_node,
					TYPE_MODE (sjlj_fc_type_node),
					TYPE_ALIGN (sjlj_fc_type_node));
      crtl->eh.sjlj_fc
	= assign_stack_local (TYPE_MODE (sjlj_fc_type_node),
			      int_size_in_bytes (sjlj_fc_type_node),
			      align);

      sjlj_mark_call_sites ();
      sjlj_emit_function_enter (NULL_RTX);
      sjlj_emit_function_exit ();
    }

  VEC_free (int, heap, sjlj_lp_call_site_index);
}

/* After initial rtl generation, call back to finish generating
   exception support code.  */

void
finish_eh_generation (void)
{
  basic_block bb;

  /* Construct the landing pads.  */
  if (targetm_common.except_unwind_info (&global_options) == UI_SJLJ)
    sjlj_build_landing_pads ();
  else
    dw2_build_landing_pads ();
  break_superblocks ();

  if (targetm_common.except_unwind_info (&global_options) == UI_SJLJ
      /* Kludge for Alpha (see alpha_gp_save_rtx).  */
      || single_succ_edge (ENTRY_BLOCK_PTR)->insns.r)
    commit_edge_insertions ();

  /* Redirect all EH edges from the post_landing_pad to the landing pad.  */
  FOR_EACH_BB (bb)
    {
      eh_landing_pad lp;
      edge_iterator ei;
      edge e;

      lp = get_eh_landing_pad_from_rtx (BB_END (bb));

      FOR_EACH_EDGE (e, ei, bb->succs)
	if (e->flags & EDGE_EH)
	  break;

      /* We should not have generated any new throwing insns during this
	 pass, and we should not have lost any EH edges, so we only need
	 to handle two cases here:
	 (1) reachable handler and an existing edge to post-landing-pad,
	 (2) no reachable handler and no edge.  */
      gcc_assert ((lp != NULL) == (e != NULL));
      if (lp != NULL)
	{
	  gcc_assert (BB_HEAD (e->dest) == label_rtx (lp->post_landing_pad));

	  redirect_edge_succ (e, BLOCK_FOR_INSN (lp->landing_pad));
	  e->flags |= (CALL_P (BB_END (bb))
		       ? EDGE_ABNORMAL | EDGE_ABNORMAL_CALL
		       : EDGE_ABNORMAL);
	}
    }
}

/* This section handles removing dead code for flow.  */

void
remove_eh_landing_pad (eh_landing_pad lp)
{
  eh_landing_pad *pp;

  for (pp = &lp->region->landing_pads; *pp != lp; pp = &(*pp)->next_lp)
    continue;
  *pp = lp->next_lp;

  if (lp->post_landing_pad)
    EH_LANDING_PAD_NR (lp->post_landing_pad) = 0;
  VEC_replace (eh_landing_pad, cfun->eh->lp_array, lp->index, NULL);
}

/* Splice REGION from the region tree.  */

void
remove_eh_handler (eh_region region)
{
  eh_region *pp, *pp_start, p, outer;
  eh_landing_pad lp;

  for (lp = region->landing_pads; lp ; lp = lp->next_lp)
    {
      if (lp->post_landing_pad)
	EH_LANDING_PAD_NR (lp->post_landing_pad) = 0;
      VEC_replace (eh_landing_pad, cfun->eh->lp_array, lp->index, NULL);
    }

  outer = region->outer;
  if (outer)
    pp_start = &outer->inner;
  else
    pp_start = &cfun->eh->region_tree;
  for (pp = pp_start, p = *pp; p != region; pp = &p->next_peer, p = *pp)
    continue;
  if (region->inner)
    {
      *pp = p = region->inner;
      do
	{
	  p->outer = outer;
	  pp = &p->next_peer;
	  p = *pp;
	}
      while (p);
    }
  *pp = region->next_peer;

  VEC_replace (eh_region, cfun->eh->region_array, region->index, NULL);
}

/* Invokes CALLBACK for every exception handler landing pad label.
   Only used by reload hackery; should not be used by new code.  */

void
for_each_eh_label (void (*callback) (rtx))
{
  eh_landing_pad lp;
  int i;

  for (i = 1; VEC_iterate (eh_landing_pad, cfun->eh->lp_array, i, lp); ++i)
    {
      if (lp)
	{
	  rtx lab = lp->landing_pad;
	  if (lab && LABEL_P (lab))
	    (*callback) (lab);
	}
    }
}

/* Create the REG_EH_REGION note for INSN, given its ECF_FLAGS for a
   call insn.

   At the gimple level, we use LP_NR
       > 0 : The statement transfers to landing pad LP_NR
       = 0 : The statement is outside any EH region
       < 0 : The statement is within MUST_NOT_THROW region -LP_NR.

   At the rtl level, we use LP_NR
       > 0 : The insn transfers to landing pad LP_NR
       = 0 : The insn cannot throw
       < 0 : The insn is within MUST_NOT_THROW region -LP_NR
       = INT_MIN : The insn cannot throw or execute a nonlocal-goto.
       missing note: The insn is outside any EH region.

  ??? This difference probably ought to be avoided.  We could stand
  to record nothrow for arbitrary gimple statements, and so avoid
  some moderately complex lookups in stmt_could_throw_p.  Perhaps
  NOTHROW should be mapped on both sides to INT_MIN.  Perhaps the
  no-nonlocal-goto property should be recorded elsewhere as a bit
  on the call_insn directly.  Perhaps we should make more use of
  attaching the trees to call_insns (reachable via symbol_ref in
  direct call cases) and just pull the data out of the trees.  */

void
make_reg_eh_region_note (rtx insn, int ecf_flags, int lp_nr)
{
  rtx value;
  if (ecf_flags & ECF_NOTHROW)
    value = const0_rtx;
  else if (lp_nr != 0)
    value = GEN_INT (lp_nr);
  else
    return;
  add_reg_note (insn, REG_EH_REGION, value);
}

/* Create a REG_EH_REGION note for a CALL_INSN that cannot throw
   nor perform a non-local goto.  Replace the region note if it
   already exists.  */

void
make_reg_eh_region_note_nothrow_nononlocal (rtx insn)
{
  rtx note = find_reg_note (insn, REG_EH_REGION, NULL_RTX);
  rtx intmin = GEN_INT (INT_MIN);

  if (note != 0)
    XEXP (note, 0) = intmin;
  else
    add_reg_note (insn, REG_EH_REGION, intmin);
}

/* Return true if INSN could throw, assuming no REG_EH_REGION note
   to the contrary.  */

bool
insn_could_throw_p (const_rtx insn)
{
  if (!flag_exceptions)
    return false;
  if (CALL_P (insn))
    return true;
  if (INSN_P (insn) && cfun->can_throw_non_call_exceptions)
    return may_trap_p (PATTERN (insn));
  return false;
}

/* Copy an REG_EH_REGION note to each insn that might throw beginning
   at FIRST and ending at LAST.  NOTE_OR_INSN is either the source insn
   to look for a note, or the note itself.  */

void
copy_reg_eh_region_note_forward (rtx note_or_insn, rtx first, rtx last)
{
  rtx insn, note = note_or_insn;

  if (INSN_P (note_or_insn))
    {
      note = find_reg_note (note_or_insn, REG_EH_REGION, NULL_RTX);
      if (note == NULL)
	return;
    }
  note = XEXP (note, 0);

  for (insn = first; insn != last ; insn = NEXT_INSN (insn))
    if (!find_reg_note (insn, REG_EH_REGION, NULL_RTX)
        && insn_could_throw_p (insn))
      add_reg_note (insn, REG_EH_REGION, note);
}

/* Likewise, but iterate backward.  */

void
copy_reg_eh_region_note_backward (rtx note_or_insn, rtx last, rtx first)
{
  rtx insn, note = note_or_insn;

  if (INSN_P (note_or_insn))
    {
      note = find_reg_note (note_or_insn, REG_EH_REGION, NULL_RTX);
      if (note == NULL)
	return;
    }
  note = XEXP (note, 0);

  for (insn = last; insn != first; insn = PREV_INSN (insn))
    if (insn_could_throw_p (insn))
      add_reg_note (insn, REG_EH_REGION, note);
}


/* Extract all EH information from INSN.  Return true if the insn
   was marked NOTHROW.  */

static bool
get_eh_region_and_lp_from_rtx (const_rtx insn, eh_region *pr,
			       eh_landing_pad *plp)
{
  eh_landing_pad lp = NULL;
  eh_region r = NULL;
  bool ret = false;
  rtx note;
  int lp_nr;

  if (! INSN_P (insn))
    goto egress;

  if (NONJUMP_INSN_P (insn)
      && GET_CODE (PATTERN (insn)) == SEQUENCE)
    insn = XVECEXP (PATTERN (insn), 0, 0);

  note = find_reg_note (insn, REG_EH_REGION, NULL_RTX);
  if (!note)
    {
      ret = !insn_could_throw_p (insn);
      goto egress;
    }

  lp_nr = INTVAL (XEXP (note, 0));
  if (lp_nr == 0 || lp_nr == INT_MIN)
    {
      ret = true;
      goto egress;
    }

  if (lp_nr < 0)
    r = VEC_index (eh_region, cfun->eh->region_array, -lp_nr);
  else
    {
      lp = VEC_index (eh_landing_pad, cfun->eh->lp_array, lp_nr);
      r = lp->region;
    }

 egress:
  *plp = lp;
  *pr = r;
  return ret;
}

/* Return the landing pad to which INSN may go, or NULL if it does not
   have a reachable landing pad within this function.  */

eh_landing_pad
get_eh_landing_pad_from_rtx (const_rtx insn)
{
  eh_landing_pad lp;
  eh_region r;

  get_eh_region_and_lp_from_rtx (insn, &r, &lp);
  return lp;
}

/* Return the region to which INSN may go, or NULL if it does not
   have a reachable region within this function.  */

eh_region
get_eh_region_from_rtx (const_rtx insn)
{
  eh_landing_pad lp;
  eh_region r;

  get_eh_region_and_lp_from_rtx (insn, &r, &lp);
  return r;
}

/* Return true if INSN throws and is caught by something in this function.  */

bool
can_throw_internal (const_rtx insn)
{
  return get_eh_landing_pad_from_rtx (insn) != NULL;
}

/* Return true if INSN throws and escapes from the current function.  */

bool
can_throw_external (const_rtx insn)
{
  eh_landing_pad lp;
  eh_region r;
  bool nothrow;

  if (! INSN_P (insn))
    return false;

  if (NONJUMP_INSN_P (insn)
      && GET_CODE (PATTERN (insn)) == SEQUENCE)
    {
      rtx seq = PATTERN (insn);
      int i, n = XVECLEN (seq, 0);

      for (i = 0; i < n; i++)
	if (can_throw_external (XVECEXP (seq, 0, i)))
	  return true;

      return false;
    }

  nothrow = get_eh_region_and_lp_from_rtx (insn, &r, &lp);

  /* If we can't throw, we obviously can't throw external.  */
  if (nothrow)
    return false;

  /* If we have an internal landing pad, then we're not external.  */
  if (lp != NULL)
    return false;

  /* If we're not within an EH region, then we are external.  */
  if (r == NULL)
    return true;

  /* The only thing that ought to be left is MUST_NOT_THROW regions,
     which don't always have landing pads.  */
  gcc_assert (r->type == ERT_MUST_NOT_THROW);
  return false;
}

/* Return true if INSN cannot throw at all.  */

bool
insn_nothrow_p (const_rtx insn)
{
  eh_landing_pad lp;
  eh_region r;

  if (! INSN_P (insn))
    return true;

  if (NONJUMP_INSN_P (insn)
      && GET_CODE (PATTERN (insn)) == SEQUENCE)
    {
      rtx seq = PATTERN (insn);
      int i, n = XVECLEN (seq, 0);

      for (i = 0; i < n; i++)
	if (!insn_nothrow_p (XVECEXP (seq, 0, i)))
	  return false;

      return true;
    }

  return get_eh_region_and_lp_from_rtx (insn, &r, &lp);
}

/* Return true if INSN can perform a non-local goto.  */
/* ??? This test is here in this file because it (ab)uses REG_EH_REGION.  */

bool
can_nonlocal_goto (const_rtx insn)
{
  if (nonlocal_goto_handler_labels && CALL_P (insn))
    {
      rtx note = find_reg_note (insn, REG_EH_REGION, NULL_RTX);
      if (!note || INTVAL (XEXP (note, 0)) != INT_MIN)
	return true;
    }
  return false;
}

/* Set TREE_NOTHROW and crtl->all_throwers_are_sibcalls.  */

static unsigned int
set_nothrow_function_flags (void)
{
  rtx insn;

  crtl->nothrow = 1;

  /* Assume crtl->all_throwers_are_sibcalls until we encounter
     something that can throw an exception.  We specifically exempt
     CALL_INSNs that are SIBLING_CALL_P, as these are really jumps,
     and can't throw.  Most CALL_INSNs are not SIBLING_CALL_P, so this
     is optimistic.  */

  crtl->all_throwers_are_sibcalls = 1;

  /* If we don't know that this implementation of the function will
     actually be used, then we must not set TREE_NOTHROW, since
     callers must not assume that this function does not throw.  */
  if (TREE_NOTHROW (current_function_decl))
    return 0;

  if (! flag_exceptions)
    return 0;

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    if (can_throw_external (insn))
      {
        crtl->nothrow = 0;

	if (!CALL_P (insn) || !SIBLING_CALL_P (insn))
	  {
	    crtl->all_throwers_are_sibcalls = 0;
	    return 0;
	  }
      }

  for (insn = crtl->epilogue_delay_list; insn;
       insn = XEXP (insn, 1))
    if (can_throw_external (insn))
      {
        crtl->nothrow = 0;

	if (!CALL_P (insn) || !SIBLING_CALL_P (insn))
	  {
	    crtl->all_throwers_are_sibcalls = 0;
	    return 0;
	  }
      }
  if (crtl->nothrow
      && (cgraph_function_body_availability (cgraph_get_node
					     (current_function_decl))
          >= AVAIL_AVAILABLE))
    {
      struct cgraph_node *node = cgraph_get_node (current_function_decl);
      struct cgraph_edge *e;
      for (e = node->callers; e; e = e->next_caller)
        e->can_throw_external = false;
      cgraph_set_nothrow_flag (node, true);

      if (dump_file)
	fprintf (dump_file, "Marking function nothrow: %s\n\n",
		 current_function_name ());
    }
  return 0;
}

struct rtl_opt_pass pass_set_nothrow_function_flags =
{
 {
  RTL_PASS,
  "nothrow",                            /* name */
  NULL,                                 /* gate */
  set_nothrow_function_flags,           /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_NONE,                              /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  0                                     /* todo_flags_finish */
 }
};


/* Various hooks for unwind library.  */

/* Expand the EH support builtin functions:
   __builtin_eh_pointer and __builtin_eh_filter.  */

static eh_region
expand_builtin_eh_common (tree region_nr_t)
{
  HOST_WIDE_INT region_nr;
  eh_region region;

  gcc_assert (host_integerp (region_nr_t, 0));
  region_nr = tree_low_cst (region_nr_t, 0);

  region = VEC_index (eh_region, cfun->eh->region_array, region_nr);

  /* ??? We shouldn't have been able to delete a eh region without
     deleting all the code that depended on it.  */
  gcc_assert (region != NULL);

  return region;
}

/* Expand to the exc_ptr value from the given eh region.  */

rtx
expand_builtin_eh_pointer (tree exp)
{
  eh_region region
    = expand_builtin_eh_common (CALL_EXPR_ARG (exp, 0));
  if (region->exc_ptr_reg == NULL)
    region->exc_ptr_reg = gen_reg_rtx (ptr_mode);
  return region->exc_ptr_reg;
}

/* Expand to the filter value from the given eh region.  */

rtx
expand_builtin_eh_filter (tree exp)
{
  eh_region region
    = expand_builtin_eh_common (CALL_EXPR_ARG (exp, 0));
  if (region->filter_reg == NULL)
    region->filter_reg = gen_reg_rtx (targetm.eh_return_filter_mode ());
  return region->filter_reg;
}

/* Copy the exc_ptr and filter values from one landing pad's registers
   to another.  This is used to inline the resx statement.  */

rtx
expand_builtin_eh_copy_values (tree exp)
{
  eh_region dst
    = expand_builtin_eh_common (CALL_EXPR_ARG (exp, 0));
  eh_region src
    = expand_builtin_eh_common (CALL_EXPR_ARG (exp, 1));
  enum machine_mode fmode = targetm.eh_return_filter_mode ();

  if (dst->exc_ptr_reg == NULL)
    dst->exc_ptr_reg = gen_reg_rtx (ptr_mode);
  if (src->exc_ptr_reg == NULL)
    src->exc_ptr_reg = gen_reg_rtx (ptr_mode);

  if (dst->filter_reg == NULL)
    dst->filter_reg = gen_reg_rtx (fmode);
  if (src->filter_reg == NULL)
    src->filter_reg = gen_reg_rtx (fmode);

  emit_move_insn (dst->exc_ptr_reg, src->exc_ptr_reg);
  emit_move_insn (dst->filter_reg, src->filter_reg);

  return const0_rtx;
}

/* Do any necessary initialization to access arbitrary stack frames.
   On the SPARC, this means flushing the register windows.  */

void
expand_builtin_unwind_init (void)
{
  /* Set this so all the registers get saved in our frame; we need to be
     able to copy the saved values for any registers from frames we unwind.  */
  crtl->saves_all_registers = 1;

#ifdef SETUP_FRAME_ADDRESSES
  SETUP_FRAME_ADDRESSES ();
#endif
}

/* Map a non-negative number to an eh return data register number; expands
   to -1 if no return data register is associated with the input number.
   At least the inputs 0 and 1 must be mapped; the target may provide more.  */

rtx
expand_builtin_eh_return_data_regno (tree exp)
{
  tree which = CALL_EXPR_ARG (exp, 0);
  unsigned HOST_WIDE_INT iwhich;

  if (TREE_CODE (which) != INTEGER_CST)
    {
      error ("argument of %<__builtin_eh_return_regno%> must be constant");
      return constm1_rtx;
    }

  iwhich = tree_low_cst (which, 1);
  iwhich = EH_RETURN_DATA_REGNO (iwhich);
  if (iwhich == INVALID_REGNUM)
    return constm1_rtx;

#ifdef DWARF_FRAME_REGNUM
  iwhich = DWARF_FRAME_REGNUM (iwhich);
#else
  iwhich = DBX_REGISTER_NUMBER (iwhich);
#endif

  return GEN_INT (iwhich);
}

/* Given a value extracted from the return address register or stack slot,
   return the actual address encoded in that value.  */

rtx
expand_builtin_extract_return_addr (tree addr_tree)
{
  rtx addr = expand_expr (addr_tree, NULL_RTX, Pmode, EXPAND_NORMAL);

  if (GET_MODE (addr) != Pmode
      && GET_MODE (addr) != VOIDmode)
    {
#ifdef POINTERS_EXTEND_UNSIGNED
      addr = convert_memory_address (Pmode, addr);
#else
      addr = convert_to_mode (Pmode, addr, 0);
#endif
    }

  /* First mask out any unwanted bits.  */
#ifdef MASK_RETURN_ADDR
  expand_and (Pmode, addr, MASK_RETURN_ADDR, addr);
#endif

  /* Then adjust to find the real return address.  */
#if defined (RETURN_ADDR_OFFSET)
  addr = plus_constant (Pmode, addr, RETURN_ADDR_OFFSET);
#endif

  return addr;
}

/* Given an actual address in addr_tree, do any necessary encoding
   and return the value to be stored in the return address register or
   stack slot so the epilogue will return to that address.  */

rtx
expand_builtin_frob_return_addr (tree addr_tree)
{
  rtx addr = expand_expr (addr_tree, NULL_RTX, ptr_mode, EXPAND_NORMAL);

  addr = convert_memory_address (Pmode, addr);

#ifdef RETURN_ADDR_OFFSET
  addr = force_reg (Pmode, addr);
  addr = plus_constant (Pmode, addr, -RETURN_ADDR_OFFSET);
#endif

  return addr;
}

/* Set up the epilogue with the magic bits we'll need to return to the
   exception handler.  */

void
expand_builtin_eh_return (tree stackadj_tree ATTRIBUTE_UNUSED,
			  tree handler_tree)
{
  rtx tmp;

#ifdef EH_RETURN_STACKADJ_RTX
  tmp = expand_expr (stackadj_tree, crtl->eh.ehr_stackadj,
		     VOIDmode, EXPAND_NORMAL);
  tmp = convert_memory_address (Pmode, tmp);
  if (!crtl->eh.ehr_stackadj)
    crtl->eh.ehr_stackadj = copy_to_reg (tmp);
  else if (tmp != crtl->eh.ehr_stackadj)
    emit_move_insn (crtl->eh.ehr_stackadj, tmp);
#endif

  tmp = expand_expr (handler_tree, crtl->eh.ehr_handler,
		     VOIDmode, EXPAND_NORMAL);
  tmp = convert_memory_address (Pmode, tmp);
  if (!crtl->eh.ehr_handler)
    crtl->eh.ehr_handler = copy_to_reg (tmp);
  else if (tmp != crtl->eh.ehr_handler)
    emit_move_insn (crtl->eh.ehr_handler, tmp);

  if (!crtl->eh.ehr_label)
    crtl->eh.ehr_label = gen_label_rtx ();
  emit_jump (crtl->eh.ehr_label);
}

/* Expand __builtin_eh_return.  This exit path from the function loads up
   the eh return data registers, adjusts the stack, and branches to a
   given PC other than the normal return address.  */

void
expand_eh_return (void)
{
  rtx around_label;

  if (! crtl->eh.ehr_label)
    return;

  crtl->calls_eh_return = 1;

#ifdef EH_RETURN_STACKADJ_RTX
  emit_move_insn (EH_RETURN_STACKADJ_RTX, const0_rtx);
#endif

  around_label = gen_label_rtx ();
  emit_jump (around_label);

  emit_label (crtl->eh.ehr_label);
  clobber_return_register ();

#ifdef EH_RETURN_STACKADJ_RTX
  emit_move_insn (EH_RETURN_STACKADJ_RTX, crtl->eh.ehr_stackadj);
#endif

#ifdef HAVE_eh_return
  if (HAVE_eh_return)
    emit_insn (gen_eh_return (crtl->eh.ehr_handler));
  else
#endif
    {
#ifdef EH_RETURN_HANDLER_RTX
      emit_move_insn (EH_RETURN_HANDLER_RTX, crtl->eh.ehr_handler);
#else
      error ("__builtin_eh_return not supported on this target");
#endif
    }

  emit_label (around_label);
}

/* Convert a ptr_mode address ADDR_TREE to a Pmode address controlled by
   POINTERS_EXTEND_UNSIGNED and return it.  */

rtx
expand_builtin_extend_pointer (tree addr_tree)
{
  rtx addr = expand_expr (addr_tree, NULL_RTX, ptr_mode, EXPAND_NORMAL);
  int extend;

#ifdef POINTERS_EXTEND_UNSIGNED
  extend = POINTERS_EXTEND_UNSIGNED;
#else
  /* The previous EH code did an unsigned extend by default, so we do this also
     for consistency.  */
  extend = 1;
#endif

  return convert_modes (targetm.unwind_word_mode (), ptr_mode, addr, extend);
}

/* In the following functions, we represent entries in the action table
   as 1-based indices.  Special cases are:

	 0:	null action record, non-null landing pad; implies cleanups
	-1:	null action record, null landing pad; implies no action
	-2:	no call-site entry; implies must_not_throw
	-3:	we have yet to process outer regions

   Further, no special cases apply to the "next" field of the record.
   For next, 0 means end of list.  */

struct action_record
{
  int offset;
  int filter;
  int next;
};

static int
action_record_eq (const void *pentry, const void *pdata)
{
  const struct action_record *entry = (const struct action_record *) pentry;
  const struct action_record *data = (const struct action_record *) pdata;
  return entry->filter == data->filter && entry->next == data->next;
}

static hashval_t
action_record_hash (const void *pentry)
{
  const struct action_record *entry = (const struct action_record *) pentry;
  return entry->next * 1009 + entry->filter;
}

static int
add_action_record (htab_t ar_hash, int filter, int next)
{
  struct action_record **slot, *new_ar, tmp;

  tmp.filter = filter;
  tmp.next = next;
  slot = (struct action_record **) htab_find_slot (ar_hash, &tmp, INSERT);

  if ((new_ar = *slot) == NULL)
    {
      new_ar = XNEW (struct action_record);
      new_ar->offset = VEC_length (uchar, crtl->eh.action_record_data) + 1;
      new_ar->filter = filter;
      new_ar->next = next;
      *slot = new_ar;

      /* The filter value goes in untouched.  The link to the next
	 record is a "self-relative" byte offset, or zero to indicate
	 that there is no next record.  So convert the absolute 1 based
	 indices we've been carrying around into a displacement.  */

      push_sleb128 (&crtl->eh.action_record_data, filter);
      if (next)
	next -= VEC_length (uchar, crtl->eh.action_record_data) + 1;
      push_sleb128 (&crtl->eh.action_record_data, next);
    }

  return new_ar->offset;
}

static int
collect_one_action_chain (htab_t ar_hash, eh_region region)
{
  int next;

  /* If we've reached the top of the region chain, then we have
     no actions, and require no landing pad.  */
  if (region == NULL)
    return -1;

  switch (region->type)
    {
    case ERT_CLEANUP:
      {
	eh_region r;
	/* A cleanup adds a zero filter to the beginning of the chain, but
	   there are special cases to look out for.  If there are *only*
	   cleanups along a path, then it compresses to a zero action.
	   Further, if there are multiple cleanups along a path, we only
	   need to represent one of them, as that is enough to trigger
	   entry to the landing pad at runtime.  */
	next = collect_one_action_chain (ar_hash, region->outer);
	if (next <= 0)
	  return 0;
	for (r = region->outer; r ; r = r->outer)
	  if (r->type == ERT_CLEANUP)
	    return next;
	return add_action_record (ar_hash, 0, next);
      }

    case ERT_TRY:
      {
	eh_catch c;

	/* Process the associated catch regions in reverse order.
	   If there's a catch-all handler, then we don't need to
	   search outer regions.  Use a magic -3 value to record
	   that we haven't done the outer search.  */
	next = -3;
	for (c = region->u.eh_try.last_catch; c ; c = c->prev_catch)
	  {
	    if (c->type_list == NULL)
	      {
		/* Retrieve the filter from the head of the filter list
		   where we have stored it (see assign_filter_values).  */
		int filter = TREE_INT_CST_LOW (TREE_VALUE (c->filter_list));
		next = add_action_record (ar_hash, filter, 0);
	      }
	    else
	      {
		/* Once the outer search is done, trigger an action record for
		   each filter we have.  */
		tree flt_node;

		if (next == -3)
		  {
		    next = collect_one_action_chain (ar_hash, region->outer);

		    /* If there is no next action, terminate the chain.  */
		    if (next == -1)
		      next = 0;
		    /* If all outer actions are cleanups or must_not_throw,
		       we'll have no action record for it, since we had wanted
		       to encode these states in the call-site record directly.
		       Add a cleanup action to the chain to catch these.  */
		    else if (next <= 0)
		      next = add_action_record (ar_hash, 0, 0);
		  }

		flt_node = c->filter_list;
		for (; flt_node; flt_node = TREE_CHAIN (flt_node))
		  {
		    int filter = TREE_INT_CST_LOW (TREE_VALUE (flt_node));
		    next = add_action_record (ar_hash, filter, next);
		  }
	      }
	  }
	return next;
      }

    case ERT_ALLOWED_EXCEPTIONS:
      /* An exception specification adds its filter to the
	 beginning of the chain.  */
      next = collect_one_action_chain (ar_hash, region->outer);

      /* If there is no next action, terminate the chain.  */
      if (next == -1)
	next = 0;
      /* If all outer actions are cleanups or must_not_throw,
	 we'll have no action record for it, since we had wanted
	 to encode these states in the call-site record directly.
	 Add a cleanup action to the chain to catch these.  */
      else if (next <= 0)
	next = add_action_record (ar_hash, 0, 0);

      return add_action_record (ar_hash, region->u.allowed.filter, next);

    case ERT_MUST_NOT_THROW:
      /* A must-not-throw region with no inner handlers or cleanups
	 requires no call-site entry.  Note that this differs from
	 the no handler or cleanup case in that we do require an lsda
	 to be generated.  Return a magic -2 value to record this.  */
      return -2;
    }

  gcc_unreachable ();
}

static int
add_call_site (rtx landing_pad, int action, int section)
{
  call_site_record record;

  record = ggc_alloc_call_site_record_d ();
  record->landing_pad = landing_pad;
  record->action = action;

  VEC_safe_push (call_site_record, gc,
		 crtl->eh.call_site_record[section], record);

  return call_site_base + VEC_length (call_site_record,
				      crtl->eh.call_site_record[section]) - 1;
}

/* Turn REG_EH_REGION notes back into NOTE_INSN_EH_REGION notes.
   The new note numbers will not refer to region numbers, but
   instead to call site entries.  */

static unsigned int
convert_to_eh_region_ranges (void)
{
  rtx insn, iter, note;
  htab_t ar_hash;
  int last_action = -3;
  rtx last_action_insn = NULL_RTX;
  rtx last_landing_pad = NULL_RTX;
  rtx first_no_action_insn = NULL_RTX;
  int call_site = 0;
  int cur_sec = 0;
  rtx section_switch_note = NULL_RTX;
  rtx first_no_action_insn_before_switch = NULL_RTX;
  rtx last_no_action_insn_before_switch = NULL_RTX;
  int saved_call_site_base = call_site_base;

  crtl->eh.action_record_data = VEC_alloc (uchar, gc, 64);

  ar_hash = htab_create (31, action_record_hash, action_record_eq, free);

  for (iter = get_insns (); iter ; iter = NEXT_INSN (iter))
    if (INSN_P (iter))
      {
	eh_landing_pad lp;
	eh_region region;
	bool nothrow;
	int this_action;
	rtx this_landing_pad;

	insn = iter;
	if (NONJUMP_INSN_P (insn)
	    && GET_CODE (PATTERN (insn)) == SEQUENCE)
	  insn = XVECEXP (PATTERN (insn), 0, 0);

	nothrow = get_eh_region_and_lp_from_rtx (insn, &region, &lp);
	if (nothrow)
	  continue;
	if (region)
	  this_action = collect_one_action_chain (ar_hash, region);
	else
	  this_action = -1;

	/* Existence of catch handlers, or must-not-throw regions
	   implies that an lsda is needed (even if empty).  */
	if (this_action != -1)
	  crtl->uses_eh_lsda = 1;

	/* Delay creation of region notes for no-action regions
	   until we're sure that an lsda will be required.  */
	else if (last_action == -3)
	  {
	    first_no_action_insn = iter;
	    last_action = -1;
	  }

	if (this_action >= 0)
	  this_landing_pad = lp->landing_pad;
	else
	  this_landing_pad = NULL_RTX;

	/* Differing actions or landing pads implies a change in call-site
	   info, which implies some EH_REGION note should be emitted.  */
	if (last_action != this_action
	    || last_landing_pad != this_landing_pad)
	  {
	    /* If there is a queued no-action region in the other section
	       with hot/cold partitioning, emit it now.  */
	    if (first_no_action_insn_before_switch)
	      {
		gcc_assert (this_action != -1
			    && last_action == (first_no_action_insn
					       ? -1 : -3));
		call_site = add_call_site (NULL_RTX, 0, 0);
		note = emit_note_before (NOTE_INSN_EH_REGION_BEG,
					 first_no_action_insn_before_switch);
		NOTE_EH_HANDLER (note) = call_site;
		note = emit_note_after (NOTE_INSN_EH_REGION_END,
					last_no_action_insn_before_switch);
		NOTE_EH_HANDLER (note) = call_site;
		gcc_assert (last_action != -3
			    || (last_action_insn
				== last_no_action_insn_before_switch));
		first_no_action_insn_before_switch = NULL_RTX;
		last_no_action_insn_before_switch = NULL_RTX;
		call_site_base++;
	      }
	    /* If we'd not seen a previous action (-3) or the previous
	       action was must-not-throw (-2), then we do not need an
	       end note.  */
	    if (last_action >= -1)
	      {
		/* If we delayed the creation of the begin, do it now.  */
		if (first_no_action_insn)
		  {
		    call_site = add_call_site (NULL_RTX, 0, cur_sec);
		    note = emit_note_before (NOTE_INSN_EH_REGION_BEG,
					     first_no_action_insn);
		    NOTE_EH_HANDLER (note) = call_site;
		    first_no_action_insn = NULL_RTX;
		  }

		note = emit_note_after (NOTE_INSN_EH_REGION_END,
					last_action_insn);
		NOTE_EH_HANDLER (note) = call_site;
	      }

	    /* If the new action is must-not-throw, then no region notes
	       are created.  */
	    if (this_action >= -1)
	      {
		call_site = add_call_site (this_landing_pad,
					   this_action < 0 ? 0 : this_action,
					   cur_sec);
		note = emit_note_before (NOTE_INSN_EH_REGION_BEG, iter);
		NOTE_EH_HANDLER (note) = call_site;
	      }

	    last_action = this_action;
	    last_landing_pad = this_landing_pad;
	  }
	last_action_insn = iter;
      }
    else if (NOTE_P (iter)
	     && NOTE_KIND (iter) == NOTE_INSN_SWITCH_TEXT_SECTIONS)
      {
	gcc_assert (section_switch_note == NULL_RTX);
	gcc_assert (flag_reorder_blocks_and_partition);
	section_switch_note = iter;
	if (first_no_action_insn)
	  {
	    first_no_action_insn_before_switch = first_no_action_insn;
	    last_no_action_insn_before_switch = last_action_insn;
	    first_no_action_insn = NULL_RTX;
	    gcc_assert (last_action == -1);
	    last_action = -3;
	  }
	/* Force closing of current EH region before section switch and
	   opening a new one afterwards.  */
	else if (last_action != -3)
	  last_landing_pad = pc_rtx;
	call_site_base += VEC_length (call_site_record,
				      crtl->eh.call_site_record[cur_sec]);
	cur_sec++;
	gcc_assert (crtl->eh.call_site_record[cur_sec] == NULL);
	crtl->eh.call_site_record[cur_sec]
	  = VEC_alloc (call_site_record, gc, 10);
      }

  if (last_action >= -1 && ! first_no_action_insn)
    {
      note = emit_note_after (NOTE_INSN_EH_REGION_END, last_action_insn);
      NOTE_EH_HANDLER (note) = call_site;
    }

  call_site_base = saved_call_site_base;

  htab_delete (ar_hash);
  return 0;
}

static bool
gate_convert_to_eh_region_ranges (void)
{
  /* Nothing to do for SJLJ exceptions or if no regions created.  */
  if (cfun->eh->region_tree == NULL)
    return false;
  if (targetm_common.except_unwind_info (&global_options) == UI_SJLJ)
    return false;
  return true;
}

struct rtl_opt_pass pass_convert_to_eh_region_ranges =
{
 {
  RTL_PASS,
  "eh_ranges",                          /* name */
  gate_convert_to_eh_region_ranges,	/* gate */
  convert_to_eh_region_ranges,          /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_NONE,                              /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  0              			/* todo_flags_finish */
 }
};

static void
push_uleb128 (VEC (uchar, gc) **data_area, unsigned int value)
{
  do
    {
      unsigned char byte = value & 0x7f;
      value >>= 7;
      if (value)
	byte |= 0x80;
      VEC_safe_push (uchar, gc, *data_area, byte);
    }
  while (value);
}

static void
push_sleb128 (VEC (uchar, gc) **data_area, int value)
{
  unsigned char byte;
  int more;

  do
    {
      byte = value & 0x7f;
      value >>= 7;
      more = ! ((value == 0 && (byte & 0x40) == 0)
		|| (value == -1 && (byte & 0x40) != 0));
      if (more)
	byte |= 0x80;
      VEC_safe_push (uchar, gc, *data_area, byte);
    }
  while (more);
}


#ifndef HAVE_AS_LEB128
static int
dw2_size_of_call_site_table (int section)
{
  int n = VEC_length (call_site_record, crtl->eh.call_site_record[section]);
  int size = n * (4 + 4 + 4);
  int i;

  for (i = 0; i < n; ++i)
    {
      struct call_site_record_d *cs =
	VEC_index (call_site_record, crtl->eh.call_site_record[section], i);
      size += size_of_uleb128 (cs->action);
    }

  return size;
}

static int
sjlj_size_of_call_site_table (void)
{
  int n = VEC_length (call_site_record, crtl->eh.call_site_record[0]);
  int size = 0;
  int i;

  for (i = 0; i < n; ++i)
    {
      struct call_site_record_d *cs =
	VEC_index (call_site_record, crtl->eh.call_site_record[0], i);
      size += size_of_uleb128 (INTVAL (cs->landing_pad));
      size += size_of_uleb128 (cs->action);
    }

  return size;
}
#endif

static void
dw2_output_call_site_table (int cs_format, int section)
{
  int n = VEC_length (call_site_record, crtl->eh.call_site_record[section]);
  int i;
  const char *begin;

  if (section == 0)
    begin = current_function_func_begin_label;
  else if (first_function_block_is_cold)
    begin = crtl->subsections.hot_section_label;
  else
    begin = crtl->subsections.cold_section_label;

  for (i = 0; i < n; ++i)
    {
      struct call_site_record_d *cs =
	VEC_index (call_site_record, crtl->eh.call_site_record[section], i);
      char reg_start_lab[32];
      char reg_end_lab[32];
      char landing_pad_lab[32];

      ASM_GENERATE_INTERNAL_LABEL (reg_start_lab, "LEHB", call_site_base + i);
      ASM_GENERATE_INTERNAL_LABEL (reg_end_lab, "LEHE", call_site_base + i);

      if (cs->landing_pad)
	ASM_GENERATE_INTERNAL_LABEL (landing_pad_lab, "L",
				     CODE_LABEL_NUMBER (cs->landing_pad));

      /* ??? Perhaps use insn length scaling if the assembler supports
	 generic arithmetic.  */
      /* ??? Perhaps use attr_length to choose data1 or data2 instead of
	 data4 if the function is small enough.  */
      if (cs_format == DW_EH_PE_uleb128)
	{
	  dw2_asm_output_delta_uleb128 (reg_start_lab, begin,
					"region %d start", i);
	  dw2_asm_output_delta_uleb128 (reg_end_lab, reg_start_lab,
					"length");
	  if (cs->landing_pad)
	    dw2_asm_output_delta_uleb128 (landing_pad_lab, begin,
					  "landing pad");
	  else
	    dw2_asm_output_data_uleb128 (0, "landing pad");
	}
      else
	{
	  dw2_asm_output_delta (4, reg_start_lab, begin,
				"region %d start", i);
	  dw2_asm_output_delta (4, reg_end_lab, reg_start_lab, "length");
	  if (cs->landing_pad)
	    dw2_asm_output_delta (4, landing_pad_lab, begin,
				  "landing pad");
	  else
	    dw2_asm_output_data (4, 0, "landing pad");
	}
      dw2_asm_output_data_uleb128 (cs->action, "action");
    }

  call_site_base += n;
}

static void
sjlj_output_call_site_table (void)
{
  int n = VEC_length (call_site_record, crtl->eh.call_site_record[0]);
  int i;

  for (i = 0; i < n; ++i)
    {
      struct call_site_record_d *cs =
	VEC_index (call_site_record, crtl->eh.call_site_record[0], i);

      dw2_asm_output_data_uleb128 (INTVAL (cs->landing_pad),
				   "region %d landing pad", i);
      dw2_asm_output_data_uleb128 (cs->action, "action");
    }

  call_site_base += n;
}

/* Switch to the section that should be used for exception tables.  */

static void
switch_to_exception_section (const char * ARG_UNUSED (fnname))
{
  section *s;

  if (exception_section)
    s = exception_section;
  else
    {
      /* Compute the section and cache it into exception_section,
	 unless it depends on the function name.  */
      if (targetm_common.have_named_sections)
	{
	  int flags;

	  if (EH_TABLES_CAN_BE_READ_ONLY)
	    {
	      int tt_format =
		ASM_PREFERRED_EH_DATA_FORMAT (/*code=*/0, /*global=*/1);
	      flags = ((! flag_pic
			|| ((tt_format & 0x70) != DW_EH_PE_absptr
			    && (tt_format & 0x70) != DW_EH_PE_aligned))
		       ? 0 : SECTION_WRITE);
	    }
	  else
	    flags = SECTION_WRITE;

#ifdef HAVE_LD_EH_GC_SECTIONS
	  if (flag_function_sections)
	    {
	      char *section_name = XNEWVEC (char, strlen (fnname) + 32);
	      sprintf (section_name, ".gcc_except_table.%s", fnname);
	      s = get_section (section_name, flags, NULL);
	      free (section_name);
	    }
	  else
#endif
	    exception_section
	      = s = get_section (".gcc_except_table", flags, NULL);
	}
      else
	exception_section
	  = s = flag_pic ? data_section : readonly_data_section;
    }

  switch_to_section (s);
}


/* Output a reference from an exception table to the type_info object TYPE.
   TT_FORMAT and TT_FORMAT_SIZE describe the DWARF encoding method used for
   the value.  */

static void
output_ttype (tree type, int tt_format, int tt_format_size)
{
  rtx value;
  bool is_public = true;

  if (type == NULL_TREE)
    value = const0_rtx;
  else
    {
      /* FIXME lto.  pass_ipa_free_lang_data changes all types to
	 runtime types so TYPE should already be a runtime type
	 reference.  When pass_ipa_free_lang data is made a default
	 pass, we can then remove the call to lookup_type_for_runtime
	 below.  */
      if (TYPE_P (type))
	type = lookup_type_for_runtime (type);

      value = expand_expr (type, NULL_RTX, VOIDmode, EXPAND_INITIALIZER);

      /* Let cgraph know that the rtti decl is used.  Not all of the
	 paths below go through assemble_integer, which would take
	 care of this for us.  */
      STRIP_NOPS (type);
      if (TREE_CODE (type) == ADDR_EXPR)
	{
	  type = TREE_OPERAND (type, 0);
	  if (TREE_CODE (type) == VAR_DECL)
	    is_public = TREE_PUBLIC (type);
	}
      else
	gcc_assert (TREE_CODE (type) == INTEGER_CST);
    }

  /* Allow the target to override the type table entry format.  */
  if (targetm.asm_out.ttype (value))
    return;

  if (tt_format == DW_EH_PE_absptr || tt_format == DW_EH_PE_aligned)
    assemble_integer (value, tt_format_size,
		      tt_format_size * BITS_PER_UNIT, 1);
  else
    dw2_asm_output_encoded_addr_rtx (tt_format, value, is_public, NULL);
}

static void
output_one_function_exception_table (int section)
{
  int tt_format, cs_format, lp_format, i;
#ifdef HAVE_AS_LEB128
  char ttype_label[32];
  char cs_after_size_label[32];
  char cs_end_label[32];
#else
  int call_site_len;
#endif
  int have_tt_data;
  int tt_format_size = 0;

  have_tt_data = (VEC_length (tree, cfun->eh->ttype_data)
		  || (targetm.arm_eabi_unwinder
		      ? VEC_length (tree, cfun->eh->ehspec_data.arm_eabi)
		      : VEC_length (uchar, cfun->eh->ehspec_data.other)));

  /* Indicate the format of the @TType entries.  */
  if (! have_tt_data)
    tt_format = DW_EH_PE_omit;
  else
    {
      tt_format = ASM_PREFERRED_EH_DATA_FORMAT (/*code=*/0, /*global=*/1);
#ifdef HAVE_AS_LEB128
      ASM_GENERATE_INTERNAL_LABEL (ttype_label,
				   section ? "LLSDATTC" : "LLSDATT",
				   current_function_funcdef_no);
#endif
      tt_format_size = size_of_encoded_value (tt_format);

      assemble_align (tt_format_size * BITS_PER_UNIT);
    }

  targetm.asm_out.internal_label (asm_out_file, section ? "LLSDAC" : "LLSDA",
				  current_function_funcdef_no);

  /* The LSDA header.  */

  /* Indicate the format of the landing pad start pointer.  An omitted
     field implies @LPStart == @Start.  */
  /* Currently we always put @LPStart == @Start.  This field would
     be most useful in moving the landing pads completely out of
     line to another section, but it could also be used to minimize
     the size of uleb128 landing pad offsets.  */
  lp_format = DW_EH_PE_omit;
  dw2_asm_output_data (1, lp_format, "@LPStart format (%s)",
		       eh_data_format_name (lp_format));

  /* @LPStart pointer would go here.  */

  dw2_asm_output_data (1, tt_format, "@TType format (%s)",
		       eh_data_format_name (tt_format));

#ifndef HAVE_AS_LEB128
  if (targetm_common.except_unwind_info (&global_options) == UI_SJLJ)
    call_site_len = sjlj_size_of_call_site_table ();
  else
    call_site_len = dw2_size_of_call_site_table (section);
#endif

  /* A pc-relative 4-byte displacement to the @TType data.  */
  if (have_tt_data)
    {
#ifdef HAVE_AS_LEB128
      char ttype_after_disp_label[32];
      ASM_GENERATE_INTERNAL_LABEL (ttype_after_disp_label,
				   section ? "LLSDATTDC" : "LLSDATTD",
				   current_function_funcdef_no);
      dw2_asm_output_delta_uleb128 (ttype_label, ttype_after_disp_label,
				    "@TType base offset");
      ASM_OUTPUT_LABEL (asm_out_file, ttype_after_disp_label);
#else
      /* Ug.  Alignment queers things.  */
      unsigned int before_disp, after_disp, last_disp, disp;

      before_disp = 1 + 1;
      after_disp = (1 + size_of_uleb128 (call_site_len)
		    + call_site_len
		    + VEC_length (uchar, crtl->eh.action_record_data)
		    + (VEC_length (tree, cfun->eh->ttype_data)
		       * tt_format_size));

      disp = after_disp;
      do
	{
	  unsigned int disp_size, pad;

	  last_disp = disp;
	  disp_size = size_of_uleb128 (disp);
	  pad = before_disp + disp_size + after_disp;
	  if (pad % tt_format_size)
	    pad = tt_format_size - (pad % tt_format_size);
	  else
	    pad = 0;
	  disp = after_disp + pad;
	}
      while (disp != last_disp);

      dw2_asm_output_data_uleb128 (disp, "@TType base offset");
#endif
    }

  /* Indicate the format of the call-site offsets.  */
#ifdef HAVE_AS_LEB128
  cs_format = DW_EH_PE_uleb128;
#else
  cs_format = DW_EH_PE_udata4;
#endif
  dw2_asm_output_data (1, cs_format, "call-site format (%s)",
		       eh_data_format_name (cs_format));

#ifdef HAVE_AS_LEB128
  ASM_GENERATE_INTERNAL_LABEL (cs_after_size_label,
			       section ? "LLSDACSBC" : "LLSDACSB",
			       current_function_funcdef_no);
  ASM_GENERATE_INTERNAL_LABEL (cs_end_label,
			       section ? "LLSDACSEC" : "LLSDACSE",
			       current_function_funcdef_no);
  dw2_asm_output_delta_uleb128 (cs_end_label, cs_after_size_label,
				"Call-site table length");
  ASM_OUTPUT_LABEL (asm_out_file, cs_after_size_label);
  if (targetm_common.except_unwind_info (&global_options) == UI_SJLJ)
    sjlj_output_call_site_table ();
  else
    dw2_output_call_site_table (cs_format, section);
  ASM_OUTPUT_LABEL (asm_out_file, cs_end_label);
#else
  dw2_asm_output_data_uleb128 (call_site_len, "Call-site table length");
  if (targetm_common.except_unwind_info (&global_options) == UI_SJLJ)
    sjlj_output_call_site_table ();
  else
    dw2_output_call_site_table (cs_format, section);
#endif

  /* ??? Decode and interpret the data for flag_debug_asm.  */
  {
    uchar uc;
    FOR_EACH_VEC_ELT (uchar, crtl->eh.action_record_data, i, uc)
      dw2_asm_output_data (1, uc, i ? NULL : "Action record table");
  }

  if (have_tt_data)
    assemble_align (tt_format_size * BITS_PER_UNIT);

  i = VEC_length (tree, cfun->eh->ttype_data);
  while (i-- > 0)
    {
      tree type = VEC_index (tree, cfun->eh->ttype_data, i);
      output_ttype (type, tt_format, tt_format_size);
    }

#ifdef HAVE_AS_LEB128
  if (have_tt_data)
      ASM_OUTPUT_LABEL (asm_out_file, ttype_label);
#endif

  /* ??? Decode and interpret the data for flag_debug_asm.  */
  if (targetm.arm_eabi_unwinder)
    {
      tree type;
      for (i = 0;
	   VEC_iterate (tree, cfun->eh->ehspec_data.arm_eabi, i, type); ++i)
	output_ttype (type, tt_format, tt_format_size);
    }
  else
    {
      uchar uc;
      for (i = 0;
	   VEC_iterate (uchar, cfun->eh->ehspec_data.other, i, uc); ++i)
	dw2_asm_output_data (1, uc,
			     i ? NULL : "Exception specification table");
    }
}

void
output_function_exception_table (const char *fnname)
{
  rtx personality = get_personality_function (current_function_decl);

  /* Not all functions need anything.  */
  if (! crtl->uses_eh_lsda)
    return;

  if (personality)
    {
      assemble_external_libcall (personality);

      if (targetm.asm_out.emit_except_personality)
	targetm.asm_out.emit_except_personality (personality);
    }

  switch_to_exception_section (fnname);

  /* If the target wants a label to begin the table, emit it here.  */
  targetm.asm_out.emit_except_table_label (asm_out_file);

  output_one_function_exception_table (0);
  if (crtl->eh.call_site_record[1] != NULL)
    output_one_function_exception_table (1);

  switch_to_section (current_function_section ());
}

void
set_eh_throw_stmt_table (struct function *fun, struct htab *table)
{
  fun->eh->throw_stmt_table = table;
}

htab_t
get_eh_throw_stmt_table (struct function *fun)
{
  return fun->eh->throw_stmt_table;
}

/* Determine if the function needs an EH personality function.  */

enum eh_personality_kind
function_needs_eh_personality (struct function *fn)
{
  enum eh_personality_kind kind = eh_personality_none;
  eh_region i;

  FOR_ALL_EH_REGION_FN (i, fn)
    {
      switch (i->type)
	{
	case ERT_CLEANUP:
	  /* Can do with any personality including the generic C one.  */
	  kind = eh_personality_any;
	  break;

	case ERT_TRY:
	case ERT_ALLOWED_EXCEPTIONS:
	  /* Always needs a EH personality function.  The generic C
	     personality doesn't handle these even for empty type lists.  */
	  return eh_personality_lang;

	case ERT_MUST_NOT_THROW:
	  /* Always needs a EH personality function.  The language may specify
	     what abort routine that must be used, e.g. std::terminate.  */
	  return eh_personality_lang;
	}
    }

  return kind;
}

/* Dump EH information to OUT.  */

void
dump_eh_tree (FILE * out, struct function *fun)
{
  eh_region i;
  int depth = 0;
  static const char *const type_name[] = {
    "cleanup", "try", "allowed_exceptions", "must_not_throw"
  };

  i = fun->eh->region_tree;
  if (!i)
    return;

  fprintf (out, "Eh tree:\n");
  while (1)
    {
      fprintf (out, "  %*s %i %s", depth * 2, "",
	       i->index, type_name[(int) i->type]);

      if (i->landing_pads)
	{
	  eh_landing_pad lp;

	  fprintf (out, " land:");
	  if (current_ir_type () == IR_GIMPLE)
	    {
	      for (lp = i->landing_pads; lp ; lp = lp->next_lp)
		{
		  fprintf (out, "{%i,", lp->index);
		  print_generic_expr (out, lp->post_landing_pad, 0);
		  fputc ('}', out);
		  if (lp->next_lp)
		    fputc (',', out);
		}
	    }
	  else
	    {
	      for (lp = i->landing_pads; lp ; lp = lp->next_lp)
		{
		  fprintf (out, "{%i,", lp->index);
		  if (lp->landing_pad)
		    fprintf (out, "%i%s,", INSN_UID (lp->landing_pad),
			     NOTE_P (lp->landing_pad) ? "(del)" : "");
		  else
		    fprintf (out, "(nil),");
		  if (lp->post_landing_pad)
		    {
		      rtx lab = label_rtx (lp->post_landing_pad);
		      fprintf (out, "%i%s}", INSN_UID (lab),
			       NOTE_P (lab) ? "(del)" : "");
		    }
		  else
		    fprintf (out, "(nil)}");
		  if (lp->next_lp)
		    fputc (',', out);
		}
	    }
	}

      switch (i->type)
	{
	case ERT_CLEANUP:
	case ERT_MUST_NOT_THROW:
	  break;

	case ERT_TRY:
	  {
	    eh_catch c;
	    fprintf (out, " catch:");
	    for (c = i->u.eh_try.first_catch; c; c = c->next_catch)
	      {
		fputc ('{', out);
		if (c->label)
		  {
		    fprintf (out, "lab:");
		    print_generic_expr (out, c->label, 0);
		    fputc (';', out);
		  }
		print_generic_expr (out, c->type_list, 0);
		fputc ('}', out);
		if (c->next_catch)
		  fputc (',', out);
	      }
	  }
	  break;

	case ERT_ALLOWED_EXCEPTIONS:
	  fprintf (out, " filter :%i types:", i->u.allowed.filter);
	  print_generic_expr (out, i->u.allowed.type_list, 0);
	  break;
	}
      fputc ('\n', out);

      /* If there are sub-regions, process them.  */
      if (i->inner)
	i = i->inner, depth++;
      /* If there are peers, process them.  */
      else if (i->next_peer)
	i = i->next_peer;
      /* Otherwise, step back up the tree to the next peer.  */
      else
	{
	  do
	    {
	      i = i->outer;
	      depth--;
	      if (i == NULL)
		return;
	    }
	  while (i->next_peer == NULL);
	  i = i->next_peer;
	}
    }
}

/* Dump the EH tree for FN on stderr.  */

DEBUG_FUNCTION void
debug_eh_tree (struct function *fn)
{
  dump_eh_tree (stderr, fn);
}

/* Verify invariants on EH datastructures.  */

DEBUG_FUNCTION void
verify_eh_tree (struct function *fun)
{
  eh_region r, outer;
  int nvisited_lp, nvisited_r;
  int count_lp, count_r, depth, i;
  eh_landing_pad lp;
  bool err = false;

  if (!fun->eh->region_tree)
    return;

  count_r = 0;
  for (i = 1; VEC_iterate (eh_region, fun->eh->region_array, i, r); ++i)
    if (r)
      {
	if (r->index == i)
	  count_r++;
	else
	  {
	    error ("region_array is corrupted for region %i", r->index);
	    err = true;
	  }
      }

  count_lp = 0;
  for (i = 1; VEC_iterate (eh_landing_pad, fun->eh->lp_array, i, lp); ++i)
    if (lp)
      {
	if (lp->index == i)
	  count_lp++;
	else
	  {
	    error ("lp_array is corrupted for lp %i", lp->index);
	    err = true;
	  }
      }

  depth = nvisited_lp = nvisited_r = 0;
  outer = NULL;
  r = fun->eh->region_tree;
  while (1)
    {
      if (VEC_index (eh_region, fun->eh->region_array, r->index) != r)
	{
	  error ("region_array is corrupted for region %i", r->index);
	  err = true;
	}
      if (r->outer != outer)
	{
	  error ("outer block of region %i is wrong", r->index);
	  err = true;
	}
      if (depth < 0)
	{
	  error ("negative nesting depth of region %i", r->index);
	  err = true;
	}
      nvisited_r++;

      for (lp = r->landing_pads; lp ; lp = lp->next_lp)
	{
	  if (VEC_index (eh_landing_pad, fun->eh->lp_array, lp->index) != lp)
	    {
	      error ("lp_array is corrupted for lp %i", lp->index);
	      err = true;
	    }
	  if (lp->region != r)
	    {
	      error ("region of lp %i is wrong", lp->index);
	      err = true;
	    }
	  nvisited_lp++;
	}

      if (r->inner)
	outer = r, r = r->inner, depth++;
      else if (r->next_peer)
	r = r->next_peer;
      else
	{
	  do
	    {
	      r = r->outer;
	      if (r == NULL)
		goto region_done;
	      depth--;
	      outer = r->outer;
	    }
	  while (r->next_peer == NULL);
	  r = r->next_peer;
	}
    }
 region_done:
  if (depth != 0)
    {
      error ("tree list ends on depth %i", depth);
      err = true;
    }
  if (count_r != nvisited_r)
    {
      error ("region_array does not match region_tree");
      err = true;
    }
  if (count_lp != nvisited_lp)
    {
      error ("lp_array does not match region_tree");
      err = true;
    }

  if (err)
    {
      dump_eh_tree (stderr, fun);
      internal_error ("verify_eh_tree failed");
    }
}

#include "gt-except.h"
