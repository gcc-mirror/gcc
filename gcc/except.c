/* Implements exception handling.
   Copyright (C) 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
   Free Software Foundation, Inc.
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


/* An exception is an event that can be signaled from within a
   function. This event can then be "caught" or "trapped" by the
   callers of this function. This potentially allows program flow to
   be transferred to any arbitrary code associated with a function call
   several levels up the stack.

   The intended use for this mechanism is for signaling "exceptional
   events" in an out-of-band fashion, hence its name. The C++ language
   (and many other OO-styled or functional languages) practically
   requires such a mechanism, as otherwise it becomes very difficult
   or even impossible to signal failure conditions in complex
   situations.  The traditional C++ example is when an error occurs in
   the process of constructing an object; without such a mechanism, it
   is impossible to signal that the error occurs without adding global
   state variables and error checks around every object construction.

   The act of causing this event to occur is referred to as "throwing
   an exception". (Alternate terms include "raising an exception" or
   "signaling an exception".) The term "throw" is used because control
   is returned to the callers of the function that is signaling the
   exception, and thus there is the concept of "throwing" the
   exception up the call stack.

   [ Add updated documentation on how to use this.  ]  */


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
#include "integrate.h"
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
#include "langhooks.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "tree-pass.h"
#include "timevar.h"
#include "tree-flow.h"

/* Provide defaults for stuff that may not be defined when using
   sjlj exceptions.  */
#ifndef EH_RETURN_DATA_REGNO
#define EH_RETURN_DATA_REGNO(N) INVALID_REGNUM
#endif

/* Protect cleanup actions with must-not-throw regions, with a call
   to the given failure handler.  */
gimple (*lang_protect_cleanup_actions) (void);

/* Return true if type A catches type B.  */
int (*lang_eh_type_covers) (tree a, tree b);

/* Map a type to a runtime object to match type.  */
tree (*lang_eh_runtime_type) (tree);

/* A hash table of label to region number.  */

struct GTY(()) ehl_map_entry {
  rtx label;
  struct eh_region_d *region;
};

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

static int t2r_eq (const void *, const void *);
static hashval_t t2r_hash (const void *);

static int ttypes_filter_eq (const void *, const void *);
static hashval_t ttypes_filter_hash (const void *);
static int ehspec_filter_eq (const void *, const void *);
static hashval_t ehspec_filter_hash (const void *);
static int add_ttypes_entry (htab_t, tree);
static int add_ehspec_entry (htab_t, htab_t, tree);
static void assign_filter_values (void);
static void build_post_landing_pads (void);
static void connect_post_landing_pads (void);
static void dw2_build_landing_pads (void);

struct sjlj_lp_info;
static bool sjlj_find_directly_reachable_regions (struct sjlj_lp_info *);
static void sjlj_assign_call_site_values (rtx, struct sjlj_lp_info *);
static void sjlj_mark_call_sites (struct sjlj_lp_info *);
static void sjlj_emit_function_enter (rtx);
static void sjlj_emit_function_exit (void);
static void sjlj_emit_dispatch_table (rtx, struct sjlj_lp_info *);
static void sjlj_build_landing_pads (void);

static void remove_eh_handler (struct eh_region_d *);
static void remove_eh_handler_and_replace (struct eh_region_d *,
					   struct eh_region_d *, bool);

/* The return value of reachable_next_level.  */
enum reachable_code
{
  /* The given exception is not processed by the given region.  */
  RNL_NOT_CAUGHT,
  /* The given exception may need processing by the given region.  */
  RNL_MAYBE_CAUGHT,
  /* The given exception is completely processed by the given region.  */
  RNL_CAUGHT,
  /* The given exception is completely processed by the runtime.  */
  RNL_BLOCKED
};

struct reachable_info;
static enum reachable_code reachable_next_level (struct eh_region_d *, tree,
						 struct reachable_info *, bool);

static int action_record_eq (const void *, const void *);
static hashval_t action_record_hash (const void *);
static int add_action_record (htab_t, int, int);
static int collect_one_action_chain (htab_t, struct eh_region_d *);
static int add_call_site (rtx, int);

static void push_uleb128 (varray_type *, unsigned int);
static void push_sleb128 (varray_type *, int);
#ifndef HAVE_AS_LEB128
static int dw2_size_of_call_site_table (void);
static int sjlj_size_of_call_site_table (void);
#endif
static void dw2_output_call_site_table (void);
static void sjlj_output_call_site_table (void);


/* Routine to see if exception handling is turned on.
   DO_WARN is nonzero if we want to inform the user that exception
   handling is turned off.

   This is used to ensure that -fexceptions has been specified if the
   compiler tries to use any exception-specific functions.  */

int
doing_eh (int do_warn)
{
  if (! flag_exceptions)
    {
      static int warned = 0;
      if (! warned && do_warn)
	{
	  error ("exception handling disabled, use -fexceptions to enable");
	  warned = 1;
	}
      return 0;
    }
  return 1;
}


void
init_eh (void)
{
  if (! flag_exceptions)
    return;

  type_to_runtime_map = htab_create_ggc (31, t2r_hash, t2r_eq, NULL);

  /* Create the SjLj_Function_Context structure.  This should match
     the definition in unwind-sjlj.c.  */
  if (USING_SJLJ_EXCEPTIONS)
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

      tmp = build_index_type (build_int_cst (NULL_TREE, 4 - 1));
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
      tmp = build_int_cst (NULL_TREE, JMP_BUF_SIZE - 1);
#else
      /* Should be large enough for most systems, if it is not,
	 JMP_BUF_SIZE should be defined with the proper value.  It will
	 also tend to be larger than necessary for most systems, a more
	 optimal port will define JMP_BUF_SIZE.  */
      tmp = build_int_cst (NULL_TREE, FIRST_PSEUDO_REGISTER + 2 - 1);
#endif
#else
      /* builtin_setjmp takes a pointer to 5 words.  */
      tmp = build_int_cst (NULL_TREE, 5 * BITS_PER_WORD / POINTER_SIZE - 1);
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
  cfun->eh = GGC_CNEW (struct eh_status);
}

/* Routines to generate the exception tree somewhat directly.
   These are used from tree-eh.c when processing exception related
   nodes during tree optimization.  */

static struct eh_region_d *
gen_eh_region (enum eh_region_type type, struct eh_region_d *outer)
{
  struct eh_region_d *new_eh;

#ifdef ENABLE_CHECKING
  gcc_assert (doing_eh (0));
#endif

  /* Insert a new blank region as a leaf in the tree.  */
  new_eh = GGC_CNEW (struct eh_region_d);
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

  new_eh->region_number = ++cfun->eh->last_region_number;

  return new_eh;
}

struct eh_region_d *
gen_eh_region_cleanup (struct eh_region_d *outer)
{
  struct eh_region_d *cleanup = gen_eh_region (ERT_CLEANUP, outer);
  return cleanup;
}

struct eh_region_d *
gen_eh_region_try (struct eh_region_d *outer)
{
  return gen_eh_region (ERT_TRY, outer);
}

struct eh_region_d *
gen_eh_region_catch (struct eh_region_d *t, tree type_or_list)
{
  struct eh_region_d *c, *l;
  tree type_list, type_node;

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

  c = gen_eh_region (ERT_CATCH, t->outer);
  c->u.eh_catch.type_list = type_list;
  l = t->u.eh_try.last_catch;
  c->u.eh_catch.prev_catch = l;
  if (l)
    l->u.eh_catch.next_catch = c;
  else
    t->u.eh_try.eh_catch = c;
  t->u.eh_try.last_catch = c;

  return c;
}

struct eh_region_d *
gen_eh_region_allowed (struct eh_region_d *outer, tree allowed)
{
  struct eh_region_d *region = gen_eh_region (ERT_ALLOWED_EXCEPTIONS, outer);
  region->u.allowed.type_list = allowed;

  for (; allowed ; allowed = TREE_CHAIN (allowed))
    add_type_for_runtime (TREE_VALUE (allowed));

  return region;
}

struct eh_region_d *
gen_eh_region_must_not_throw (struct eh_region_d *outer)
{
  return gen_eh_region (ERT_MUST_NOT_THROW, outer);
}

int
get_eh_region_number (struct eh_region_d *region)
{
  return region->region_number;
}

bool
get_eh_region_may_contain_throw (struct eh_region_d *region)
{
  return region->may_contain_throw;
}

tree
get_eh_region_tree_label (struct eh_region_d *region)
{
  return region->tree_label;
}

tree
get_eh_region_no_tree_label (int region)
{
  return VEC_index (eh_region, cfun->eh->region_array, region)->tree_label;
}

void
set_eh_region_tree_label (struct eh_region_d *region, tree lab)
{
  region->tree_label = lab;
}

void
expand_resx_expr (tree exp)
{
  int region_nr = TREE_INT_CST_LOW (TREE_OPERAND (exp, 0));
  rtx insn;
  struct eh_region_d *reg = VEC_index (eh_region,
				       cfun->eh->region_array, region_nr);

  do_pending_stack_adjust ();
  insn = emit_jump_insn (gen_rtx_RESX (VOIDmode, region_nr));
  if (reg->resume)
    reg->resume = gen_rtx_INSN_LIST (VOIDmode, insn, reg->resume);
  else
    reg->resume = insn;
  emit_barrier ();
}

/* Note that the current EH region (if any) may contain a throw, or a
   call to a function which itself may contain a throw.  */

void
note_eh_region_may_contain_throw (struct eh_region_d *region)
{
  while (region && !region->may_contain_throw)
    {
      region->may_contain_throw = 1;
      region = region->outer;
    }
}


/* Return an rtl expression for a pointer to the exception object
   within a handler.  */

rtx
get_exception_pointer (void)
{
  if (! crtl->eh.exc_ptr)
    crtl->eh.exc_ptr = gen_reg_rtx (ptr_mode);
  return crtl->eh.exc_ptr;
}

/* Return an rtl expression for the exception dispatch filter
   within a handler.  */

rtx
get_exception_filter (void)
{
  if (! crtl->eh.filter)
    crtl->eh.filter = gen_reg_rtx (targetm.eh_return_filter_mode ());
  return crtl->eh.filter;
}

/* This section is for the exception handling specific optimization pass.  */

/* Random access the exception region tree.  */

void
collect_eh_region_array (void)
{
  struct eh_region_d *i;

  i = cfun->eh->region_tree;
  if (! i)
    return;

  VEC_safe_grow (eh_region, gc, cfun->eh->region_array,
		 cfun->eh->last_region_number + 1);
  VEC_replace (eh_region, cfun->eh->region_array, 0, 0);

  while (1)
    {
      VEC_replace (eh_region, cfun->eh->region_array, i->region_number, i);

      /* If there are sub-regions, process them.  */
      if (i->inner)
	i = i->inner;
      /* If there are peers, process them.  */
      else if (i->next_peer)
	i = i->next_peer;
      /* Otherwise, step back up the tree to the next peer.  */
      else
	{
	  do {
	    i = i->outer;
	    if (i == NULL)
	      return;
	  } while (i->next_peer == NULL);
	  i = i->next_peer;
	}
    }
}

/* R is MUST_NOT_THROW region that is not reachable via local
   RESX instructions.  It still must be kept in the tree in case runtime
   can unwind through it, or we will eliminate out terminate call
   runtime would do otherwise.  Return TRUE if R contains throwing statements
   or some of the exceptions in inner regions can be unwound up to R. 
   
   CONTAINS_STMT is bitmap of all regions that contains some throwing
   statements.  
   
   Function looks O(^3) at first sight.  In fact the function is called at most
   once for every MUST_NOT_THROW in EH tree from remove_unreachable_regions
   Because the outer loop walking subregions does not dive in MUST_NOT_THROW,
   the outer loop examines every region at most once.  The inner loop
   is doing unwinding from the throwing statement same way as we do during
   CFG construction, so it is O(^2) in size of EH tree, but O(n) in size
   of CFG.  In practice Eh trees are wide, not deep, so this is not
   a problem.  */

static bool
can_be_reached_by_runtime (sbitmap contains_stmt, struct eh_region_d *r)
{
  struct eh_region_d *i = r->inner;
  unsigned n;
  bitmap_iterator bi;

  if (TEST_BIT (contains_stmt, r->region_number))
    return true;
  if (r->aka)
    EXECUTE_IF_SET_IN_BITMAP (r->aka, 0, n, bi)
      if (TEST_BIT (contains_stmt, n))
      return true;
  if (!i)
    return false;
  while (1)
    {
      /* It is pointless to look into MUST_NOT_THROW
         or dive into subregions.  They never unwind up.  */
      if (i->type != ERT_MUST_NOT_THROW)
	{
	  bool found = TEST_BIT (contains_stmt, i->region_number);
	  if (!found && i->aka)
	    EXECUTE_IF_SET_IN_BITMAP (i->aka, 0, n, bi)
	      if (TEST_BIT (contains_stmt, n))
	      {
		found = true;
		break;
	      }
	  /* We have nested region that contains throwing statement.
	     See if resuming might lead up to the resx or we get locally
	     caught sooner.  If we get locally caught sooner, we either
	     know region R is not reachable or it would have direct edge
	     from the EH resx and thus consider region reachable at
	     firest place.  */
	  if (found)
	    {
	      struct eh_region_d *i1 = i;
	      tree type_thrown = NULL_TREE;

	      if (i1->type == ERT_THROW)
		{
		  type_thrown = i1->u.eh_throw.type;
		  i1 = i1->outer;
		}
	      for (; i1 != r; i1 = i1->outer)
		if (reachable_next_level (i1, type_thrown, NULL,
					  false) >= RNL_CAUGHT)
		  break;
	      if (i1 == r)
		return true;
	    }
	}
      /* If there are sub-regions, process them.  */
      if (i->type != ERT_MUST_NOT_THROW && i->inner)
	i = i->inner;
      /* If there are peers, process them.  */
      else if (i->next_peer)
	i = i->next_peer;
      /* Otherwise, step back up the tree to the next peer.  */
      else
	{
	  do
	    {
	      i = i->outer;
	      if (i == r)
		return false;
	    }
	  while (i->next_peer == NULL);
	  i = i->next_peer;
	}
    }
}

/* Bring region R to the root of tree.  */

static void
bring_to_root (struct eh_region_d *r)
{
  struct eh_region_d **pp;
  struct eh_region_d *outer = r->outer;
  if (!r->outer)
    return;
  for (pp = &outer->inner; *pp != r; pp = &(*pp)->next_peer)
    continue;
  *pp = r->next_peer;
  r->outer = NULL;
  r->next_peer = cfun->eh->region_tree;
  cfun->eh->region_tree = r;
}

/* Return true if region R2 can be replaced by R1.  */

static bool
eh_region_replaceable_by_p (const struct eh_region_d *r1,
			    const struct eh_region_d *r2)
{
  /* Regions are semantically same if they are of same type,
     have same label and type.  */
  if (r1->type != r2->type)
    return false;
  if (r1->tree_label != r2->tree_label)
    return false;

  /* Verify that also region type dependent data are the same.  */
  switch (r1->type)
    {
      case ERT_MUST_NOT_THROW:
      case ERT_CLEANUP:
	break;
      case ERT_TRY:
	{
	  struct eh_region_d *c1, *c2;
	  for (c1 = r1->u.eh_try.eh_catch,
	       c2 = r2->u.eh_try.eh_catch;
	       c1 && c2;
	       c1 = c1->u.eh_catch.next_catch,
	       c2 = c2->u.eh_catch.next_catch)
	    if (!eh_region_replaceable_by_p (c1, c2))
	      return false;
	  if (c1 || c2)
	    return false;
        }
	break;
      case ERT_CATCH:
        if (!list_equal_p (r1->u.eh_catch.type_list, r2->u.eh_catch.type_list))
	  return false;
        if (!list_equal_p (r1->u.eh_catch.filter_list,
			   r2->u.eh_catch.filter_list))
	  return false;
        break;
      case ERT_ALLOWED_EXCEPTIONS:
        if (!list_equal_p (r1->u.allowed.type_list, r2->u.allowed.type_list))
	  return false;
	if (r1->u.allowed.filter != r2->u.allowed.filter)
	  return false;
	break;
      case ERT_THROW:
	if (r1->u.eh_throw.type != r2->u.eh_throw.type)
	  return false;
	break;
      default:
        gcc_unreachable ();
    }
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Regions %i and %i match\n", r1->region_number,
    						     r2->region_number);
  return true;
}

/* Replace region R2 by R1.  */

static void
replace_region (struct eh_region_d *r1, struct eh_region_d *r2)
{
  struct eh_region_d *next1 = r1->u.eh_try.eh_catch;
  struct eh_region_d *next2 = r2->u.eh_try.eh_catch;
  bool is_try = r1->type == ERT_TRY;

  gcc_assert (r1->type != ERT_CATCH);
  remove_eh_handler_and_replace (r2, r1, false);
  if (is_try)
    {
      while (next1)
	{
	  r1 = next1;
	  r2 = next2;
	  gcc_assert (next1->type == ERT_CATCH);
	  gcc_assert (next2->type == ERT_CATCH);
	  next1 = next1->u.eh_catch.next_catch;
	  next2 = next2->u.eh_catch.next_catch;
	  remove_eh_handler_and_replace (r2, r1, false);
	}
    }
}

/* Return hash value of type list T.  */

static hashval_t
hash_type_list (tree t)
{
  hashval_t val = 0;
  for (; t; t = TREE_CHAIN (t))
    val = iterative_hash_hashval_t (TREE_HASH (TREE_VALUE (t)), val);
  return val;
}

/* Hash EH regions so semantically same regions get same hash value.  */

static hashval_t
hash_eh_region (const void *r)
{
  const struct eh_region_d *region = (const struct eh_region_d *) r;
  hashval_t val = region->type;

  if (region->tree_label)
    val = iterative_hash_hashval_t (LABEL_DECL_UID (region->tree_label), val);
  switch (region->type)
    {
      case ERT_MUST_NOT_THROW:
      case ERT_CLEANUP:
	break;
      case ERT_TRY:
	{
	  struct eh_region_d *c;
	  for (c = region->u.eh_try.eh_catch;
	       c; c = c->u.eh_catch.next_catch)
	    val = iterative_hash_hashval_t (hash_eh_region (c), val);
        }
	break;
      case ERT_CATCH:
        val = iterative_hash_hashval_t (hash_type_list
					  (region->u.eh_catch.type_list), val);
        break;
      case ERT_ALLOWED_EXCEPTIONS:
        val = iterative_hash_hashval_t
		(hash_type_list (region->u.allowed.type_list), val);
        val = iterative_hash_hashval_t (region->u.allowed.filter, val);
	break;
      case ERT_THROW:
        val |= iterative_hash_hashval_t (TYPE_UID (region->u.eh_throw.type), val);
	break;
      default:
        gcc_unreachable ();
    }
  return val;
}

/* Return true if regions R1 and R2 are equal.  */

static int
eh_regions_equal_p (const void *r1, const void *r2)
{
  return eh_region_replaceable_by_p ((const struct eh_region_d *) r1,
				     (const struct eh_region_d *) r2);
}

/* Walk all peers of REGION and try to merge those regions
   that are semantically equivalent.  Look into subregions
   recursively too.  */

static bool
merge_peers (struct eh_region_d *region)
{
  struct eh_region_d *r1, *r2, *outer = NULL, *next;
  bool merged = false;
  int num_regions = 0;
  if (region)
    outer = region->outer;
  else
    return false;

  /* First see if there is inner region equivalent to region
     in question.  EH control flow is acyclic so we know we
     can merge them.  */
  if (outer)
    for (r1 = region; r1; r1 = next)
      {
        next = r1->next_peer;
	if (r1->type == ERT_CATCH)
	  continue;
        if (eh_region_replaceable_by_p (r1->outer, r1))
	  {
	    replace_region (r1->outer, r1);
	    merged = true;
	  }
	else
	  num_regions ++;
      }

  /* Get new first region and try to match the peers
     for equivalence.  */
  if (outer)
    region = outer->inner;
  else
    region = cfun->eh->region_tree;

  /* There are few regions to inspect:
     N^2 loop matching each region with each region
     will do the job well.  */
  if (num_regions < 10)
    {
      for (r1 = region; r1; r1 = r1->next_peer)
	{
	  if (r1->type == ERT_CATCH)
	    continue;
	  for (r2 = r1->next_peer; r2; r2 = next)
	    {
	      next = r2->next_peer;
	      if (eh_region_replaceable_by_p (r1, r2))
		{
		  replace_region (r1, r2);
		  merged = true;
		}
	    }
	}
    }
  /* Or use hashtable to avoid N^2 behaviour.  */
  else
    {
      htab_t hash;
      hash = htab_create (num_regions, hash_eh_region,
			  eh_regions_equal_p, NULL);
      for (r1 = region; r1; r1 = next)
	{
          void **slot;

	  next = r1->next_peer;
	  if (r1->type == ERT_CATCH)
	    continue;
	  slot = htab_find_slot (hash, r1, INSERT);
	  if (!*slot)
	    *slot = r1;
	  else
	    replace_region ((struct eh_region_d *) *slot, r1);
	}
      htab_delete (hash);
    }
  for (r1 = region; r1; r1 = r1->next_peer)
    merged |= merge_peers (r1->inner);
  return merged;
}

/* Remove all regions whose labels are not reachable.
   REACHABLE is bitmap of all regions that are used by the function
   CONTAINS_STMT is bitmap of all regions that contains stmt (or NULL). */

void
remove_unreachable_regions (sbitmap reachable, sbitmap contains_stmt)
{
  int i;
  struct eh_region_d *r;
  VEC(eh_region,heap) *must_not_throws = VEC_alloc (eh_region, heap, 16);
  struct eh_region_d *local_must_not_throw = NULL;
  struct eh_region_d *first_must_not_throw = NULL;

  for (i = cfun->eh->last_region_number; i > 0; --i)
    {
      r = VEC_index (eh_region, cfun->eh->region_array, i);
      if (!r || r->region_number != i)
	continue;
      if (!TEST_BIT (reachable, i) && !r->resume)
	{
	  bool kill_it = true;

	  r->tree_label = NULL;
	  switch (r->type)
	    {
	    case ERT_THROW:
	      /* Don't remove ERT_THROW regions if their outer region
	         is reachable.  */
	      if (r->outer && TEST_BIT (reachable, r->outer->region_number))
		kill_it = false;
	      break;
	    case ERT_MUST_NOT_THROW:
	      /* MUST_NOT_THROW regions are implementable solely in the
	         runtime, but we need them when inlining function.

	         Keep them if outer region is not MUST_NOT_THROW a well
	         and if they contain some statement that might unwind through
	         them.  */
	      if ((!r->outer || r->outer->type != ERT_MUST_NOT_THROW)
		  && (!contains_stmt
		      || can_be_reached_by_runtime (contains_stmt, r)))
		kill_it = false;
	      break;
	    case ERT_TRY:
	      {
		/* TRY regions are reachable if any of its CATCH regions
		   are reachable.  */
		struct eh_region_d *c;
		for (c = r->u.eh_try.eh_catch; c;
		     c = c->u.eh_catch.next_catch)
		  if (TEST_BIT (reachable, c->region_number))
		    {
		      kill_it = false;
		      break;
		    }
		break;
	      }

	    default:
	      break;
	    }

	  if (kill_it)
	    {
	      if (dump_file)
		fprintf (dump_file, "Removing unreachable eh region %i\n",
			 r->region_number);
	      remove_eh_handler (r);
	    }
	  else if (r->type == ERT_MUST_NOT_THROW)
	    {
	      if (!first_must_not_throw)
	        first_must_not_throw = r;
	      VEC_safe_push (eh_region, heap, must_not_throws, r);
	    }
	}
      else
	if (r->type == ERT_MUST_NOT_THROW)
	  {
	    if (!local_must_not_throw)
	      local_must_not_throw = r;
	    if (r->outer)
	      VEC_safe_push (eh_region, heap, must_not_throws, r);
	  }
    }

  /* MUST_NOT_THROW regions without local handler are all the same; they
     trigger terminate call in runtime.
     MUST_NOT_THROW handled locally can differ in debug info associated
     to std::terminate () call or if one is coming from Java and other
     from C++ whether they call terminate or abort.  

     We merge all MUST_NOT_THROW regions handled by the run-time into one.
     We alsobring all local MUST_NOT_THROW regions to the roots of EH tree
     (since unwinding never continues to the outer region anyway).
     If MUST_NOT_THROW with local handler is present in the tree, we use
     that region to merge into, since it will remain in tree anyway;
     otherwise we use first MUST_NOT_THROW.

     Merging of locally handled regions needs changes to the CFG.  Crossjumping
     should take care of this, by looking at the actual code and
     ensuring that the cleanup actions are really the same.  */

  if (local_must_not_throw)
    first_must_not_throw = local_must_not_throw;

  for (i = 0; VEC_iterate (eh_region, must_not_throws, i, r); i++)
    {
      if (!r->label && !r->tree_label && r != first_must_not_throw)
	{
	  if (dump_file)
	    fprintf (dump_file, "Replacing MUST_NOT_THROW region %i by %i\n",
		     r->region_number,
		     first_must_not_throw->region_number);
	  remove_eh_handler_and_replace (r, first_must_not_throw, false);
	  first_must_not_throw->may_contain_throw |= r->may_contain_throw;
	}
      else
	bring_to_root (r);
    }
  merge_peers (cfun->eh->region_tree);
#ifdef ENABLE_CHECKING
  verify_eh_tree (cfun);
#endif
  VEC_free (eh_region, heap, must_not_throws);
}

/* Return array mapping LABEL_DECL_UID to region such that region's tree_label
   is identical to label.  */

VEC (int, heap) *
label_to_region_map (void)
{
  VEC (int, heap) * label_to_region = NULL;
  int i;
  int idx;

  VEC_safe_grow_cleared (int, heap, label_to_region,
			 cfun->cfg->last_label_uid + 1);
  for (i = cfun->eh->last_region_number; i > 0; --i)
    {
      struct eh_region_d *r = VEC_index (eh_region, cfun->eh->region_array, i);
      if (r && r->region_number == i
	  && r->tree_label && LABEL_DECL_UID (r->tree_label) >= 0)
	{
	  if ((idx = VEC_index (int, label_to_region,
				LABEL_DECL_UID (r->tree_label))) != 0)
	      r->next_region_sharing_label =
	      VEC_index (eh_region, cfun->eh->region_array, idx);
	  else
	    r->next_region_sharing_label = NULL;
	  VEC_replace (int, label_to_region, LABEL_DECL_UID (r->tree_label),
		       i);
	}
    }
  return label_to_region;
}

/* Return number of EH regions.  */
int
num_eh_regions (void)
{
  return cfun->eh->last_region_number + 1;
}

/* Return next region sharing same label as REGION.  */

int
get_next_region_sharing_label (int region)
{
  struct eh_region_d *r;
  if (!region)
    return 0;
  r = VEC_index (eh_region, cfun->eh->region_array, region);
  if (!r || !r->next_region_sharing_label)
    return 0;
  return r->next_region_sharing_label->region_number;
}

/* Return bitmap of all labels that are handlers of must not throw regions.  */

bitmap
must_not_throw_labels (void)
{
  struct eh_region_d *i;
  bitmap labels = BITMAP_ALLOC (NULL);

  i = cfun->eh->region_tree;
  if (! i)
    return labels;

  while (1)
    {
      if (i->type == ERT_MUST_NOT_THROW && i->tree_label
          && LABEL_DECL_UID (i->tree_label) >= 0)
        bitmap_set_bit (labels, LABEL_DECL_UID (i->tree_label));

      /* If there are sub-regions, process them.  */
      if (i->inner)
	i = i->inner;
      /* If there are peers, process them.  */
      else if (i->next_peer)
	i = i->next_peer;
      /* Otherwise, step back up the tree to the next peer.  */
      else
	{
	  do {
	    i = i->outer;
	    if (i == NULL)
	      return labels;
	  } while (i->next_peer == NULL);
	  i = i->next_peer;
	}
    }
}

/* Set up EH labels for RTL.  */

void
convert_from_eh_region_ranges (void)
{
  int i, n = cfun->eh->last_region_number;

  /* Most of the work is already done at the tree level.  All we need to
     do is collect the rtl labels that correspond to the tree labels that
     collect the rtl labels that correspond to the tree labels
     we allocated earlier.  */
  for (i = 1; i <= n; ++i)
    {
      struct eh_region_d *region;

      region = VEC_index (eh_region, cfun->eh->region_array, i);
      if (region && region->tree_label)
	region->label = DECL_RTL_IF_SET (region->tree_label);
    }
}

void
find_exception_handler_labels (void)
{
  int i;

  if (cfun->eh->region_tree == NULL)
    return;

  for (i = cfun->eh->last_region_number; i > 0; --i)
    {
      struct eh_region_d *region;
      rtx lab;

      region = VEC_index (eh_region, cfun->eh->region_array, i);
      if (! region || region->region_number != i)
	continue;
      if (crtl->eh.built_landing_pads)
	lab = region->landing_pad;
      else
	lab = region->label;
    }
}

/* Returns true if the current function has exception handling regions.  */

bool
current_function_has_exception_handlers (void)
{
  int i;

  for (i = cfun->eh->last_region_number; i > 0; --i)
    {
      struct eh_region_d *region;

      region = VEC_index (eh_region, cfun->eh->region_array, i);
      if (region
	  && region->region_number == i
	  && region->type != ERT_THROW)
	return true;
    }

  return false;
}

/* A subroutine of duplicate_eh_regions.  Search the region tree under O
   for the minimum and maximum region numbers.  Update *MIN and *MAX.  */

static void
duplicate_eh_regions_0 (eh_region o, int *min, int *max)
{
  int i;

  if (o->aka)
    {
      i = bitmap_first_set_bit (o->aka);
      if (i < *min)
	*min = i;
      i = bitmap_last_set_bit (o->aka);
      if (i > *max)
	*max = i;
    }
  if (o->region_number < *min)
    *min = o->region_number;
  if (o->region_number > *max)
    *max = o->region_number;

  if (o->inner)
    {
      o = o->inner;
      duplicate_eh_regions_0 (o, min, max);
      while (o->next_peer)
	{
	  o = o->next_peer;
	  duplicate_eh_regions_0 (o, min, max);
	}
    }
}

/* A subroutine of duplicate_eh_regions.  Copy the region tree under OLD.
   Root it at OUTER, and apply EH_OFFSET to the region number.  Don't worry
   about the other internal pointers just yet, just the tree-like pointers.  */

static eh_region
duplicate_eh_regions_1 (eh_region old, eh_region outer, int eh_offset)
{
  eh_region ret, n;

  ret = n = GGC_NEW (struct eh_region_d);

  *n = *old;
  n->outer = outer;
  n->next_peer = NULL;
  if (old->aka)
    {
      unsigned i;
      bitmap_iterator bi;
      n->aka = BITMAP_GGC_ALLOC ();

      EXECUTE_IF_SET_IN_BITMAP (old->aka, 0, i, bi)
      {
	bitmap_set_bit (n->aka, i + eh_offset);
	VEC_replace (eh_region, cfun->eh->region_array, i + eh_offset, n);
      }
    }

  n->region_number += eh_offset;
  VEC_replace (eh_region, cfun->eh->region_array, n->region_number, n);

  if (old->inner)
    {
      old = old->inner;
      n = n->inner = duplicate_eh_regions_1 (old, ret, eh_offset);
      while (old->next_peer)
	{
	  old = old->next_peer;
	  n = n->next_peer = duplicate_eh_regions_1 (old, ret, eh_offset);
	}
    }

  return ret;
}

/* Look for first outer region of R (or R itself) that is
   TRY region. Return NULL if none.  */

static struct eh_region_d *
find_prev_try (struct eh_region_d * r)
{
  for (; r && r->type != ERT_TRY; r = r->outer)
    if (r->type == ERT_MUST_NOT_THROW
	|| (r->type == ERT_ALLOWED_EXCEPTIONS
	    && !r->u.allowed.type_list))
      {
	r = NULL;
	break;
      }
  return r;
}

/* Duplicate the EH regions of IFUN, rooted at COPY_REGION, into current
   function and root the tree below OUTER_REGION.  Remap labels using MAP
   callback.  The special case of COPY_REGION of 0 means all regions.  */

int
duplicate_eh_regions (struct function *ifun, duplicate_eh_regions_map map,
		      void *data, int copy_region, int outer_region)
{
  eh_region cur, outer, *splice;
  int i, min_region, max_region, eh_offset, cfun_last_region_number;
  int num_regions;

  if (!ifun->eh)
    return 0;
#ifdef ENABLE_CHECKING
  verify_eh_tree (ifun);
#endif

  /* Find the range of region numbers to be copied.  The interface we 
     provide here mandates a single offset to find new number from old,
     which means we must look at the numbers present, instead of the
     count or something else.  */
  if (copy_region > 0)
    {
      min_region = INT_MAX;
      max_region = 0;

      cur = VEC_index (eh_region, ifun->eh->region_array, copy_region);
      duplicate_eh_regions_0 (cur, &min_region, &max_region);
    }
  else
    {
      min_region = 1;
      max_region = ifun->eh->last_region_number;
    }
  num_regions = max_region - min_region + 1;
  cfun_last_region_number = cfun->eh->last_region_number;
  eh_offset = cfun_last_region_number + 1 - min_region;

  /* If we've not yet created a region array, do so now.  */
  cfun->eh->last_region_number = cfun_last_region_number + num_regions;
  VEC_safe_grow_cleared (eh_region, gc, cfun->eh->region_array,
			 cfun->eh->last_region_number + 1);

  /* Locate the spot at which to insert the new tree.  */
  if (outer_region > 0)
    {
      outer = VEC_index (eh_region, cfun->eh->region_array, outer_region);
      if (outer)
	splice = &outer->inner;
      else
	splice = &cfun->eh->region_tree;
    }
  else
    {
      outer = NULL;
      splice = &cfun->eh->region_tree;
    }
  while (*splice)
    splice = &(*splice)->next_peer;

  if (!ifun->eh->region_tree)
    {
      if (outer)
	for (i = cfun_last_region_number + 1;
	     i <= cfun->eh->last_region_number; i++)
	  {
	    VEC_replace (eh_region, cfun->eh->region_array, i, outer);
	    if (outer->aka == NULL)
	      outer->aka = BITMAP_GGC_ALLOC ();
	    bitmap_set_bit (outer->aka, i);
	  }
      return eh_offset;
    }

  /* Copy all the regions in the subtree.  */
  if (copy_region > 0)
    {
      cur = VEC_index (eh_region, ifun->eh->region_array, copy_region);
      *splice = duplicate_eh_regions_1 (cur, outer, eh_offset);
    }
  else
    {
      eh_region n;

      cur = ifun->eh->region_tree;
      *splice = n = duplicate_eh_regions_1 (cur, outer, eh_offset);
      while (cur->next_peer)
	{
	  cur = cur->next_peer;
	  n = n->next_peer = duplicate_eh_regions_1 (cur, outer, eh_offset);
	}
    }

  /* Remap all the labels in the new regions.  */
  for (i = cfun_last_region_number + 1;
       VEC_iterate (eh_region, cfun->eh->region_array, i, cur); ++i)
    if (cur && cur->tree_label)
      cur->tree_label = map (cur->tree_label, data);

  /* Remap all of the internal catch and cleanup linkages.  Since we 
     duplicate entire subtrees, all of the referenced regions will have
     been copied too.  And since we renumbered them as a block, a simple
     bit of arithmetic finds us the index for the replacement region.  */
  for (i = cfun_last_region_number + 1;
       VEC_iterate (eh_region, cfun->eh->region_array, i, cur); ++i)
    {
      /* All removed EH that is toplevel in input function is now
         in outer EH of output function.  */
      if (cur == NULL)
	{
	  gcc_assert (VEC_index
		      (eh_region, ifun->eh->region_array,
		       i - eh_offset) == NULL);
	  if (outer)
	    {
	      VEC_replace (eh_region, cfun->eh->region_array, i, outer);
	      if (outer->aka == NULL)
		outer->aka = BITMAP_GGC_ALLOC ();
	      bitmap_set_bit (outer->aka, i);
	    }
	  continue;
	}
      if (i != cur->region_number)
	continue;

#define REMAP(REG) \
	(REG) = VEC_index (eh_region, cfun->eh->region_array, \
			   (REG)->region_number + eh_offset)

      switch (cur->type)
	{
	case ERT_TRY:
	  if (cur->u.eh_try.eh_catch)
	    REMAP (cur->u.eh_try.eh_catch);
	  if (cur->u.eh_try.last_catch)
	    REMAP (cur->u.eh_try.last_catch);
	  break;

	case ERT_CATCH:
	  if (cur->u.eh_catch.next_catch)
	    REMAP (cur->u.eh_catch.next_catch);
	  if (cur->u.eh_catch.prev_catch)
	    REMAP (cur->u.eh_catch.prev_catch);
	  break;

	default:
	  break;
	}

#undef REMAP
    }
#ifdef ENABLE_CHECKING
  verify_eh_tree (cfun);
#endif

  return eh_offset;
}

/* Return new copy of eh region OLD inside region NEW_OUTER.
   Do not care about updating the tree otherwise.  */

static struct eh_region_d *
copy_eh_region_1 (struct eh_region_d *old, struct eh_region_d *new_outer)
{
  struct eh_region_d *new_eh = gen_eh_region (old->type, new_outer);
  new_eh->u = old->u;
  new_eh->tree_label = old->tree_label;
  new_eh->may_contain_throw = old->may_contain_throw;
  VEC_safe_grow (eh_region, gc, cfun->eh->region_array,
		 cfun->eh->last_region_number + 1);
  VEC_replace (eh_region, cfun->eh->region_array, new_eh->region_number, new_eh);
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Copying region %i to %i\n", old->region_number, new_eh->region_number);
  return new_eh;
}

/* Return new copy of eh region OLD inside region NEW_OUTER.  
  
   Copy whole catch-try chain if neccesary.  */

static struct eh_region_d *
copy_eh_region (struct eh_region_d *old, struct eh_region_d *new_outer)
{
  struct eh_region_d *r, *n, *old_try, *new_try, *ret = NULL;
  VEC(eh_region,heap) *catch_list = NULL;

  if (old->type != ERT_CATCH)
    {
      gcc_assert (old->type != ERT_TRY);
      r = copy_eh_region_1 (old, new_outer);
      return r;
    }

  /* Locate and copy corresponding TRY.  */
  for (old_try = old->next_peer; old_try->type == ERT_CATCH; old_try = old_try->next_peer)
    continue;
  gcc_assert (old_try->type == ERT_TRY);
  new_try = gen_eh_region_try (new_outer);
  new_try->tree_label = old_try->tree_label;
  new_try->may_contain_throw = old_try->may_contain_throw;
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Copying try-catch regions. Try: %i to %i\n",
    	     old_try->region_number, new_try->region_number);
  VEC_safe_grow (eh_region, gc, cfun->eh->region_array,
		 cfun->eh->last_region_number + 1);
  VEC_replace (eh_region, cfun->eh->region_array, new_try->region_number, new_try);

  /* In order to keep CATCH list in order, we need to copy in reverse order.  */
  for (r = old_try->u.eh_try.last_catch; r->type == ERT_CATCH; r = r->next_peer)
    VEC_safe_push (eh_region, heap, catch_list, r);

  while (VEC_length (eh_region, catch_list))
    {
      r = VEC_pop (eh_region, catch_list);

      /* Duplicate CATCH.  */
      n = gen_eh_region_catch (new_try, r->u.eh_catch.type_list);
      n->tree_label = r->tree_label;
      n->may_contain_throw = r->may_contain_throw;
      VEC_safe_grow (eh_region, gc, cfun->eh->region_array,
		     cfun->eh->last_region_number + 1);
      VEC_replace (eh_region, cfun->eh->region_array, n->region_number, n);
      n->tree_label = r->tree_label;

      if (dump_file && (dump_flags & TDF_DETAILS))
        fprintf (dump_file, "Copying try-catch regions. Catch: %i to %i\n",
	         r->region_number, n->region_number);
      if (r == old)
	ret = n;
    }
  VEC_free (eh_region, heap, catch_list);
  gcc_assert (ret);
  return ret;
}

/* Callback for forach_reachable_handler that push REGION into single VECtor DATA.  */

static void
push_reachable_handler (struct eh_region_d *region, void *data)
{
  VEC(eh_region,heap) **trace = (VEC(eh_region,heap) **) data;
  VEC_safe_push (eh_region, heap, *trace, region);
}

/* Redirect EH edge E that to NEW_DEST_LABEL.
   IS_RESX, INLINABLE_CALL and REGION_NMUBER match the parameter of
   foreach_reachable_handler.  */

struct eh_region_d *
redirect_eh_edge_to_label (edge e, tree new_dest_label, bool is_resx,
			   bool inlinable_call, int region_number)
{
  struct eh_region_d *outer;
  struct eh_region_d *region;
  VEC (eh_region, heap) * trace = NULL;
  int i;
  int start_here = -1;
  basic_block old_bb = e->dest;
  struct eh_region_d *old, *r = NULL;
  bool update_inplace = true;
  edge_iterator ei;
  edge e2;

  /* If there is only one EH edge, we don't need to duplicate;
     just update labels in the tree.  */
  FOR_EACH_EDGE (e2, ei, old_bb->preds)
    if ((e2->flags & EDGE_EH) && e2 != e)
      {
        update_inplace = false;
        break;
      }

  region = VEC_index (eh_region, cfun->eh->region_array, region_number);
  gcc_assert (region);

  foreach_reachable_handler (region_number, is_resx, inlinable_call,
			     push_reachable_handler, &trace);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      dump_eh_tree (dump_file, cfun);
      fprintf (dump_file, "Trace: ");
      for (i = 0; i < (int) VEC_length (eh_region, trace); i++)
	fprintf (dump_file, " %i", VEC_index (eh_region, trace, i)->region_number);
      fprintf (dump_file, " inplace: %i\n", update_inplace);
    }

  if (update_inplace)
    {
      /* In easy route just walk trace and update all occurences of the label.  */
      for (i = 0; i < (int) VEC_length (eh_region, trace); i++)
	{
	  r = VEC_index (eh_region, trace, i);
	  if (r->tree_label && label_to_block (r->tree_label) == old_bb)
	    {
	      r->tree_label = new_dest_label;
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "Updating label for region %i\n",
			 r->region_number);
	    }
	}
      r = region;
    }
  else
    {
      /* Now look for outermost handler that reffers to the basic block in question.
         We start our duplication there.  */
      for (i = 0; i < (int) VEC_length (eh_region, trace); i++)
	{
	  r = VEC_index (eh_region, trace, i);
	  if (r->tree_label && label_to_block (r->tree_label) == old_bb)
	    start_here = i;
	}
      outer = VEC_index (eh_region, trace, start_here)->outer;
      gcc_assert (start_here >= 0);

      /* And now do the dirty job!  */
      for (i = start_here; i >= 0; i--)
	{
	  old = VEC_index (eh_region, trace, i);
	  gcc_assert (!outer || old->outer != outer->outer);

	  /* Copy region and update label.  */
	  r = copy_eh_region (old, outer);
	  VEC_replace (eh_region, trace, i, r);
	  if (r->tree_label && label_to_block (r->tree_label) == old_bb)
	    {
	      r->tree_label = new_dest_label;
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "Updating label for region %i\n",
			 r->region_number);
	    }

	  /* We got into copying CATCH.  copy_eh_region already did job
	     of copying all catch blocks corresponding to the try.  Now
	     we need to update labels in all of them and see trace.

	     We continue nesting into TRY region corresponding to CATCH:
	     When duplicating EH tree contaiing subregions of CATCH,
	     the CATCH region itself is never inserted to trace so we
	     never get here anyway.  */
	  if (r->type == ERT_CATCH)
	    {
	      /* Walk other catch regions we copied and update labels as needed.  */
	      for (r = r->next_peer; r->type == ERT_CATCH; r = r->next_peer)
		if (r->tree_label && label_to_block (r->tree_label) == old_bb)
		  {
		    r->tree_label = new_dest_label;
		    if (dump_file && (dump_flags & TDF_DETAILS))
		      fprintf (dump_file, "Updating label for region %i\n",
			       r->region_number);
		  }
	       gcc_assert (r->type == ERT_TRY);

	       /* Skip sibling catch regions from the trace.
		  They are already updated.  */
	       while (i > 0 && VEC_index (eh_region, trace, i - 1)->outer == old->outer)
		 {
		   gcc_assert (VEC_index (eh_region, trace, i - 1)->type == ERT_CATCH);
		   i--;
		 }
	     }

	  outer = r;
	}
        
      if (is_resx || region->type == ERT_THROW)
	r = copy_eh_region (region, outer);
    }

  VEC_free (eh_region, heap, trace);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      dump_eh_tree (dump_file, cfun);
      fprintf (dump_file, "New region: %i\n", r->region_number);
    }
  return r;
}

/* Return region number of region that is outer to both if REGION_A and
   REGION_B in IFUN.  */

int
eh_region_outermost (struct function *ifun, int region_a, int region_b)
{
  struct eh_region_d *rp_a, *rp_b;
  sbitmap b_outer;

  gcc_assert (ifun->eh->last_region_number > 0);
  gcc_assert (ifun->eh->region_tree);

  rp_a = VEC_index (eh_region, ifun->eh->region_array, region_a);
  rp_b = VEC_index (eh_region, ifun->eh->region_array, region_b);
  gcc_assert (rp_a != NULL);
  gcc_assert (rp_b != NULL);

  b_outer = sbitmap_alloc (ifun->eh->last_region_number + 1);
  sbitmap_zero (b_outer);

  do
    {
      SET_BIT (b_outer, rp_b->region_number);
      rp_b = rp_b->outer;
    }
  while (rp_b);

  do
    {
      if (TEST_BIT (b_outer, rp_a->region_number))
	{
	  sbitmap_free (b_outer);
	  return rp_a->region_number;
	}
      rp_a = rp_a->outer;
    }
  while (rp_a);

  sbitmap_free (b_outer);
  return -1;
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

  slot = (tree *) htab_find_slot_with_hash (type_to_runtime_map, type,
					    TREE_HASH (type), INSERT);
  if (*slot == NULL)
    {
      tree runtime = (*lang_eh_runtime_type) (type);
      *slot = tree_cons (type, runtime, NULL_TREE);
    }
}

tree
lookup_type_for_runtime (tree type)
{
  tree *slot;

  slot = (tree *) htab_find_slot_with_hash (type_to_runtime_map, type,
					    TREE_HASH (type), NO_INSERT);

  /* We should have always inserted the data earlier.  */
  return TREE_VALUE (*slot);
}


/* Represent an entry in @TTypes for either catch actions
   or exception filter actions.  */
struct GTY(()) ttypes_filter {
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

/* Add TYPE (which may be NULL) to crtl->eh.ttype_data, using TYPES_HASH
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
      n->filter = VEC_length (tree, crtl->eh.ttype_data) + 1;
      *slot = n;

      VEC_safe_push (tree, gc, crtl->eh.ttype_data, type);
    }

  return n->filter;
}

/* Add LIST to crtl->eh.ehspec_data, using EHSPEC_HASH and TYPES_HASH
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
      /* Filter value is a -1 based byte index into a uleb128 buffer.  */

      n = XNEW (struct ttypes_filter);
      n->t = list;
      n->filter = -(VARRAY_ACTIVE_SIZE (crtl->eh.ehspec_data) + 1);
      *slot = n;

      /* Generate a 0 terminated list of filter values.  */
      for (; list ; list = TREE_CHAIN (list))
	{
	  if (targetm.arm_eabi_unwinder)
	    VARRAY_PUSH_TREE (crtl->eh.ehspec_data, TREE_VALUE (list));
	  else
	    {
	      /* Look up each type in the list and encode its filter
		 value as a uleb128.  */
	      push_uleb128 (&crtl->eh.ehspec_data,
		  add_ttypes_entry (ttypes_hash, TREE_VALUE (list)));
	    }
	}
      if (targetm.arm_eabi_unwinder)
	VARRAY_PUSH_TREE (crtl->eh.ehspec_data, NULL_TREE);
      else
	VARRAY_PUSH_UCHAR (crtl->eh.ehspec_data, 0);
    }

  return n->filter;
}

/* Generate the action filter values to be used for CATCH and
   ALLOWED_EXCEPTIONS regions.  When using dwarf2 exception regions,
   we use lots of landing pads, and so every type or list can share
   the same filter value, which saves table space.  */

static void
assign_filter_values (void)
{
  int i;
  htab_t ttypes, ehspec;

  crtl->eh.ttype_data = VEC_alloc (tree, gc, 16);
  if (targetm.arm_eabi_unwinder)
    VARRAY_TREE_INIT (crtl->eh.ehspec_data, 64, "ehspec_data");
  else
    VARRAY_UCHAR_INIT (crtl->eh.ehspec_data, 64, "ehspec_data");

  ttypes = htab_create (31, ttypes_filter_hash, ttypes_filter_eq, free);
  ehspec = htab_create (31, ehspec_filter_hash, ehspec_filter_eq, free);

  for (i = cfun->eh->last_region_number; i > 0; --i)
    {
      struct eh_region_d *r;

      r = VEC_index (eh_region, cfun->eh->region_array, i);

      /* Mind we don't process a region more than once.  */
      if (!r || r->region_number != i)
	continue;

      switch (r->type)
	{
	case ERT_CATCH:
	  /* Whatever type_list is (NULL or true list), we build a list
	     of filters for the region.  */
	  r->u.eh_catch.filter_list = NULL_TREE;

	  if (r->u.eh_catch.type_list != NULL)
	    {
	      /* Get a filter value for each of the types caught and store
		 them in the region's dedicated list.  */
	      tree tp_node = r->u.eh_catch.type_list;

	      for (;tp_node; tp_node = TREE_CHAIN (tp_node))
		{
		  int flt = add_ttypes_entry (ttypes, TREE_VALUE (tp_node));
		  tree flt_node = build_int_cst (NULL_TREE, flt);

		  r->u.eh_catch.filter_list
		    = tree_cons (NULL_TREE, flt_node, r->u.eh_catch.filter_list);
		}
	    }
	  else
	    {
	      /* Get a filter value for the NULL list also since it will need
		 an action record anyway.  */
	      int flt = add_ttypes_entry (ttypes, NULL);
	      tree flt_node = build_int_cst (NULL_TREE, flt);

	      r->u.eh_catch.filter_list
		= tree_cons (NULL_TREE, flt_node, r->u.eh_catch.filter_list);
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
  basic_block bb;
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
  bb = create_basic_block (seq, last, BLOCK_FOR_INSN (insn)->prev_bb);
  update_bb_for_insn (bb);
  bb->flags |= BB_SUPERBLOCK;
  return bb;
}

/* Generate the code to actually handle exceptions, which will follow the
   landing pads.  */

static void
build_post_landing_pads (void)
{
  int i;

  for (i = cfun->eh->last_region_number; i > 0; --i)
    {
      struct eh_region_d *region;
      rtx seq;

      region = VEC_index (eh_region, cfun->eh->region_array, i);
      /* Mind we don't process a region more than once.  */
      if (!region || region->region_number != i)
	continue;

      switch (region->type)
	{
	case ERT_TRY:
	  /* It is possible that TRY region is kept alive only because some of
	     contained catch region still have RESX instruction but they are
	     reached via their copies.  In this case we need to do nothing.  */
	  if (!region->u.eh_try.eh_catch->label)
	    break;

	  /* ??? Collect the set of all non-overlapping catch handlers
	       all the way up the chain until blocked by a cleanup.  */
	  /* ??? Outer try regions can share landing pads with inner
	     try regions if the types are completely non-overlapping,
	     and there are no intervening cleanups.  */

	  region->post_landing_pad = gen_label_rtx ();

	  start_sequence ();

	  emit_label (region->post_landing_pad);

	  /* ??? It is mighty inconvenient to call back into the
	     switch statement generation code in expand_end_case.
	     Rapid prototyping sez a sequence of ifs.  */
	  {
	    struct eh_region_d *c;
	    for (c = region->u.eh_try.eh_catch; c ; c = c->u.eh_catch.next_catch)
	      {
		if (c->u.eh_catch.type_list == NULL)
		  emit_jump (c->label);
		else
		  {
		    /* Need for one cmp/jump per type caught. Each type
		       list entry has a matching entry in the filter list
		       (see assign_filter_values).  */
		    tree tp_node = c->u.eh_catch.type_list;
		    tree flt_node = c->u.eh_catch.filter_list;

		    for (; tp_node; )
		      {
			emit_cmp_and_jump_insns
			  (crtl->eh.filter,
			   GEN_INT (tree_low_cst (TREE_VALUE (flt_node), 0)),
			   EQ, NULL_RTX,
			   targetm.eh_return_filter_mode (), 0, c->label);

			tp_node = TREE_CHAIN (tp_node);
			flt_node = TREE_CHAIN (flt_node);
		      }
		  }
	      }
	  }

	  /* We delay the generation of the _Unwind_Resume until we generate
	     landing pads.  We emit a marker here so as to get good control
	     flow data in the meantime.  */
	  gcc_assert (!region->resume);
	  region->resume
	    = emit_jump_insn (gen_rtx_RESX (VOIDmode, region->region_number));
	  emit_barrier ();

	  seq = get_insns ();
	  end_sequence ();

	  emit_to_new_bb_before (seq, region->u.eh_try.eh_catch->label);

	  break;

	case ERT_ALLOWED_EXCEPTIONS:
	  if (!region->label)
	    break;
	  region->post_landing_pad = gen_label_rtx ();

	  start_sequence ();

	  emit_label (region->post_landing_pad);

	  emit_cmp_and_jump_insns (crtl->eh.filter,
				   GEN_INT (region->u.allowed.filter),
				   EQ, NULL_RTX,
				   targetm.eh_return_filter_mode (), 0, region->label);

	  /* We delay the generation of the _Unwind_Resume until we generate
	     landing pads.  We emit a marker here so as to get good control
	     flow data in the meantime.  */
	  gcc_assert (!region->resume);
	  region->resume
	    = emit_jump_insn (gen_rtx_RESX (VOIDmode, region->region_number));
	  emit_barrier ();

	  seq = get_insns ();
	  end_sequence ();

	  emit_to_new_bb_before (seq, region->label);
	  break;

	case ERT_CLEANUP:
	case ERT_MUST_NOT_THROW:
	  region->post_landing_pad = region->label;
	  break;

	case ERT_CATCH:
	case ERT_THROW:
	  /* Nothing to do.  */
	  break;

	default:
	  gcc_unreachable ();
	}
    }
}

/* Replace RESX patterns with jumps to the next handler if any, or calls to
   _Unwind_Resume otherwise.  */

static void
connect_post_landing_pads (void)
{
  int i;

  for (i = cfun->eh->last_region_number; i > 0; --i)
    {
      struct eh_region_d *region;
      struct eh_region_d *outer;
      rtx seq;
      rtx barrier;
      rtx resume_list;

      region = VEC_index (eh_region, cfun->eh->region_array, i);
      /* Mind we don't process a region more than once.  */
      if (!region || region->region_number != i)
	continue;

      /* If there is no RESX, or it has been deleted by flow, there's
	 nothing to fix up.  */
      if (! region->resume)
	continue;

      /* Search for another landing pad in this function.  */
      for (outer = region->outer; outer ; outer = outer->outer)
	if (outer->post_landing_pad)
	  break;

      for (resume_list = region->resume; resume_list;
      	   resume_list = (GET_CODE (resume_list) == INSN_LIST
			  ? XEXP (resume_list, 1) : NULL_RTX))
	{
	  rtx resume = (GET_CODE (resume_list) == INSN_LIST
			? XEXP (resume_list, 0) : resume_list);
          if (INSN_DELETED_P (resume))
	    continue;
	  start_sequence ();

	  if (outer)
	    {
	      edge e;
	      basic_block src, dest;

	      emit_jump (outer->post_landing_pad);
	      src = BLOCK_FOR_INSN (resume);
	      dest = BLOCK_FOR_INSN (outer->post_landing_pad);
	      while (EDGE_COUNT (src->succs) > 0)
		remove_edge (EDGE_SUCC (src, 0));
	      e = make_edge (src, dest, 0);
	      e->probability = REG_BR_PROB_BASE;
	      e->count = src->count;
	    }
	  else
	    {
	      emit_library_call (unwind_resume_libfunc, LCT_THROW,
				 VOIDmode, 1, crtl->eh.exc_ptr, ptr_mode);

	      /* What we just emitted was a throwing libcall, so it got a
		 barrier automatically added after it.  If the last insn in
		 the libcall sequence isn't the barrier, it's because the
		 target emits multiple insns for a call, and there are insns
		 after the actual call insn (which are redundant and would be
		 optimized away).  The barrier is inserted exactly after the
		 call insn, so let's go get that and delete the insns after
		 it, because below we need the barrier to be the last insn in
		 the sequence.  */
	      delete_insns_since (NEXT_INSN (last_call_insn ()));
	    }

	  seq = get_insns ();
	  end_sequence ();
	  barrier = emit_insn_before (seq, resume);
	  /* Avoid duplicate barrier.  */
	  gcc_assert (BARRIER_P (barrier));
	  delete_insn (barrier);
	  delete_insn (resume);
	}

      /* ??? From tree-ssa we can wind up with catch regions whose
	 label is not instantiated, but whose resx is present.  Now
	 that we've dealt with the resx, kill the region.  */
      if (region->label == NULL && region->type == ERT_CLEANUP)
	remove_eh_handler (region);
    }
}


static void
dw2_build_landing_pads (void)
{
  int i;

  for (i = cfun->eh->last_region_number; i > 0; --i)
    {
      struct eh_region_d *region;
      rtx seq;
      basic_block bb;
      edge e;

      region = VEC_index (eh_region, cfun->eh->region_array, i);
      /* Mind we don't process a region more than once.  */
      if (!region || region->region_number != i)
	continue;

      if (region->type != ERT_CLEANUP
	  && region->type != ERT_TRY
	  && region->type != ERT_ALLOWED_EXCEPTIONS)
	continue;

      if (!region->post_landing_pad)
	continue;

      start_sequence ();

      region->landing_pad = gen_label_rtx ();
      emit_label (region->landing_pad);

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

      emit_move_insn (crtl->eh.exc_ptr,
		      gen_rtx_REG (ptr_mode, EH_RETURN_DATA_REGNO (0)));
      emit_move_insn (crtl->eh.filter,
		      gen_rtx_REG (targetm.eh_return_filter_mode (),
				   EH_RETURN_DATA_REGNO (1)));

      seq = get_insns ();
      end_sequence ();

      bb = emit_to_new_bb_before (seq, region->post_landing_pad);
      e = make_edge (bb, bb->next_bb, EDGE_FALLTHRU);
      e->count = bb->count;
      e->probability = REG_BR_PROB_BASE;
    }
}


struct sjlj_lp_info
{
  int directly_reachable;
  int action_index;
  int dispatch_index;
  int call_site_index;
};

static bool
sjlj_find_directly_reachable_regions (struct sjlj_lp_info *lp_info)
{
  rtx insn;
  bool found_one = false;

  for (insn = get_insns (); insn ; insn = NEXT_INSN (insn))
    {
      struct eh_region_d *region;
      enum reachable_code rc;
      tree type_thrown;
      rtx note;

      if (! INSN_P (insn))
	continue;

      note = find_reg_note (insn, REG_EH_REGION, NULL_RTX);
      if (!note || INTVAL (XEXP (note, 0)) <= 0)
	continue;

      region = VEC_index (eh_region, cfun->eh->region_array, INTVAL (XEXP (note, 0)));
      if (!region)
	continue;

      type_thrown = NULL_TREE;
      if (region->type == ERT_THROW)
	{
	  type_thrown = region->u.eh_throw.type;
	  region = region->outer;
	}

      /* Find the first containing region that might handle the exception.
	 That's the landing pad to which we will transfer control.  */
      rc = RNL_NOT_CAUGHT;
      for (; region; region = region->outer)
	{
	  rc = reachable_next_level (region, type_thrown, NULL, false);
	  if (rc != RNL_NOT_CAUGHT)
	    break;
	}
      if (rc == RNL_MAYBE_CAUGHT || rc == RNL_CAUGHT)
	{
	  lp_info[region->region_number].directly_reachable = 1;
	  found_one = true;
	}
    }

  return found_one;
}

static void
sjlj_assign_call_site_values (rtx dispatch_label, struct sjlj_lp_info *lp_info)
{
  htab_t ar_hash;
  int i, index;

  /* First task: build the action table.  */

  VARRAY_UCHAR_INIT (crtl->eh.action_record_data, 64, "action_record_data");
  ar_hash = htab_create (31, action_record_hash, action_record_eq, free);

  for (i = cfun->eh->last_region_number; i > 0; --i)
    if (lp_info[i].directly_reachable)
      {
	struct eh_region_d *r =
	  VEC_index (eh_region, cfun->eh->region_array, i);

	r->landing_pad = dispatch_label;
	lp_info[i].action_index = collect_one_action_chain (ar_hash, r);
	if (lp_info[i].action_index != -1)
	  crtl->uses_eh_lsda = 1;
      }

  htab_delete (ar_hash);

  /* Next: assign dispatch values.  In dwarf2 terms, this would be the
     landing pad label for the region.  For sjlj though, there is one
     common landing pad from which we dispatch to the post-landing pads.

     A region receives a dispatch index if it is directly reachable
     and requires in-function processing.  Regions that share post-landing
     pads may share dispatch indices.  */
  /* ??? Post-landing pad sharing doesn't actually happen at the moment
     (see build_post_landing_pads) so we don't bother checking for it.  */

  index = 0;
  for (i = cfun->eh->last_region_number; i > 0; --i)
    if (lp_info[i].directly_reachable)
      lp_info[i].dispatch_index = index++;

  /* Finally: assign call-site values.  If dwarf2 terms, this would be
     the region number assigned by convert_to_eh_region_ranges, but
     handles no-action and must-not-throw differently.  */

  call_site_base = 1;
  for (i = cfun->eh->last_region_number; i > 0; --i)
    if (lp_info[i].directly_reachable)
      {
	int action = lp_info[i].action_index;

	/* Map must-not-throw to otherwise unused call-site index 0.  */
	if (action == -2)
	  index = 0;
	/* Map no-action to otherwise unused call-site index -1.  */
	else if (action == -1)
	  index = -1;
	/* Otherwise, look it up in the table.  */
	else
	  index = add_call_site (GEN_INT (lp_info[i].dispatch_index), action);

	lp_info[i].call_site_index = index;
      }
}

static void
sjlj_mark_call_sites (struct sjlj_lp_info *lp_info)
{
  int last_call_site = -2;
  rtx insn, mem;

  for (insn = get_insns (); insn ; insn = NEXT_INSN (insn))
    {
      struct eh_region_d *region;
      int this_call_site;
      rtx note, before, p;

      /* Reset value tracking at extended basic block boundaries.  */
      if (LABEL_P (insn))
	last_call_site = -2;

      if (! INSN_P (insn))
	continue;

      note = find_reg_note (insn, REG_EH_REGION, NULL_RTX);

      /* Calls that are known to not throw need not be marked.  */
      if (note && INTVAL (XEXP (note, 0)) <= 0)
	continue;

      if (note)
	region = VEC_index (eh_region, cfun->eh->region_array, INTVAL (XEXP (note, 0)));
      else
        region = NULL;

      if (!region)
	{
	  /* Calls (and trapping insns) without notes are outside any
	     exception handling region in this function.  Mark them as
	     no action.  */
	  if (CALL_P (insn)
	      || (flag_non_call_exceptions
		  && may_trap_p (PATTERN (insn))))
	    this_call_site = -1;
	  else
	    continue;
	}
      else
	this_call_site = lp_info[region->region_number].call_site_index;

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

  fc = crtl->eh.sjlj_fc;

  start_sequence ();

  /* We're storing this libcall's address into memory instead of
     calling it directly.  Thus, we must call assemble_external_libcall
     here, as we can not depend on emit_library_call to do it for us.  */
  assemble_external_libcall (eh_personality_libfunc);
  mem = adjust_address (fc, Pmode, sjlj_fc_personality_ofs);
  emit_move_insn (mem, eh_personality_libfunc);

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

#ifdef DONT_USE_BUILTIN_SETJMP
  {
    rtx x;
    x = emit_library_call_value (setjmp_libfunc, NULL_RTX, LCT_RETURNS_TWICE,
				 TYPE_MODE (integer_type_node), 1,
				 plus_constant (XEXP (fc, 0),
						sjlj_fc_jbuf_ofs), Pmode);

    emit_cmp_and_jump_insns (x, const0_rtx, NE, 0,
			     TYPE_MODE (integer_type_node), 0, dispatch_label);
    add_reg_br_prob_note (get_insns (), REG_BR_PROB_BASE/100);
  }
#else
  expand_builtin_setjmp_setup (plus_constant (XEXP (fc, 0), sjlj_fc_jbuf_ofs),
			       dispatch_label);
#endif

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
sjlj_emit_dispatch_table (rtx dispatch_label, struct sjlj_lp_info *lp_info)
{
  enum machine_mode unwind_word_mode = targetm.unwind_word_mode ();
  enum machine_mode filter_mode = targetm.eh_return_filter_mode ();
  int i, first_reachable;
  rtx mem, dispatch, seq, fc;
  rtx before;
  basic_block bb;
  edge e;

  fc = crtl->eh.sjlj_fc;

  start_sequence ();

  emit_label (dispatch_label);

#ifndef DONT_USE_BUILTIN_SETJMP
  expand_builtin_setjmp_receiver (dispatch_label);
#endif

  /* Load up dispatch index, exc_ptr and filter values from the
     function context.  */
  mem = adjust_address (fc, TYPE_MODE (integer_type_node),
			sjlj_fc_call_site_ofs);
  dispatch = copy_to_reg (mem);

  mem = adjust_address (fc, unwind_word_mode, sjlj_fc_data_ofs);
  if (unwind_word_mode != ptr_mode)
    {
#ifdef POINTERS_EXTEND_UNSIGNED
      mem = convert_memory_address (ptr_mode, mem);
#else
      mem = convert_to_mode (ptr_mode, mem, 0);
#endif
    }
  emit_move_insn (crtl->eh.exc_ptr, mem);

  mem = adjust_address (fc, unwind_word_mode,
			sjlj_fc_data_ofs + GET_MODE_SIZE (unwind_word_mode));
  if (unwind_word_mode != filter_mode)
    mem = convert_to_mode (filter_mode, mem, 0);
  emit_move_insn (crtl->eh.filter, mem);

  /* Jump to one of the directly reachable regions.  */
  /* ??? This really ought to be using a switch statement.  */

  first_reachable = 0;
  for (i = cfun->eh->last_region_number; i > 0; --i)
    {
      if (! lp_info[i].directly_reachable)
	continue;

      if (! first_reachable)
	{
	  first_reachable = i;
	  continue;
	}

      emit_cmp_and_jump_insns (dispatch, GEN_INT (lp_info[i].dispatch_index),
			       EQ, NULL_RTX, TYPE_MODE (integer_type_node), 0,
	                       (((struct eh_region_d *)
				 VEC_index (eh_region,
					    cfun->eh->region_array, i))
				->post_landing_pad));
    }

  seq = get_insns ();
  end_sequence ();

  before = (((struct eh_region_d *)
	     VEC_index (eh_region, cfun->eh->region_array, first_reachable))
	    ->post_landing_pad);

  bb = emit_to_new_bb_before (seq, before);
  e = make_edge (bb, bb->next_bb, EDGE_FALLTHRU);
  e->count = bb->count;
  e->probability = REG_BR_PROB_BASE;
}

static void
sjlj_build_landing_pads (void)
{
  struct sjlj_lp_info *lp_info;

  lp_info = XCNEWVEC (struct sjlj_lp_info, cfun->eh->last_region_number + 1);

  if (sjlj_find_directly_reachable_regions (lp_info))
    {
      rtx dispatch_label = gen_label_rtx ();
      int align = STACK_SLOT_ALIGNMENT (sjlj_fc_type_node,
					TYPE_MODE (sjlj_fc_type_node),
					TYPE_ALIGN (sjlj_fc_type_node));
      crtl->eh.sjlj_fc
	= assign_stack_local (TYPE_MODE (sjlj_fc_type_node),
			      int_size_in_bytes (sjlj_fc_type_node),
			      align);

      sjlj_assign_call_site_values (dispatch_label, lp_info);
      sjlj_mark_call_sites (lp_info);

      sjlj_emit_function_enter (dispatch_label);
      sjlj_emit_dispatch_table (dispatch_label, lp_info);
      sjlj_emit_function_exit ();
    }

  free (lp_info);
}

/* After initial rtl generation, call back to finish generating
   exception support code.  */

static void
finish_eh_generation (void)
{
  basic_block bb;

  /* Nothing to do if no regions created.  */
  if (cfun->eh->region_tree == NULL)
    return;

  /* The object here is to provide detailed information (via
     reachable_handlers) on how exception control flows within the
     function for the CFG construction.  In this first pass, we can
     include type information garnered from ERT_THROW and
     ERT_ALLOWED_EXCEPTIONS regions, and hope that it will be useful
     in deleting unreachable handlers.  Subsequently, we will generate
     landing pads which will connect many of the handlers, and then
     type information will not be effective.  Still, this is a win
     over previous implementations.  */

  /* These registers are used by the landing pads.  Make sure they
     have been generated.  */
  get_exception_pointer ();
  get_exception_filter ();

  /* Construct the landing pads.  */

  assign_filter_values ();
  build_post_landing_pads ();
  connect_post_landing_pads ();
  if (USING_SJLJ_EXCEPTIONS)
    sjlj_build_landing_pads ();
  else
    dw2_build_landing_pads ();

  crtl->eh.built_landing_pads = 1;

  /* We've totally changed the CFG.  Start over.  */
  find_exception_handler_labels ();
  break_superblocks ();
  if (USING_SJLJ_EXCEPTIONS
      /* Kludge for Alpha/Tru64 (see alpha_gp_save_rtx).  */
      || single_succ_edge (ENTRY_BLOCK_PTR)->insns.r)
    commit_edge_insertions ();
  FOR_EACH_BB (bb)
    {
      edge e;
      edge_iterator ei;
      bool eh = false;
      for (ei = ei_start (bb->succs); (e = ei_safe_edge (ei)); )
	{
	  if (e->flags & EDGE_EH)
	    {
	      remove_edge (e);
	      eh = true;
	    }
	  else
	    ei_next (&ei);
	}
      if (eh)
	rtl_make_eh_edge (NULL, bb, BB_END (bb));
    }
}

/* This section handles removing dead code for flow.  */

/* Splice REGION from the region tree and replace it by REPLACE etc.
   When UPDATE_CATCH_TRY is true mind updating links from catch to try
   region.*/

static void
remove_eh_handler_and_replace (struct eh_region_d *region,
			       struct eh_region_d *replace,
			       bool update_catch_try)
{
  struct eh_region_d **pp, **pp_start, *p, *outer, *inner;
  rtx lab;

  outer = region->outer;

  /* For the benefit of efficiently handling REG_EH_REGION notes,
     replace this region in the region array with its containing
     region.  Note that previous region deletions may result in
     multiple copies of this region in the array, so we have a
     list of alternate numbers by which we are known.  */

  VEC_replace (eh_region, cfun->eh->region_array, region->region_number,
	       replace);
  if (region->aka)
    {
      unsigned i;
      bitmap_iterator bi;

      EXECUTE_IF_SET_IN_BITMAP (region->aka, 0, i, bi)
	{
          VEC_replace (eh_region, cfun->eh->region_array, i, replace);
	}
    }

  if (replace)
    {
      if (!replace->aka)
        replace->aka = BITMAP_GGC_ALLOC ();
      if (region->aka)
	bitmap_ior_into (replace->aka, region->aka);
      bitmap_set_bit (replace->aka, region->region_number);
    }

  if (crtl->eh.built_landing_pads)
    lab = region->landing_pad;
  else
    lab = region->label;
  if (outer)
    pp_start = &outer->inner;
  else
    pp_start = &cfun->eh->region_tree;
  for (pp = pp_start, p = *pp; p != region; pp = &p->next_peer, p = *pp)
    continue;
  *pp = region->next_peer;

  if (replace)
    pp_start = &replace->inner;
  else
    pp_start = &cfun->eh->region_tree;
  inner = region->inner;
  if (inner)
    {
      for (p = inner; p->next_peer ; p = p->next_peer)
	p->outer = replace;
      p->outer = replace;

      p->next_peer = *pp_start;
      *pp_start = inner;
    }

  if (region->type == ERT_CATCH
      && update_catch_try)
    {
      struct eh_region_d *eh_try, *next, *prev;

      for (eh_try = region->next_peer;
	   eh_try->type == ERT_CATCH;
	   eh_try = eh_try->next_peer)
	continue;
      gcc_assert (eh_try->type == ERT_TRY);

      next = region->u.eh_catch.next_catch;
      prev = region->u.eh_catch.prev_catch;

      if (next)
	next->u.eh_catch.prev_catch = prev;
      else
	eh_try->u.eh_try.last_catch = prev;
      if (prev)
	prev->u.eh_catch.next_catch = next;
      else
	{
	  eh_try->u.eh_try.eh_catch = next;
	  if (! next)
	    remove_eh_handler (eh_try);
	}
    }
}

/* Splice REGION from the region tree and replace it by the outer region
   etc.  */

static void
remove_eh_handler (struct eh_region_d *region)
{
  remove_eh_handler_and_replace (region, region->outer, true);
}

/* Remove Eh region R that has turned out to have no code in its handler.  */

void
remove_eh_region (int r)
{
  struct eh_region_d *region;

  region = VEC_index (eh_region, cfun->eh->region_array, r);
  remove_eh_handler (region);
}

/* Remove Eh region R that has turned out to have no code in its handler
   and replace in by R2.  */

void
remove_eh_region_and_replace_by_outer_of (int r, int r2)
{
  struct eh_region_d *region, *region2;

  region = VEC_index (eh_region, cfun->eh->region_array, r);
  region2 = VEC_index (eh_region, cfun->eh->region_array, r2);
  remove_eh_handler_and_replace (region, region2->outer, true);
}

/* Invokes CALLBACK for every exception handler label.  Only used by old
   loop hackery; should not be used by new code.  */

void
for_each_eh_label (void (*callback) (rtx))
{
  int i;
  for (i = 0; i < cfun->eh->last_region_number; i++)
    {
      struct eh_region_d *r = VEC_index (eh_region, cfun->eh->region_array, i);
      if (r && r->region_number == i && r->label
          && LABEL_P (r->label))
	(*callback) (r->label);
    }
}

/* Invoke CALLBACK for every exception region in the current function.  */

void
for_each_eh_region (void (*callback) (struct eh_region_d *))
{
  int i, n = cfun->eh->last_region_number;
  for (i = 1; i <= n; ++i)
    {
      struct eh_region_d *region;

      region = VEC_index (eh_region, cfun->eh->region_array, i);
      if (region)
	(*callback) (region);
    }
}

/* This section describes CFG exception edges for flow.  */

/* For communicating between calls to reachable_next_level.  */
struct reachable_info
{
  tree types_caught;
  tree types_allowed;
  void (*callback) (struct eh_region_d *, void *);
  void *callback_data;
};

/* A subroutine of reachable_next_level.  Return true if TYPE, or a
   base class of TYPE, is in HANDLED.  */

static int
check_handled (tree handled, tree type)
{
  tree t;

  /* We can check for exact matches without front-end help.  */
  if (! lang_eh_type_covers)
    {
      for (t = handled; t ; t = TREE_CHAIN (t))
	if (TREE_VALUE (t) == type)
	  return 1;
    }
  else
    {
      for (t = handled; t ; t = TREE_CHAIN (t))
	if ((*lang_eh_type_covers) (TREE_VALUE (t), type))
	  return 1;
    }

  return 0;
}

/* A subroutine of reachable_next_level.  If we are collecting a list
   of handlers, add one.  After landing pad generation, reference
   it instead of the handlers themselves.  Further, the handlers are
   all wired together, so by referencing one, we've got them all.
   Before landing pad generation we reference each handler individually.

   LP_REGION contains the landing pad; REGION is the handler.  */

static void
add_reachable_handler (struct reachable_info *info,
		       struct eh_region_d *lp_region,
		       struct eh_region_d *region)
{
  if (! info)
    return;

  if (crtl->eh.built_landing_pads)
    info->callback (lp_region, info->callback_data);
  else
    info->callback (region, info->callback_data);
}

/* Process one level of exception regions for reachability.
   If TYPE_THROWN is non-null, then it is the *exact* type being
   propagated.  If INFO is non-null, then collect handler labels
   and caught/allowed type information between invocations.  */

static enum reachable_code
reachable_next_level (struct eh_region_d *region, tree type_thrown,
		      struct reachable_info *info,
		      bool maybe_resx)
{
  switch (region->type)
    {
    case ERT_CLEANUP:
      /* Before landing-pad generation, we model control flow
	 directly to the individual handlers.  In this way we can
	 see that catch handler types may shadow one another.  */
      add_reachable_handler (info, region, region);
      return RNL_MAYBE_CAUGHT;

    case ERT_TRY:
      {
	struct eh_region_d *c;
	enum reachable_code ret = RNL_NOT_CAUGHT;

	for (c = region->u.eh_try.eh_catch; c ; c = c->u.eh_catch.next_catch)
	  {
	    /* A catch-all handler ends the search.  */
	    if (c->u.eh_catch.type_list == NULL)
	      {
		add_reachable_handler (info, region, c);
		return RNL_CAUGHT;
	      }

	    if (type_thrown)
	      {
		/* If we have at least one type match, end the search.  */
		tree tp_node = c->u.eh_catch.type_list;

		for (; tp_node; tp_node = TREE_CHAIN (tp_node))
		  {
		    tree type = TREE_VALUE (tp_node);

		    if (type == type_thrown
			|| (lang_eh_type_covers
			    && (*lang_eh_type_covers) (type, type_thrown)))
		      {
			add_reachable_handler (info, region, c);
			return RNL_CAUGHT;
		      }
		  }

		/* If we have definitive information of a match failure,
		   the catch won't trigger.  */
		if (lang_eh_type_covers)
		  return RNL_NOT_CAUGHT;
	      }

	    /* At this point, we either don't know what type is thrown or
	       don't have front-end assistance to help deciding if it is
	       covered by one of the types in the list for this region.

	       We'd then like to add this region to the list of reachable
	       handlers since it is indeed potentially reachable based on the
	       information we have.

	       Actually, this handler is for sure not reachable if all the
	       types it matches have already been caught. That is, it is only
	       potentially reachable if at least one of the types it catches
	       has not been previously caught.  */

	    if (! info)
	      ret = RNL_MAYBE_CAUGHT;
	    else
	      {
		tree tp_node = c->u.eh_catch.type_list;
		bool maybe_reachable = false;

		/* Compute the potential reachability of this handler and
		   update the list of types caught at the same time.  */
		for (; tp_node; tp_node = TREE_CHAIN (tp_node))
		  {
		    tree type = TREE_VALUE (tp_node);

		    if (! check_handled (info->types_caught, type))
		      {
			info->types_caught
			  = tree_cons (NULL, type, info->types_caught);

			maybe_reachable = true;
		      }
		  }

		if (maybe_reachable)
		  {
		    add_reachable_handler (info, region, c);

		    /* ??? If the catch type is a base class of every allowed
		       type, then we know we can stop the search.  */
		    ret = RNL_MAYBE_CAUGHT;
		  }
	      }
	  }

	return ret;
      }

    case ERT_ALLOWED_EXCEPTIONS:
      /* An empty list of types definitely ends the search.  */
      if (region->u.allowed.type_list == NULL_TREE)
	{
	  add_reachable_handler (info, region, region);
	  return RNL_CAUGHT;
	}

      /* Collect a list of lists of allowed types for use in detecting
	 when a catch may be transformed into a catch-all.  */
      if (info)
	info->types_allowed = tree_cons (NULL_TREE,
					 region->u.allowed.type_list,
					 info->types_allowed);

      /* If we have definitive information about the type hierarchy,
	 then we can tell if the thrown type will pass through the
	 filter.  */
      if (type_thrown && lang_eh_type_covers)
	{
	  if (check_handled (region->u.allowed.type_list, type_thrown))
	    return RNL_NOT_CAUGHT;
	  else
	    {
	      add_reachable_handler (info, region, region);
	      return RNL_CAUGHT;
	    }
	}

      add_reachable_handler (info, region, region);
      return RNL_MAYBE_CAUGHT;

    case ERT_CATCH:
      /* Catch regions are handled by their controlling try region.  */
      return RNL_NOT_CAUGHT;

    case ERT_MUST_NOT_THROW:
      /* Here we end our search, since no exceptions may propagate.
        
         Local landing pads of ERT_MUST_NOT_THROW instructions are reachable
	 only via locally handled RESX instructions.  

	 When we inline a function call, we can bring in new handlers.  In order
	 to avoid ERT_MUST_NOT_THROW landing pads from being deleted as unreachable
	 assume that such handlers exists prior for any inlinable call prior
	 inlining decisions are fixed.  */

      if (maybe_resx)
	{
	  add_reachable_handler (info, region, region);
	  return RNL_CAUGHT;
	}
      else
	return RNL_BLOCKED;

    case ERT_THROW:
    case ERT_UNKNOWN:
      /* Shouldn't see these here.  */
      gcc_unreachable ();
      break;
    default:
      gcc_unreachable ();
    }
}

/* Invoke CALLBACK on each region reachable from REGION_NUMBER.  */

void
foreach_reachable_handler (int region_number, bool is_resx, bool inlinable_call,
			   void (*callback) (struct eh_region_d *, void *),
			   void *callback_data)
{
  struct reachable_info info;
  struct eh_region_d *region;
  tree type_thrown;

  memset (&info, 0, sizeof (info));
  info.callback = callback;
  info.callback_data = callback_data;

  region = VEC_index (eh_region, cfun->eh->region_array, region_number);
  if (!region)
    return;

  type_thrown = NULL_TREE;
  if (is_resx)
    {
      /* A RESX leaves a region instead of entering it.  Thus the
	 region itself may have been deleted out from under us.  */
      if (region == NULL)
	return;
      region = region->outer;
    }
  else if (region->type == ERT_THROW)
    {
      type_thrown = region->u.eh_throw.type;
      region = region->outer;
    }

  while (region)
    {
      if (reachable_next_level (region, type_thrown, &info,
      				inlinable_call || is_resx) >= RNL_CAUGHT)
	break;
      /* If we have processed one cleanup, there is no point in
	 processing any more of them.  Each cleanup will have an edge
	 to the next outer cleanup region, so the flow graph will be
	 accurate.  */
      if (region->type == ERT_CLEANUP)
        {
	  enum reachable_code code = RNL_NOT_CAUGHT;
	  region = find_prev_try (region->outer);
	  /* Continue looking for outer TRY region until we find one
	     that might cath something.  */
          while (region
	  	 && (code = reachable_next_level (region, type_thrown, &info,
      			                          inlinable_call || is_resx))
		     == RNL_NOT_CAUGHT)
	    region = find_prev_try (region->outer);
	  if (code >= RNL_CAUGHT)
	    break;
	}
      if (region)
	region = region->outer;
    }
}

/* Retrieve a list of labels of exception handlers which can be
   reached by a given insn.  */

static void
arh_to_landing_pad (struct eh_region_d *region, void *data)
{
  rtx *p_handlers = (rtx *) data;
  if (! *p_handlers)
    *p_handlers = alloc_INSN_LIST (region->landing_pad, NULL_RTX);
}

static void
arh_to_label (struct eh_region_d *region, void *data)
{
  rtx *p_handlers = (rtx *) data;
  *p_handlers = alloc_INSN_LIST (region->label, *p_handlers);
}

rtx
reachable_handlers (rtx insn)
{
  bool is_resx = false;
  rtx handlers = NULL;
  int region_number;

  if (JUMP_P (insn)
      && GET_CODE (PATTERN (insn)) == RESX)
    {
      region_number = XINT (PATTERN (insn), 0);
      is_resx = true;
    }
  else
    {
      rtx note = find_reg_note (insn, REG_EH_REGION, NULL_RTX);
      if (!note || INTVAL (XEXP (note, 0)) <= 0)
	return NULL;
      region_number = INTVAL (XEXP (note, 0));
    }

  foreach_reachable_handler (region_number, is_resx, false,
			     (crtl->eh.built_landing_pads
			      ? arh_to_landing_pad
			      : arh_to_label),
			     &handlers);

  return handlers;
}

/* Determine if the given INSN can throw an exception that is caught
   within the function.  */

bool
can_throw_internal_1 (int region_number, bool is_resx, bool inlinable_call)
{
  struct eh_region_d *region;
  tree type_thrown;

  region = VEC_index (eh_region, cfun->eh->region_array, region_number);
  if (!region)
    return false;

  type_thrown = NULL_TREE;
  if (is_resx)
    region = region->outer;
  else if (region->type == ERT_THROW)
    {
      type_thrown = region->u.eh_throw.type;
      region = region->outer;
    }

  /* If this exception is ignored by each and every containing region,
     then control passes straight out.  The runtime may handle some
     regions, which also do not require processing internally.  */
  for (; region; region = region->outer)
    {
      enum reachable_code how = reachable_next_level (region, type_thrown, 0,
      						      inlinable_call || is_resx);
      if (how == RNL_BLOCKED)
	return false;
      if (how != RNL_NOT_CAUGHT)
	return true;
    }

  return false;
}

bool
can_throw_internal (const_rtx insn)
{
  rtx note;

  if (! INSN_P (insn))
    return false;

  if (JUMP_P (insn)
      && GET_CODE (PATTERN (insn)) == RESX
      && XINT (PATTERN (insn), 0) > 0)
    return can_throw_internal_1 (XINT (PATTERN (insn), 0), true, false);

  if (NONJUMP_INSN_P (insn)
      && GET_CODE (PATTERN (insn)) == SEQUENCE)
    insn = XVECEXP (PATTERN (insn), 0, 0);

  /* Every insn that might throw has an EH_REGION note.  */
  note = find_reg_note (insn, REG_EH_REGION, NULL_RTX);
  if (!note || INTVAL (XEXP (note, 0)) <= 0)
    return false;

  return can_throw_internal_1 (INTVAL (XEXP (note, 0)), false, false);
}

/* Determine if the given INSN can throw an exception that is
   visible outside the function.  */

bool
can_throw_external_1 (int region_number, bool is_resx, bool inlinable_call)
{
  struct eh_region_d *region;
  tree type_thrown;

  region = VEC_index (eh_region, cfun->eh->region_array, region_number);
  if (!region)
    return true;

  type_thrown = NULL_TREE;
  if (is_resx)
    region = region->outer;
  else if (region->type == ERT_THROW)
    {
      type_thrown = region->u.eh_throw.type;
      region = region->outer;
    }

  /* If the exception is caught or blocked by any containing region,
     then it is not seen by any calling function.  */
  for (; region ; region = region->outer)
    if (reachable_next_level (region, type_thrown, NULL,
    	inlinable_call || is_resx) >= RNL_CAUGHT)
      return false;

  return true;
}

bool
can_throw_external (const_rtx insn)
{
  rtx note;

  if (! INSN_P (insn))
    return false;

  if (JUMP_P (insn)
      && GET_CODE (PATTERN (insn)) == RESX
      && XINT (PATTERN (insn), 0) > 0)
    return can_throw_external_1 (XINT (PATTERN (insn), 0), true, false);

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

  note = find_reg_note (insn, REG_EH_REGION, NULL_RTX);
  if (!note)
    {
      /* Calls (and trapping insns) without notes are outside any
	 exception handling region in this function.  We have to
	 assume it might throw.  Given that the front end and middle
	 ends mark known NOTHROW functions, this isn't so wildly
	 inaccurate.  */
      return (CALL_P (insn)
	      || (flag_non_call_exceptions
		  && may_trap_p (PATTERN (insn))));
    }
  if (INTVAL (XEXP (note, 0)) <= 0)
    return false;

  return can_throw_external_1 (INTVAL (XEXP (note, 0)), false, false);
}

/* Set TREE_NOTHROW and crtl->all_throwers_are_sibcalls.  */

unsigned int
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
      && (cgraph_function_body_availability (cgraph_node
					     (current_function_decl))
          >= AVAIL_AVAILABLE))
    {
      struct cgraph_node *node = cgraph_node (current_function_decl);
      struct cgraph_edge *e;
      for (e = node->callers; e; e = e->next_caller)
        e->can_throw_external = false;
      TREE_NOTHROW (current_function_decl) = 1;

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
  TODO_dump_func,                       /* todo_flags_finish */
 }
};


/* Various hooks for unwind library.  */

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
  addr = plus_constant (addr, RETURN_ADDR_OFFSET);
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
  addr = plus_constant (addr, -RETURN_ADDR_OFFSET);
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
      new_ar->offset = VARRAY_ACTIVE_SIZE (crtl->eh.action_record_data) + 1;
      new_ar->filter = filter;
      new_ar->next = next;
      *slot = new_ar;

      /* The filter value goes in untouched.  The link to the next
	 record is a "self-relative" byte offset, or zero to indicate
	 that there is no next record.  So convert the absolute 1 based
	 indices we've been carrying around into a displacement.  */

      push_sleb128 (&crtl->eh.action_record_data, filter);
      if (next)
	next -= VARRAY_ACTIVE_SIZE (crtl->eh.action_record_data) + 1;
      push_sleb128 (&crtl->eh.action_record_data, next);
    }

  return new_ar->offset;
}

static int
collect_one_action_chain (htab_t ar_hash, struct eh_region_d *region)
{
  struct eh_region_d *c;
  int next;

  /* If we've reached the top of the region chain, then we have
     no actions, and require no landing pad.  */
  if (region == NULL)
    return -1;

  switch (region->type)
    {
    case ERT_CLEANUP:
      /* A cleanup adds a zero filter to the beginning of the chain, but
	 there are special cases to look out for.  If there are *only*
	 cleanups along a path, then it compresses to a zero action.
	 Further, if there are multiple cleanups along a path, we only
	 need to represent one of them, as that is enough to trigger
	 entry to the landing pad at runtime.  */
      next = collect_one_action_chain (ar_hash, region->outer);
      if (next <= 0)
	return 0;
      for (c = region->outer; c ; c = c->outer)
	if (c->type == ERT_CLEANUP)
	  return next;
      return add_action_record (ar_hash, 0, next);

    case ERT_TRY:
      /* Process the associated catch regions in reverse order.
	 If there's a catch-all handler, then we don't need to
	 search outer regions.  Use a magic -3 value to record
	 that we haven't done the outer search.  */
      next = -3;
      for (c = region->u.eh_try.last_catch; c ; c = c->u.eh_catch.prev_catch)
	{
	  if (c->u.eh_catch.type_list == NULL)
	    {
	      /* Retrieve the filter from the head of the filter list
		 where we have stored it (see assign_filter_values).  */
	      int filter
		= TREE_INT_CST_LOW (TREE_VALUE (c->u.eh_catch.filter_list));

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

	      flt_node = c->u.eh_catch.filter_list;
	      for (; flt_node; flt_node = TREE_CHAIN (flt_node))
		{
		  int filter = TREE_INT_CST_LOW (TREE_VALUE (flt_node));
		  next = add_action_record (ar_hash, filter, next);
		}
	    }
	}
      return next;

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

    case ERT_CATCH:
    case ERT_THROW:
      /* CATCH regions are handled in TRY above.  THROW regions are
	 for optimization information only and produce no output.  */
      return collect_one_action_chain (ar_hash, region->outer);

    default:
      gcc_unreachable ();
    }
}

static int
add_call_site (rtx landing_pad, int action)
{
  call_site_record record;
  
  record = GGC_NEW (struct call_site_record_d);
  record->landing_pad = landing_pad;
  record->action = action;

  VEC_safe_push (call_site_record, gc, crtl->eh.call_site_record, record);

  return call_site_base + VEC_length (call_site_record, crtl->eh.call_site_record) - 1;
}

/* Turn REG_EH_REGION notes back into NOTE_INSN_EH_REGION notes.
   The new note numbers will not refer to region numbers, but
   instead to call site entries.  */

unsigned int
convert_to_eh_region_ranges (void)
{
  rtx insn, iter, note;
  htab_t ar_hash;
  int last_action = -3;
  rtx last_action_insn = NULL_RTX;
  rtx last_landing_pad = NULL_RTX;
  rtx first_no_action_insn = NULL_RTX;
  int call_site = 0;

  if (USING_SJLJ_EXCEPTIONS || cfun->eh->region_tree == NULL)
    return 0;

  VARRAY_UCHAR_INIT (crtl->eh.action_record_data, 64, "action_record_data");

  ar_hash = htab_create (31, action_record_hash, action_record_eq, free);

  for (iter = get_insns (); iter ; iter = NEXT_INSN (iter))
    if (INSN_P (iter))
      {
	struct eh_region_d *region;
	int this_action;
	rtx this_landing_pad;

	insn = iter;
	if (NONJUMP_INSN_P (insn)
	    && GET_CODE (PATTERN (insn)) == SEQUENCE)
	  insn = XVECEXP (PATTERN (insn), 0, 0);

	note = find_reg_note (insn, REG_EH_REGION, NULL_RTX);
	if (!note)
	  {
	    if (! (CALL_P (insn)
		   || (flag_non_call_exceptions
		       && may_trap_p (PATTERN (insn)))))
	      continue;
	    this_action = -1;
	    region = NULL;
	  }
	else
	  {
	    if (INTVAL (XEXP (note, 0)) <= 0)
	      continue;
	    region = VEC_index (eh_region, cfun->eh->region_array, INTVAL (XEXP (note, 0)));
	    this_action = collect_one_action_chain (ar_hash, region);
	  }

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

	/* Cleanups and handlers may share action chains but not
	   landing pads.  Collect the landing pad for this region.  */
	if (this_action >= 0)
	  {
	    struct eh_region_d *o;
	    for (o = region; ! o->landing_pad ; o = o->outer)
	      continue;
	    this_landing_pad = o->landing_pad;
	  }
	else
	  this_landing_pad = NULL_RTX;

	/* Differing actions or landing pads implies a change in call-site
	   info, which implies some EH_REGION note should be emitted.  */
	if (last_action != this_action
	    || last_landing_pad != this_landing_pad)
	  {
	    /* If we'd not seen a previous action (-3) or the previous
	       action was must-not-throw (-2), then we do not need an
	       end note.  */
	    if (last_action >= -1)
	      {
		/* If we delayed the creation of the begin, do it now.  */
		if (first_no_action_insn)
		  {
		    call_site = add_call_site (NULL_RTX, 0);
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
					   this_action < 0 ? 0 : this_action);
		note = emit_note_before (NOTE_INSN_EH_REGION_BEG, iter);
		NOTE_EH_HANDLER (note) = call_site;
	      }

	    last_action = this_action;
	    last_landing_pad = this_landing_pad;
	  }
	last_action_insn = iter;
      }

  if (last_action >= -1 && ! first_no_action_insn)
    {
      note = emit_note_after (NOTE_INSN_EH_REGION_END, last_action_insn);
      NOTE_EH_HANDLER (note) = call_site;
    }

  htab_delete (ar_hash);
  return 0;
}

struct rtl_opt_pass pass_convert_to_eh_region_ranges =
{
 {
  RTL_PASS,
  "eh_ranges",                          /* name */
  NULL,                                 /* gate */
  convert_to_eh_region_ranges,          /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_NONE,                              /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_dump_func,			/* todo_flags_finish */
 }
};


static void
push_uleb128 (varray_type *data_area, unsigned int value)
{
  do
    {
      unsigned char byte = value & 0x7f;
      value >>= 7;
      if (value)
	byte |= 0x80;
      VARRAY_PUSH_UCHAR (*data_area, byte);
    }
  while (value);
}

static void
push_sleb128 (varray_type *data_area, int value)
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
      VARRAY_PUSH_UCHAR (*data_area, byte);
    }
  while (more);
}


#ifndef HAVE_AS_LEB128
static int
dw2_size_of_call_site_table (void)
{
  int n = VEC_length (call_site_record, crtl->eh.call_site_record);
  int size = n * (4 + 4 + 4);
  int i;

  for (i = 0; i < n; ++i)
    {
      struct call_site_record_d *cs =
	VEC_index (call_site_record, crtl->eh.call_site_record, i);
      size += size_of_uleb128 (cs->action);
    }

  return size;
}

static int
sjlj_size_of_call_site_table (void)
{
  int n = VEC_length (call_site_record, crtl->eh.call_site_record);
  int size = 0;
  int i;

  for (i = 0; i < n; ++i)
    {
      struct call_site_record_d *cs =
	VEC_index (call_site_record, crtl->eh.call_site_record, i);
      size += size_of_uleb128 (INTVAL (cs->landing_pad));
      size += size_of_uleb128 (cs->action);
    }

  return size;
}
#endif

static void
dw2_output_call_site_table (void)
{
  int n = VEC_length (call_site_record, crtl->eh.call_site_record);
  int i;

  for (i = 0; i < n; ++i)
    {
      struct call_site_record_d *cs =
	VEC_index (call_site_record, crtl->eh.call_site_record, i);
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
#ifdef HAVE_AS_LEB128
      dw2_asm_output_delta_uleb128 (reg_start_lab,
				    current_function_func_begin_label,
				    "region %d start", i);
      dw2_asm_output_delta_uleb128 (reg_end_lab, reg_start_lab,
				    "length");
      if (cs->landing_pad)
	dw2_asm_output_delta_uleb128 (landing_pad_lab,
				      current_function_func_begin_label,
				      "landing pad");
      else
	dw2_asm_output_data_uleb128 (0, "landing pad");
#else
      dw2_asm_output_delta (4, reg_start_lab,
			    current_function_func_begin_label,
			    "region %d start", i);
      dw2_asm_output_delta (4, reg_end_lab, reg_start_lab, "length");
      if (cs->landing_pad)
	dw2_asm_output_delta (4, landing_pad_lab,
			      current_function_func_begin_label,
			      "landing pad");
      else
	dw2_asm_output_data (4, 0, "landing pad");
#endif
      dw2_asm_output_data_uleb128 (cs->action, "action");
    }

  call_site_base += n;
}

static void
sjlj_output_call_site_table (void)
{
  int n = VEC_length (call_site_record, crtl->eh.call_site_record);
  int i;

  for (i = 0; i < n; ++i)
    {
      struct call_site_record_d *cs =
	VEC_index (call_site_record, crtl->eh.call_site_record, i);

      dw2_asm_output_data_uleb128 (INTVAL (cs->landing_pad),
				   "region %d landing pad", i);
      dw2_asm_output_data_uleb128 (cs->action, "action");
    }

  call_site_base += n;
}

#ifndef TARGET_UNWIND_INFO
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
      if (targetm.have_named_sections)
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
#endif


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
      struct varpool_node *node;

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
	    {
	      node = varpool_node (type);
	      if (node)
		varpool_mark_needed_node (node);
	      is_public = TREE_PUBLIC (type);
	    }
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

void
output_function_exception_table (const char * ARG_UNUSED (fnname))
{
  int tt_format, cs_format, lp_format, i, n;
#ifdef HAVE_AS_LEB128
  char ttype_label[32];
  char cs_after_size_label[32];
  char cs_end_label[32];
#else
  int call_site_len;
#endif
  int have_tt_data;
  int tt_format_size = 0;

  /* Not all functions need anything.  */
  if (! crtl->uses_eh_lsda)
    return;

  if (eh_personality_libfunc)
    assemble_external_libcall (eh_personality_libfunc);

#ifdef TARGET_UNWIND_INFO
  /* TODO: Move this into target file.  */
  fputs ("\t.personality\t", asm_out_file);
  output_addr_const (asm_out_file, eh_personality_libfunc);
  fputs ("\n\t.handlerdata\n", asm_out_file);
  /* Note that varasm still thinks we're in the function's code section.
     The ".endp" directive that will immediately follow will take us back.  */
#else
  switch_to_exception_section (fnname);
#endif

  /* If the target wants a label to begin the table, emit it here.  */
  targetm.asm_out.except_table_label (asm_out_file);

  have_tt_data = (VEC_length (tree, crtl->eh.ttype_data) > 0
		  || VARRAY_ACTIVE_SIZE (crtl->eh.ehspec_data) > 0);

  /* Indicate the format of the @TType entries.  */
  if (! have_tt_data)
    tt_format = DW_EH_PE_omit;
  else
    {
      tt_format = ASM_PREFERRED_EH_DATA_FORMAT (/*code=*/0, /*global=*/1);
#ifdef HAVE_AS_LEB128
      ASM_GENERATE_INTERNAL_LABEL (ttype_label, "LLSDATT",
				   current_function_funcdef_no);
#endif
      tt_format_size = size_of_encoded_value (tt_format);

      assemble_align (tt_format_size * BITS_PER_UNIT);
    }

  targetm.asm_out.internal_label (asm_out_file, "LLSDA",
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
  if (USING_SJLJ_EXCEPTIONS)
    call_site_len = sjlj_size_of_call_site_table ();
  else
    call_site_len = dw2_size_of_call_site_table ();
#endif

  /* A pc-relative 4-byte displacement to the @TType data.  */
  if (have_tt_data)
    {
#ifdef HAVE_AS_LEB128
      char ttype_after_disp_label[32];
      ASM_GENERATE_INTERNAL_LABEL (ttype_after_disp_label, "LLSDATTD",
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
		    + VARRAY_ACTIVE_SIZE (crtl->eh.action_record_data)
		    + (VEC_length (tree, crtl->eh.ttype_data)
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
  ASM_GENERATE_INTERNAL_LABEL (cs_after_size_label, "LLSDACSB",
			       current_function_funcdef_no);
  ASM_GENERATE_INTERNAL_LABEL (cs_end_label, "LLSDACSE",
			       current_function_funcdef_no);
  dw2_asm_output_delta_uleb128 (cs_end_label, cs_after_size_label,
				"Call-site table length");
  ASM_OUTPUT_LABEL (asm_out_file, cs_after_size_label);
  if (USING_SJLJ_EXCEPTIONS)
    sjlj_output_call_site_table ();
  else
    dw2_output_call_site_table ();
  ASM_OUTPUT_LABEL (asm_out_file, cs_end_label);
#else
  dw2_asm_output_data_uleb128 (call_site_len,"Call-site table length");
  if (USING_SJLJ_EXCEPTIONS)
    sjlj_output_call_site_table ();
  else
    dw2_output_call_site_table ();
#endif

  /* ??? Decode and interpret the data for flag_debug_asm.  */
  n = VARRAY_ACTIVE_SIZE (crtl->eh.action_record_data);
  for (i = 0; i < n; ++i)
    dw2_asm_output_data (1, VARRAY_UCHAR (crtl->eh.action_record_data, i),
			 (i ? NULL : "Action record table"));

  if (have_tt_data)
    assemble_align (tt_format_size * BITS_PER_UNIT);

  i = VEC_length (tree, crtl->eh.ttype_data);
  while (i-- > 0)
    {
      tree type = VEC_index (tree, crtl->eh.ttype_data, i);
      output_ttype (type, tt_format, tt_format_size);
    }

#ifdef HAVE_AS_LEB128
  if (have_tt_data)
      ASM_OUTPUT_LABEL (asm_out_file, ttype_label);
#endif

  /* ??? Decode and interpret the data for flag_debug_asm.  */
  n = VARRAY_ACTIVE_SIZE (crtl->eh.ehspec_data);
  for (i = 0; i < n; ++i)
    {
      if (targetm.arm_eabi_unwinder)
	{
	  tree type = VARRAY_TREE (crtl->eh.ehspec_data, i);
	  output_ttype (type, tt_format, tt_format_size);
	}
      else
	dw2_asm_output_data (1, VARRAY_UCHAR (crtl->eh.ehspec_data, i),
			     (i ? NULL : "Exception specification table"));
    }

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

/* Dump EH information to OUT.  */

void
dump_eh_tree (FILE * out, struct function *fun)
{
  struct eh_region_d *i;
  int depth = 0;
  static const char *const type_name[] = { "unknown", "cleanup", "try", "catch",
    					   "allowed_exceptions", "must_not_throw",
    					   "throw"
  					 };

  i = fun->eh->region_tree;
  if (!i)
    return;

  fprintf (out, "Eh tree:\n");
  while (1)
    {
      fprintf (out, "  %*s %i %s", depth * 2, "",
	       i->region_number, type_name[(int) i->type]);
      if (i->tree_label)
	{
	  fprintf (out, " tree_label:");
	  print_generic_expr (out, i->tree_label, 0);
	}
      if (i->label)
	fprintf (out, " label:%i", INSN_UID (i->label));
      if (i->landing_pad)
	{
          fprintf (out, " landing_pad:%i", INSN_UID (i->landing_pad));
	  if (NOTE_P (i->landing_pad))
	    fprintf (out, " (deleted)");
        }
      if (i->post_landing_pad)
	{
          fprintf (out, " post_landing_pad:%i", INSN_UID (i->post_landing_pad));
	  if (NOTE_P (i->post_landing_pad))
	    fprintf (out, " (deleted)");
	}
      if (i->resume)
	{
	  rtx resume_list = i->resume;
          fprintf (out, " resume:");
	  while (GET_CODE (resume_list) == INSN_LIST)
	    {
	      fprintf (out, "%i,", INSN_UID (XEXP (resume_list, 0)));
	      if (NOTE_P (XEXP (resume_list, 0)))
		fprintf (out, " (deleted)");
	      resume_list = XEXP (resume_list, 1);
	    }
          fprintf (out, " resume:%i", INSN_UID (i->resume));
	  if (NOTE_P (i->resume))
	    fprintf (out, " (deleted)");
	}
      if (i->may_contain_throw)
        fprintf (out, " may_contain_throw");
      switch (i->type)
	{
	case ERT_CLEANUP:
	  break;

	case ERT_TRY:
	  {
	    struct eh_region_d *c;
	    fprintf (out, " catch regions:");
	    for (c = i->u.eh_try.eh_catch; c; c = c->u.eh_catch.next_catch)
	      fprintf (out, " %i", c->region_number);
	  }
	  break;

	case ERT_CATCH:
	  if (i->u.eh_catch.prev_catch)
	    fprintf (out, " prev: %i",
		     i->u.eh_catch.prev_catch->region_number);
	  if (i->u.eh_catch.next_catch)
	    fprintf (out, " next %i",
		     i->u.eh_catch.next_catch->region_number);
	  fprintf (out, " type:");
	  print_generic_expr (out, i->u.eh_catch.type_list, 0);
	  break;

	case ERT_ALLOWED_EXCEPTIONS:
	  fprintf (out, " filter :%i types:", i->u.allowed.filter);
	  print_generic_expr (out, i->u.allowed.type_list, 0);
	  break;

	case ERT_THROW:
	  fprintf (out, " type:");
	  print_generic_expr (out, i->u.eh_throw.type, 0);
	  break;

	case ERT_MUST_NOT_THROW:
	  break;

	case ERT_UNKNOWN:
	  break;
	}
      if (i->aka)
	{
	  fprintf (out, " also known as:");
	  dump_bitmap (out, i->aka);
	}
      else
	fprintf (out, "\n");
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

void
debug_eh_tree (struct function *fn)
{
  dump_eh_tree (stderr, fn);
}


/* Verify EH region invariants.  */

static bool
verify_eh_region (struct eh_region_d *region)
{
  bool found = false;
  if (!region)
    return false;
  switch (region->type)
    {
    case ERT_TRY:
      {
	struct eh_region_d *c, *prev = NULL;
	if (region->u.eh_try.eh_catch->u.eh_catch.prev_catch)
	  {
	    error ("Try region %i has wrong rh_catch pointer to %i",
		   region->region_number,
		   region->u.eh_try.eh_catch->region_number);
	    found = true;
	  }
	for (c = region->u.eh_try.eh_catch; c; c = c->u.eh_catch.next_catch)
	  {
	    if (c->outer != region->outer)
	      {
		error
		  ("Catch region %i has different outer region than try region %i",
		   c->region_number, region->region_number);
		found = true;
	      }
	    if (c->u.eh_catch.prev_catch != prev)
	      {
		error ("Catch region %i has corrupted catchlist",
		       c->region_number);
		found = true;
	      }
	    prev = c;
	  }
	if (prev != region->u.eh_try.last_catch)
	  {
	    error
	      ("Try region %i has wrong last_catch pointer to %i instead of %i",
	       region->region_number,
	       region->u.eh_try.last_catch->region_number,
	       prev->region_number);
	    found = true;
	  }
      }
      break;
    case ERT_CATCH:
      if (!region->u.eh_catch.prev_catch
          && (!region->next_peer || region->next_peer->type != ERT_TRY))
	{
	  error ("Catch region %i should be followed by try", region->region_number);
	  found = true;
	}
      break;
    case ERT_CLEANUP:
    case ERT_ALLOWED_EXCEPTIONS:
    case ERT_MUST_NOT_THROW:
    case ERT_THROW:
      break;
    case ERT_UNKNOWN:
      gcc_unreachable ();
    }
  for (region = region->inner; region; region = region->next_peer)
    found |= verify_eh_region (region);
  return found;
}

/* Verify invariants on EH datastructures.  */

void
verify_eh_tree (struct function *fun)
{
  struct eh_region_d *i, *outer = NULL;
  bool err = false;
  int nvisited = 0;
  int count = 0;
  int j;
  int depth = 0;

  if (!fun->eh->region_tree)
    return;
  for (j = fun->eh->last_region_number; j > 0; --j)
    if ((i = VEC_index (eh_region, fun->eh->region_array, j)))
      {
	if (i->region_number == j)
	  count++;
	if (i->region_number != j && (!i->aka || !bitmap_bit_p (i->aka, j)))
	  {
	    error ("region_array is corrupted for region %i",
		   i->region_number);
	    err = true;
	  }
      }
  i = fun->eh->region_tree;

  while (1)
    {
      if (VEC_index (eh_region, fun->eh->region_array, i->region_number) != i)
	{
	  error ("region_array is corrupted for region %i", i->region_number);
	  err = true;
	}
      if (i->outer != outer)
	{
	  error ("outer block of region %i is wrong", i->region_number);
	  err = true;
	}
      if (i->may_contain_throw && outer && !outer->may_contain_throw)
	{
	  error
	    ("region %i may contain throw and is contained in region that may not",
	     i->region_number);
	  err = true;
	}
      if (depth < 0)
	{
	  error ("negative nesting depth of region %i", i->region_number);
	  err = true;
	}
      nvisited++;
      /* If there are sub-regions, process them.  */
      if (i->inner)
	outer = i, i = i->inner, depth++;
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
		{
		  if (depth != -1)
		    {
		      error ("tree list ends on depth %i", depth + 1);
		      err = true;
		    }
		  if (count != nvisited)
		    {
		      error ("array does not match the region tree");
		      err = true;
		    }
		  if (!err)
		    for (i = fun->eh->region_tree; i; i = i->next_peer)
		      err |= verify_eh_region (i);
		  
		  if (err)
		    {
		      dump_eh_tree (stderr, fun);
		      internal_error ("verify_eh_tree failed");
		    }
		  return;
		}
	      outer = i->outer;
	    }
	  while (i->next_peer == NULL);
	  i = i->next_peer;
	}
    }
}

/* Initialize unwind_resume_libfunc.  */

void
default_init_unwind_resume_libfunc (void)
{
  /* The default c++ routines aren't actually c++ specific, so use those.  */
  unwind_resume_libfunc =
    init_one_libfunc ( USING_SJLJ_EXCEPTIONS ? "_Unwind_SjLj_Resume"
					     : "_Unwind_Resume");
}


static bool
gate_handle_eh (void)
{
  return doing_eh (0);
}

/* Complete generation of exception handling code.  */
static unsigned int
rest_of_handle_eh (void)
{
  finish_eh_generation ();
  cleanup_cfg (CLEANUP_NO_INSN_DEL);
  return 0;
}

struct rtl_opt_pass pass_rtl_eh =
{
 {
  RTL_PASS,
  "eh",                                 /* name */
  gate_handle_eh,                       /* gate */
  rest_of_handle_eh,			/* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_JUMP,                              /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_dump_func                        /* todo_flags_finish */
 }
};

#include "gt-except.h"
