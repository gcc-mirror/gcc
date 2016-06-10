/* Unit tests for GCC's garbage collector (and gengtype etc).
   Copyright (C) 2015-2016 Free Software Foundation, Inc.

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
#include "system.h"
#include "coretypes.h"
#include "tree-core.h"
#include "tree.h"
#include "ggc-internal.h" /* (for ggc_force_collect).  */
#include "selftest.h"

#if CHECKING_P

/* The various GTY markers must be outside of a namespace to be seen by
   gengtype, so we don't put this file within the selftest namespace.  */

/* A helper function for writing ggc tests.  */

static void
forcibly_ggc_collect ()
{
  ggc_force_collect = true;
  ggc_collect ();
  ggc_force_collect = false;
}



/* Verify that a simple struct works, and that it can
   own references to non-roots, and have them be marked.  */

struct GTY(()) test_struct
{
  struct test_struct *other;
};

static GTY(()) test_struct *root_test_struct;

static void
test_basic_struct ()
{
  root_test_struct = ggc_cleared_alloc <test_struct> ();
  root_test_struct->other = ggc_cleared_alloc <test_struct> ();

  forcibly_ggc_collect ();

  ASSERT_TRUE (ggc_marked_p (root_test_struct));
  ASSERT_TRUE (ggc_marked_p (root_test_struct->other));
}



/* Selftest for GTY((length)).  */

/* A test struct using GTY((length)).  */

struct GTY(()) test_of_length
{
  int num_elem;
  struct test_of_length * GTY ((length ("%h.num_elem"))) elem[1];
};

static GTY(()) test_of_length *root_test_of_length;

static void
test_length ()
{
  const int count = 5;
  size_t sz = sizeof (test_of_length) + (count- 1) * sizeof (test_of_length *);
  root_test_of_length = (test_of_length *)ggc_internal_cleared_alloc (sz);
  root_test_of_length->num_elem = count;
  for (int i = 0; i < count; i++)
    root_test_of_length->elem[i] = ggc_cleared_alloc <test_of_length> ();

  forcibly_ggc_collect ();

  ASSERT_TRUE (ggc_marked_p (root_test_of_length));
  for (int i = 0; i < count; i++)
    ASSERT_TRUE (ggc_marked_p (root_test_of_length->elem[i]));
}



/* Selftest for unions, GTY((tag)), and GTY((desc)).  */

/* A struct with a reference that's an a different offset to test_struct,
   to ensure that we're using the correct types.  */

struct GTY(()) test_other
{
  char dummy[256];
  test_struct *m_ptr;
};

enum which_field
{
  WHICH_FIELD_USE_TEST_STRUCT,
  WHICH_FIELD_USE_TEST_OTHER
};

/* An example function for use by a GTY((desc)) marker.  */

static enum which_field
calc_desc (int kind)
{
  switch (kind)
    {
    case 0: return WHICH_FIELD_USE_TEST_STRUCT;
    case 1: return WHICH_FIELD_USE_TEST_OTHER;
    default:
      gcc_unreachable ();
    }
}

/* A struct containing an example of a union, showing the "tag" and
   "desc" markers.  */

struct GTY(()) test_of_union
{
  int m_kind;
  union u {
    test_struct * GTY ((tag ("WHICH_FIELD_USE_TEST_STRUCT") )) u_test_struct;
    test_other * GTY ((tag ("WHICH_FIELD_USE_TEST_OTHER") )) u_test_other;
  } GTY ((desc ("calc_desc (%0.m_kind)"))) m_u;
};

/* Example roots.  */

static GTY(()) test_of_union *root_test_of_union_1;
static GTY(()) test_of_union *root_test_of_union_2;

/* Verify that the above work correctly.  */

static void
test_union ()
{
  root_test_of_union_1 = ggc_cleared_alloc <test_of_union> ();
  root_test_of_union_1->m_kind = 0;
  test_struct *ts = ggc_cleared_alloc <test_struct> ();
  root_test_of_union_1->m_u.u_test_struct = ts;

  root_test_of_union_2 = ggc_cleared_alloc <test_of_union> ();
  root_test_of_union_2->m_kind = 1;
  test_other *other = ggc_cleared_alloc <test_other> ();
  root_test_of_union_2->m_u.u_test_other = other;
  test_struct *referenced_by_other = ggc_cleared_alloc <test_struct> ();
  other->m_ptr = referenced_by_other;

  forcibly_ggc_collect ();

  ASSERT_TRUE (ggc_marked_p (root_test_of_union_1));
  ASSERT_TRUE (ggc_marked_p (ts));

  ASSERT_TRUE (ggc_marked_p (root_test_of_union_2));
  ASSERT_TRUE (ggc_marked_p (other));
  ASSERT_TRUE (ggc_marked_p (referenced_by_other));
}



/* Verify that destructors get run when instances are collected.  */

struct GTY(()) test_struct_with_dtor
{
  /* This struct has a destructor; it *ought* to be called
     by the ggc machinery when instances are collected.  */
  ~test_struct_with_dtor () { dtor_call_count++; }

  static int dtor_call_count;
};

int test_struct_with_dtor::dtor_call_count;

static void
test_finalization ()
{
  ASSERT_FALSE (need_finalization_p <test_struct> ());
  ASSERT_TRUE (need_finalization_p <test_struct_with_dtor> ());

  /* Create some garbage.  */
  const int count = 10;
  for (int i = 0; i < count; i++)
    ggc_cleared_alloc <test_struct_with_dtor> ();

  test_struct_with_dtor::dtor_call_count = 0;

  forcibly_ggc_collect ();

  /* Verify that the destructor was run for each instance.  */
  ASSERT_EQ (count, test_struct_with_dtor::dtor_call_count);
}



/* Verify that a global can be marked as "deletable".  */

static GTY((deletable)) test_struct *test_of_deletable;

static void
test_deletable_global ()
{
  test_of_deletable = ggc_cleared_alloc <test_struct> ();
  ASSERT_TRUE (test_of_deletable != NULL);

  forcibly_ggc_collect ();

  ASSERT_EQ (NULL, test_of_deletable);
}



/* Verify that gengtype etc can cope with inheritance.  */

class GTY((desc("%h.m_kind"), tag("0"))) example_base
{
 public:
  example_base ()
    : m_kind (0),
      m_a (ggc_cleared_alloc <test_struct> ())
  {}

  void *
  operator new (size_t sz)
  {
    return ggc_internal_cleared_alloc (sz);
  }

 protected:
  example_base (int kind)
    : m_kind (kind),
      m_a (ggc_cleared_alloc <test_struct> ())
  {}

 public:
  int m_kind;
  test_struct *m_a;
};

class GTY((tag("1"))) some_subclass : public example_base
{
 public:
  some_subclass ()
    : example_base (1),
      m_b (ggc_cleared_alloc <test_struct> ())
  {}

  test_struct *m_b;
};

class GTY((tag("2"))) some_other_subclass : public example_base
{
 public:
  some_other_subclass ()
    : example_base (2),
      m_c (ggc_cleared_alloc <test_struct> ())
  {}

  test_struct *m_c;
};

/* Various test roots, both expressed as a ptr to the actual class, and
   as a ptr to the base class.  */
static GTY(()) example_base *test_example_base;
static GTY(()) some_subclass *test_some_subclass;
static GTY(()) some_other_subclass *test_some_other_subclass;
static GTY(()) example_base *test_some_subclass_as_base_ptr;
static GTY(()) example_base *test_some_other_subclass_as_base_ptr;

static void
test_inheritance ()
{
  test_example_base = new example_base ();
  test_some_subclass = new some_subclass ();
  test_some_other_subclass = new some_other_subclass ();
  test_some_subclass_as_base_ptr = new some_subclass ();
  test_some_other_subclass_as_base_ptr = new some_other_subclass ();

  forcibly_ggc_collect ();

  /* Verify that the roots and everything referenced by them got marked
     (both for fields in the base class and those in subclasses).  */
  ASSERT_TRUE (ggc_marked_p (test_example_base));
  ASSERT_TRUE (ggc_marked_p (test_example_base->m_a));

  ASSERT_TRUE (ggc_marked_p (test_some_subclass));
  ASSERT_TRUE (ggc_marked_p (test_some_subclass->m_a));
  ASSERT_TRUE (ggc_marked_p (test_some_subclass->m_b));

  ASSERT_TRUE (ggc_marked_p (test_some_other_subclass));
  ASSERT_TRUE (ggc_marked_p (test_some_other_subclass->m_a));
  ASSERT_TRUE (ggc_marked_p (test_some_other_subclass->m_c));

  ASSERT_TRUE (ggc_marked_p (test_some_subclass_as_base_ptr));
  ASSERT_TRUE (ggc_marked_p (test_some_subclass_as_base_ptr->m_a));
  ASSERT_TRUE (ggc_marked_p (((some_subclass *)
			      test_some_subclass_as_base_ptr)->m_b));

  ASSERT_TRUE (ggc_marked_p (test_some_other_subclass_as_base_ptr));
  ASSERT_TRUE (ggc_marked_p (test_some_other_subclass_as_base_ptr->m_a));
  ASSERT_TRUE (ggc_marked_p (((some_other_subclass *)
			      test_some_other_subclass_as_base_ptr)->m_c));
}



/* Test of chain_next/chain_prev

   Construct a very long linked list, so that without
   the chain_next/chain_prev optimization we'd have
   a stack overflow when gt_ggc_mx_test_node recurses.  */

struct GTY(( chain_next ("%h.m_next"),
	     chain_prev ("%h.m_prev") )) test_node
{
  test_node *m_prev;
  test_node *m_next;
  int m_idx;
};

static GTY(()) test_node *root_test_node;

static void
test_chain_next ()
{
  /* Ideally we would construct a long list so that the number of
     stack frames would be deep enough to crash if gengtype has created
     something that recurses.

     However, as the list is lengthened to increase the chance of
     overflowing the stack, the test will require more time and memory
     to run.  On a Fedora 20 x86_64 box with 128GB of RAM, count=2000000
     without the chain_next optimization reliably overflowed the stack,
     but the test took 0.5s to run.

     For now this test runs with a low value for "count", which defeats
     the main purpose of the test - though it at least gives us coverage
     for walking a GTY((chain_next)) list.

     We could potentially increase this value once we have a better sense
     of the time and space requirements of the test on different hosts,
     or perhaps find a way to reduce the stack size when running this
     testcase.  */
  const int count = 10;

  /* Build the linked list.  */
  root_test_node = ggc_cleared_alloc <test_node> ();
  test_node *tail_node = root_test_node;
  for (int i = 0; i < count; i++)
    {
      test_node *new_node = ggc_cleared_alloc <test_node> ();
      tail_node->m_next = new_node;
      new_node->m_prev = tail_node;
      new_node->m_idx = i;
      tail_node = new_node;
    }

  forcibly_ggc_collect ();

  /* If we got here, we survived.  */

  /* Verify that all nodes in the list were marked.  */
  ASSERT_TRUE (ggc_marked_p (root_test_node));
  test_node *iter_node = root_test_node->m_next;
  for (int i = 0; i < count; i++)
    {
      ASSERT_TRUE (ggc_marked_p (iter_node));
      ASSERT_EQ (i, iter_node->m_idx);
      iter_node = iter_node->m_next;
    }
}



/* Test for GTY((user)).  */

struct GTY((user)) user_struct
{
  char dummy[16];
  test_struct *m_ptr;
};

static GTY(()) user_struct *root_user_struct_ptr;

/* A global for verifying that the user-provided gt_ggc_mx gets
   called.  */
static int num_calls_to_user_gt_ggc_mx;

/* User-provided implementation of gt_ggc_mx.  */

static void
gt_ggc_mx (user_struct *p)
{
  num_calls_to_user_gt_ggc_mx++;
  gt_ggc_mx_test_struct (p->m_ptr);
}

/* User-provided implementation of gt_pch_nx.  */

static void
gt_pch_nx (user_struct *p)
{
  gt_pch_nx_test_struct (p->m_ptr);
}

/* User-provided implementation of gt_pch_nx.  */

static void
gt_pch_nx (user_struct *p, gt_pointer_operator op, void *cookie)
{
  op (&(p->m_ptr), cookie);
}

/* Verify that GTY((user)) works.  */

static void
test_user_struct ()
{
  root_user_struct_ptr = ggc_cleared_alloc <user_struct> ();
  test_struct *referenced = ggc_cleared_alloc <test_struct> ();
  root_user_struct_ptr->m_ptr = referenced;

  num_calls_to_user_gt_ggc_mx = 0;

  forcibly_ggc_collect ();

  ASSERT_TRUE (ggc_marked_p (root_user_struct_ptr));
  ASSERT_TRUE (ggc_marked_p (referenced));
  ASSERT_TRUE (num_calls_to_user_gt_ggc_mx > 0);
}



/* Smoketest to ensure that the tree type is marked.  */

static GTY(()) tree dummy_unittesting_tree;

static void
test_tree_marking ()
{
  dummy_unittesting_tree = build_int_cst (integer_type_node, 1066);

  forcibly_ggc_collect ();

  ASSERT_TRUE (ggc_marked_p (dummy_unittesting_tree));
}



/* Ideas for other tests:
   - pch-handling  */

namespace selftest {

/* Run all of the selftests within this file.  */

void
ggc_tests_c_tests ()
{
  test_basic_struct ();
  test_length ();
  test_union ();
  test_finalization ();
  test_deletable_global ();
  test_inheritance ();
  test_chain_next ();
  test_user_struct ();
  test_tree_marking ();
}

} // namespace selftest

#include "gt-ggc-tests.h"

#else /* #if CHECKING_P */

/* The #if CHECKING_P code above has various GTY-marked roots.
   gengtype has no knowledge of the preprocessor, and so detects
   these roots and writes them out to gt-ggc-tests.h.
   In a !CHECKING_P build we can ignore gt-ggc-tests.h, but the
   root tables are referenced in the various generated gtype-*.c
   files like this:

      ...snip...
      extern const struct ggc_root_tab gt_ggc_r_gt_ggc_tests_h[];
      ...snip...

      EXPORTED_CONST struct ggc_root_tab * const gt_ggc_rtab[] = {
        ...snip...
        gt_ggc_r_gt_ggc_tests_h,
        ...snip...
      };

    Hence to avoid a link failure, we provide dummy implementations
    of these root tables in an unchecked build.

    Note that these conditional roots imply that PCH files are
    incompatible between checked and unchecked builds.  */

EXPORTED_CONST struct ggc_root_tab gt_ggc_r_gt_ggc_tests_h[] = {
  LAST_GGC_ROOT_TAB
};

EXPORTED_CONST struct ggc_root_tab gt_ggc_rd_gt_ggc_tests_h[] = {
  LAST_GGC_ROOT_TAB
};

#endif /* #else clause of #if CHECKING_P */
