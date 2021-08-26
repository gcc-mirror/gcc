/* Data structure for the modref pass.
   Copyright (C) 2020-2021 Free Software Foundation, Inc.
   Contributed by David Cepelik and Jan Hubicka

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
#include "backend.h"
#include "tree.h"
#include "ipa-modref-tree.h"
#include "selftest.h"

#if CHECKING_P

namespace selftest {

static void
test_insert_search_collapse ()
{
  modref_base_node<alias_set_type> *base_node;
  modref_ref_node<alias_set_type> *ref_node;
  modref_access_node a = unspecified_modref_access_node;

  modref_tree<alias_set_type> *t = new modref_tree<alias_set_type>(1, 2, 2);
  ASSERT_FALSE (t->every_base);

  /* Insert into an empty tree.  */
  t->insert (1, 2, a, false);
  ASSERT_NE (t->bases, NULL);
  ASSERT_EQ (t->bases->length (), 1);
  ASSERT_FALSE (t->every_base);
  ASSERT_EQ (t->search (2), NULL);

  base_node = t->search (1);
  ASSERT_NE (base_node, NULL);
  ASSERT_EQ (base_node->base, 1);
  ASSERT_NE (base_node->refs, NULL);
  ASSERT_EQ (base_node->refs->length (), 1);
  ASSERT_EQ (base_node->search (1), NULL);

  ref_node = base_node->search (2);
  ASSERT_NE (ref_node, NULL);
  ASSERT_EQ (ref_node->ref, 2);

  /* Insert when base exists but ref does not.  */
  t->insert (1, 3, a, false);
  ASSERT_NE (t->bases, NULL);
  ASSERT_EQ (t->bases->length (), 1);
  ASSERT_EQ (t->search (1), base_node);
  ASSERT_EQ (t->search (2), NULL);
  ASSERT_NE (base_node->refs, NULL);
  ASSERT_EQ (base_node->refs->length (), 2);

  ref_node = base_node->search (3);
  ASSERT_NE (ref_node, NULL);

  /* Insert when base and ref exist, but access is not dominated by nor
     dominates other accesses.  */
  t->insert (1, 2, a, false);
  ASSERT_EQ (t->bases->length (), 1);
  ASSERT_EQ (t->search (1), base_node);

  ref_node = base_node->search (2);
  ASSERT_NE (ref_node, NULL);

  /* Insert when base and ref exist and access is dominated.  */
  t->insert (1, 2, a, false);
  ASSERT_EQ (t->search (1), base_node);
  ASSERT_EQ (base_node->search (2), ref_node);

  /* Insert ref to trigger ref list collapse for base 1.  */
  t->insert (1, 4, a, false);
  ASSERT_EQ (t->search (1), base_node);
  ASSERT_EQ (base_node->refs, NULL);
  ASSERT_EQ (base_node->search (2), NULL);
  ASSERT_EQ (base_node->search (3), NULL);
  ASSERT_TRUE (base_node->every_ref);

  /* Further inserts to collapsed ref list are ignored.  */
  t->insert (1, 5, a, false);
  ASSERT_EQ (t->search (1), base_node);
  ASSERT_EQ (base_node->refs, NULL);
  ASSERT_EQ (base_node->search (2), NULL);
  ASSERT_EQ (base_node->search (3), NULL);
  ASSERT_TRUE (base_node->every_ref);

  /* Insert base to trigger base list collapse.  */
  t->insert (5, 0, a, false);
  ASSERT_TRUE (t->every_base);
  ASSERT_EQ (t->bases, NULL);
  ASSERT_EQ (t->search (1), NULL);

  /* Further inserts to collapsed base list are ignored.  */
  t->insert (7, 8, a, false);
  ASSERT_TRUE (t->every_base);
  ASSERT_EQ (t->bases, NULL);
  ASSERT_EQ (t->search (1), NULL);

  delete t;
}

static void
test_merge ()
{
  modref_tree<alias_set_type> *t1, *t2;
  modref_base_node<alias_set_type> *base_node;
  modref_access_node a = unspecified_modref_access_node;

  t1 = new modref_tree<alias_set_type>(3, 4, 1);
  t1->insert (1, 1, a, false);
  t1->insert (1, 2, a, false);
  t1->insert (1, 3, a, false);
  t1->insert (2, 1, a, false);
  t1->insert (3, 1, a, false);

  t2 = new modref_tree<alias_set_type>(10, 10, 10);
  t2->insert (1, 2, a, false);
  t2->insert (1, 3, a, false);
  t2->insert (1, 4, a, false);
  t2->insert (3, 2, a, false);
  t2->insert (3, 3, a, false);
  t2->insert (3, 4, a, false);
  t2->insert (3, 5, a, false);

  t1->merge (t2, NULL, false);

  ASSERT_FALSE (t1->every_base);
  ASSERT_NE (t1->bases, NULL);
  ASSERT_EQ (t1->bases->length (), 3);

  base_node = t1->search (1);
  ASSERT_NE (base_node->refs, NULL);
  ASSERT_FALSE (base_node->every_ref);
  ASSERT_EQ (base_node->refs->length (), 4);

  base_node = t1->search (2);
  ASSERT_NE (base_node->refs, NULL);
  ASSERT_FALSE (base_node->every_ref);
  ASSERT_EQ (base_node->refs->length (), 1);

  base_node = t1->search (3);
  ASSERT_EQ (base_node->refs, NULL);
  ASSERT_TRUE (base_node->every_ref);

  delete t1;
  delete t2;
}


void
ipa_modref_tree_c_tests ()
{
  test_insert_search_collapse ();
  test_merge ();
}

} // namespace selftest

#endif

void
gt_ggc_mx (modref_tree < int >*const &tt)
{
  if (tt->bases)
    {
      ggc_test_and_set_mark (tt->bases);
      gt_ggc_mx (tt->bases);
    }
}

void
gt_ggc_mx (modref_tree < tree_node * >*const &tt)
{
  if (tt->bases)
    {
      ggc_test_and_set_mark (tt->bases);
      gt_ggc_mx (tt->bases);
    }
}

void gt_pch_nx (modref_tree<int>* const&) {}
void gt_pch_nx (modref_tree<tree_node*>* const&) {}
void gt_pch_nx (modref_tree<int>* const&, gt_pointer_operator, void *) {}
void gt_pch_nx (modref_tree<tree_node*>* const&, gt_pointer_operator, void *) {}

void gt_ggc_mx (modref_base_node<int>* &b)
{
  ggc_test_and_set_mark (b);
  if (b->refs)
    {
      ggc_test_and_set_mark (b->refs);
      gt_ggc_mx (b->refs);
    }
}

void gt_ggc_mx (modref_base_node<tree_node*>* &b)
{
  ggc_test_and_set_mark (b);
  if (b->refs)
    {
      ggc_test_and_set_mark (b->refs);
      gt_ggc_mx (b->refs);
    }
  if (b->base)
    gt_ggc_mx (b->base);
}

void gt_pch_nx (modref_base_node<int>*) {}
void gt_pch_nx (modref_base_node<tree_node*>*) {}
void gt_pch_nx (modref_base_node<int>*, gt_pointer_operator, void *) {}
void gt_pch_nx (modref_base_node<tree_node*>*, gt_pointer_operator, void *) {}

void gt_ggc_mx (modref_ref_node<int>* &r)
{
  ggc_test_and_set_mark (r);
  if (r->accesses)
    {
      ggc_test_and_set_mark (r->accesses);
      gt_ggc_mx (r->accesses);
    }
}

void gt_ggc_mx (modref_ref_node<tree_node*>* &r)
{
  ggc_test_and_set_mark (r);
  if (r->accesses)
    {
      ggc_test_and_set_mark (r->accesses);
      gt_ggc_mx (r->accesses);
    }
  if (r->ref)
    gt_ggc_mx (r->ref);
}

void gt_pch_nx (modref_ref_node<int>* ) {}
void gt_pch_nx (modref_ref_node<tree_node*>*) {}
void gt_pch_nx (modref_ref_node<int>*, gt_pointer_operator, void *) {}
void gt_pch_nx (modref_ref_node<tree_node*>*, gt_pointer_operator, void *) {}

void gt_ggc_mx (modref_access_node &)
{
}
