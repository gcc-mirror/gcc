// Splay tree utilities                                             -*- C++ -*-
// Copyright (C) 2020-2023 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.
//
// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.
//
// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#define INCLUDE_ALGORITHM
#define INCLUDE_ARRAY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "pretty-print.h"
#include "splay-tree-utils.h"
#include "selftest.h"

#if CHECKING_P
namespace {
// A simple test node for rootless_splay_tree.
struct rootless_test_node
{
  int data;
  rootless_test_node *m_parent;
  rootless_test_node *m_children[2];
};
}

namespace selftest {

// Random input data.
static const size_t MAX_DATA = 32768;
static const int data[] = {
  1379, 14643, 30579, 28160, 31750, 22280, 5502, 4720, 30075, 27595,
  8395, 19410, 518, 19709, 29694, 19865, 25372, 11752, 15485, 21547,
  25153, 25072, 10146, 3341, 15625, 3038, 10189, 19943, 1322, 11762,
  807, 430, 11284, 11841, 23965, 32008, 4547, 8087, 13225, 23054,
  22284, 13756, 2182, 26450, 30482, 32502, 23348, 20265, 29509, 3290,
  10807, 1242, 3212, 32178, 25354, 22032, 30509, 16157, 22432, 1295,
  8348, 23342, 24678, 193, 31016, 10316, 3872, 13521, 19211, 30594,
  12229, 4794, 25083, 16098, 28144, 27896, 4801, 20689, 31450, 15614,
  19597, 13731, 30309, 24846, 11042, 31929, 18306, 28520, 16907, 12488,
  15001, 18487, 3438, 1706, 4829, 20892, 6226, 18204, 15776, 30717,
  19398, 2480, 19434, 2838, 2605, 3994, 22538, 12269, 6486, 1314,
  30301, 9919, 31405, 30847, 25000, 24013, 22196, 30220, 31415, 14630,
  26319, 4880, 21292, 20217, 20078, 14679, 25686, 28675, 13883, 14853,
  2872, 2428, 3636, 14131, 2952, 2133, 4470, 25808, 12576, 31395,
  5938, 28393, 14553, 4494, 14928, 24310, 17394, 17436, 23385, 22792,
  9785, 13118, 22338, 23320, 27059, 17663, 16434, 14954, 16962, 31088,
  22247, 22600, 7980, 1344, 15635, 13611, 32739, 3283, 12924, 17904,
  28216, 7542, 9212, 28308, 18873, 3912, 5473, 4666, 11900, 21420,
  20072, 27662, 16445, 29848, 24444, 31668, 30664, 14287, 13754, 29276,
  21462, 25517, 17632, 8105, 32510, 16677, 11162, 20734, 26873, 5097
};

// Look up VALUE in TREE using the single-comparator lookup function.
static int
lookup1 (splay_tree<int> &tree, int value)
{
  auto compare = [&](splay_tree_node<int> *node)
    {
      return value - node->value ();
    };
  return tree.lookup (compare);
}

// Look up VALUE in TREE using the double-comparator lookup function.
static int
lookup2 (splay_tree<int> &tree, int value)
{
  auto want_something_smaller = [&](splay_tree_node<int> *node)
    {
      return value < node->value ();
    };
  auto want_something_bigger = [&](splay_tree_node<int> *node)
    {
      return value > node->value ();
    };
  return tree.lookup (want_something_smaller, want_something_bigger);
}

// Test printing TREE to a pretty printer.  Don't check the output against
// anything; just make sure that it doesn't crash.
static void
test_print (splay_tree<int> &tree)
{
  auto print_node = [](pretty_printer *pp, splay_tree_node<int> *node)
    {
      pp_decimal_int (pp, node->value ());
    };
  pretty_printer pp;
  tree.print (&pp, print_node);
}

// Test various lookups on TREE using LOOKUP, where lookup returns the
// same kind of value as the rooted_splay_tree lookup functions.
static void
test_lookup (splay_tree<int> &tree, int (*lookup) (splay_tree<int> &, int))
{
  // Look up values that are known to exist.
  for (int value : data)
    ASSERT_EQ (lookup (tree, value), 0);

  // Look up values that are 1 less than values that are known to exist.
  for (int value : data)
    {
      int result = lookup (tree, value - 1);
      if (result == 0)
	ASSERT_EQ (tree->value (), value - 1);
      else if (result < 0)
	// VALUE - 1 is less than the root.
	ASSERT_EQ (tree->value (), value);
      else if (result > 0)
	{
	  // VALUE - 1 is greater than the root.
	  ASSERT_TRUE (tree->value () < value - 1);
	  if (tree.splay_next_node ())
	    ASSERT_EQ (tree->value (), value);
	}
    }

  // Look up values that are 1 greater than values that are known to exist.
  for (int value : data)
    {
      int result = lookup (tree, value + 1);
      if (result == 0)
	ASSERT_EQ (tree->value (), value + 1);
      else if (result < 0)
	{
	  // VALUE + 1 is less than the root.
	  ASSERT_TRUE (tree->value () > value + 1);
	  if (tree.splay_prev_node ())
	    ASSERT_EQ (tree->value (), value);
	}
      else if (result > 0)
	// VALUE + 1 is greater than the root.
	ASSERT_EQ (tree->value (), value);
    }
}

// Run all tests for this module.
void
splay_tree_cc_tests ()
{
  obstack ob;
  gcc_obstack_init (&ob);

  // Build up the splay tree.
  splay_tree<int> tree;
  for (int value : data)
    {
      auto *node = XOBNEW (&ob, splay_tree_node<int>);
      new (node) splay_tree_node<int> (value);
      auto compare = [&](splay_tree_node<int> *other_node)
	{
	  return value - other_node->value ();
	};
      bool inserted = tree.insert (node, compare);
      ASSERT_TRUE (inserted);
    }

  // Test the single-comparator lookup function.
  test_lookup (tree, lookup1);

  // Sort the input data.
  std::array<int, ARRAY_SIZE (data)> sorted;
  std::copy (data, data + ARRAY_SIZE (data), sorted.begin ());
  std::sort (sorted.begin (), sorted.end ());

  // Iterate over the tree in ascending order.
  tree.splay_min_node ();
  bool result = true;
  for (int value : sorted)
    {
      ASSERT_TRUE (result);
      ASSERT_EQ (tree->value (), value);
      result = tree.splay_next_node ();
    }
  ASSERT_FALSE (result);
  ASSERT_EQ (tree.min_node ()->value (), sorted.front ());

  // Test the double-comparator lookup function.
  test_lookup (tree, lookup2);

  // Test printing the tree now, while it's still bushy.
  test_print (tree);

  // Iterate over the tree in descending order.
  tree.splay_max_node ();
  result = true;
  for (auto it = sorted.rbegin (); it != sorted.rend (); ++it)
    {
      ASSERT_TRUE (result);
      ASSERT_EQ (tree->value (), *it);
      result = tree.splay_prev_node ();
    }
  ASSERT_FALSE (result);
  ASSERT_EQ (tree.max_node ()->value (), sorted.back ());

  // Try splitting the tree into three.
  int mid_min = sorted[sorted.size () / 3];
  int mid_max = sorted[sorted.size () * 2 / 3];
  ASSERT_EQ (lookup1 (tree, mid_min), 0);
  splay_tree<int> left = tree.split_before_root ();
  ASSERT_EQ (lookup1 (tree, mid_max), 0);
  splay_tree<int> right = tree.split_after_root ();

  // Test removing all the nodes from their respective trees.
  for (int value : data)
    {
      splay_tree<int> &t = (value < mid_min ? left
			    : value > mid_max ? right : tree);
      ASSERT_EQ (lookup1 (t, value), 0);
      t.remove_root ();
    }
  ASSERT_EQ (left.root (), nullptr);
  ASSERT_EQ (tree.root (), nullptr);
  ASSERT_EQ (right.root (), nullptr);

  using rootless = default_rootless_splay_tree<rootless_test_node *>;

  // Build a tree in ascending order with the lowest element as the root.
  auto *nodes = XOBNEWVEC (&ob, rootless_test_node *, MAX_DATA);
  rootless_test_node *parent = nullptr;
  for (int data : sorted)
    {
      auto *node = XOBNEW (&ob, rootless_test_node);
      new (node) rootless_test_node ();
      node->data = data;
      nodes[data] = node;
      if (parent)
	rootless::insert_child (parent, 1, node);
      parent = node;
    }

  // Try comparing nodes to make sure that their order matches the data.
  for (size_t i = 1; i < ARRAY_SIZE (data); ++i)
    {
      int data1 = data[i - 1];
      int data2 = data[i];
      int comparison = rootless::compare_nodes (nodes[data1], nodes[data2]);
      if (data1 < data2)
	ASSERT_TRUE (comparison < 0);
      else if (data1 > data2)
	ASSERT_TRUE (comparison > 0);
      else
	ASSERT_EQ (comparison, 0);
    }

  obstack_free (&ob, nullptr);
}
}
#endif // CHECKING_P
