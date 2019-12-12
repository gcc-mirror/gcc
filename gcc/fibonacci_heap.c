/* Fibonacci heap for GNU compiler.
   Copyright (C) 2016-2019 Free Software Foundation, Inc.
   Contributed by Martin Liska <mliska@suse.cz>

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
#include "alloc-pool.h"
#include "fibonacci_heap.h"
#include "selftest.h"

#if CHECKING_P

namespace selftest {

/* Selftests.  */

/* Verify that operations with empty heap work.  */

typedef fibonacci_node <int, int> int_heap_node_t;
typedef fibonacci_heap <int, int> int_heap_t;

static void
test_empty_heap ()
{
  pool_allocator allocator ("fibheap test", sizeof (int_heap_node_t));
  int_heap_t *h1 = new int_heap_t (INT_MIN, &allocator);

  ASSERT_TRUE (h1->empty ());
  ASSERT_EQ (0, h1->nodes ());
  ASSERT_EQ (NULL, h1->min ());

  int_heap_t *h2 = new int_heap_t (INT_MIN, &allocator);

  int_heap_t *r = h1->union_with (h2);
  ASSERT_TRUE (r->empty ());
  ASSERT_EQ (0, r->nodes ());
  ASSERT_EQ (NULL, r->min ());

  delete r;
}

#define TEST_HEAP_N 100
#define TEST_CALCULATE_VALUE(i)  ((3 * i) + 10000)

/* Verify heap basic operations.  */

static void
test_basic_heap_operations ()
{
  int values[TEST_HEAP_N];
  int_heap_t *h1 = new int_heap_t (INT_MIN);

  for (unsigned i = 0; i < TEST_HEAP_N; i++)
    {
      values[i] = TEST_CALCULATE_VALUE (i);
      ASSERT_EQ (i, h1->nodes ());
      h1->insert (i, &values[i]);
      ASSERT_EQ (0, h1->min_key ());
      ASSERT_EQ (values[0], *h1->min ());
    }

  for (unsigned i = 0; i < TEST_HEAP_N; i++)
    {
      ASSERT_EQ (TEST_HEAP_N - i, h1->nodes ());
      ASSERT_EQ ((int)i, h1->min_key ());
      ASSERT_EQ (values[i], *h1->min ());

      h1->extract_min ();
    }

  ASSERT_TRUE (h1->empty ());

  delete h1;
}

/* Builds a simple heap with values in interval 0..TEST_HEAP_N-1, where values
   of each key is equal to 3 * key + 10000.  BUFFER is used as a storage
   of values and NODES points to inserted nodes.  */

static int_heap_t *
build_simple_heap (int *buffer, int_heap_node_t **nodes)
{
  int_heap_t *h = new int_heap_t (INT_MIN);

  for (unsigned i = 0; i < TEST_HEAP_N; i++)
    {
      buffer[i] = TEST_CALCULATE_VALUE (i);
      nodes[i] = h->insert (i, &buffer[i]);
    }

  return h;
}

/* Verify that fibonacci_heap::replace_key works.  */

static void
test_replace_key ()
{
  int values[TEST_HEAP_N];
  int_heap_node_t *nodes[TEST_HEAP_N];

  int_heap_t *heap = build_simple_heap (values, nodes);

  int N = 10;
  for (unsigned i = 0; i < (unsigned)N; i++)
    heap->replace_key (nodes[i], 100 * 1000 + i);

  ASSERT_EQ (TEST_HEAP_N, heap->nodes ());
  ASSERT_EQ (N, heap->min_key ());
  ASSERT_EQ (TEST_CALCULATE_VALUE (N), *heap->min ());

  for (int i = 0; i < TEST_HEAP_N - 1; i++)
    heap->extract_min ();

  ASSERT_EQ (1, heap->nodes ());
  ASSERT_EQ (100 * 1000 + N - 1, heap->min_key ());

  delete heap;
}

/* Verify that heap can handle duplicate keys.  */

static void
test_duplicate_keys ()
{
  int values[3 * TEST_HEAP_N];
  int_heap_t *heap = new int_heap_t (INT_MIN);

  for (unsigned i = 0; i < 3 * TEST_HEAP_N; i++)
    {
      values[i] = TEST_CALCULATE_VALUE (i);
      heap->insert (i / 3, &values[i]);
    }

  ASSERT_EQ (3 * TEST_HEAP_N, heap->nodes ());
  ASSERT_EQ (0, heap->min_key ());
  ASSERT_EQ (TEST_CALCULATE_VALUE (0), *heap->min ());

  for (unsigned i = 0; i < 9; i++)
    heap->extract_min ();

  for (unsigned i = 0; i < 3; i++)
    {
      ASSERT_EQ (3, heap->min_key ());
      heap->extract_min ();
    }

  delete heap;
}

/* Verify that heap can handle union.  */

static void
test_union ()
{
  int value = 777;
  pool_allocator allocator ("fibheap test", sizeof (int_heap_node_t));

  int_heap_t *heap1 = new int_heap_t (INT_MIN, &allocator);
  for (unsigned i = 0; i < 2 * TEST_HEAP_N; i++)
    heap1->insert (i, &value);

  int_heap_t *heap2 = new int_heap_t (INT_MIN, &allocator);
  for (unsigned i = 2 * TEST_HEAP_N; i < 3 * TEST_HEAP_N; i++)
    heap2->insert (i, &value);

  int_heap_t *union_heap = heap1->union_with (heap2);

  for (int i = 0; i < 3 * TEST_HEAP_N; i++)
    {
      ASSERT_EQ (i, union_heap->min_key ());
      union_heap->extract_min ();
    }

  delete union_heap;
}

/* Verify that heap can handle union with a heap having exactly the same
   keys.  */

static void
test_union_of_equal_heaps ()
{
  int value = 777;
  pool_allocator allocator ("fibheap test", sizeof (int_heap_node_t));

  int_heap_t *heap1 = new int_heap_t (INT_MIN, &allocator);
  for (unsigned i = 0; i < TEST_HEAP_N; i++)
    heap1->insert (i, &value);

  int_heap_t *heap2 = new int_heap_t (INT_MIN, &allocator);
  for (unsigned i = 0; i < TEST_HEAP_N; i++)
    heap2->insert (i, &value);

  int_heap_t *union_heap = heap1->union_with (heap2);

  for (int i = 0; i < TEST_HEAP_N; i++)
    for (int j = 0; j < 2; j++)
    {
      ASSERT_EQ (i, union_heap->min_key ());
      union_heap->extract_min ();
    }

  delete union_heap;
}

/* Dummy struct for testing.  */

class heap_key
{
public:
  heap_key (int k): key (k)
  {
  }

  int key;

  bool operator< (const heap_key &other) const
  {
    return key > other.key;
  }

  bool operator== (const heap_key &other) const
  {
    return key == other.key;
  }

  bool operator> (const heap_key &other) const
  {
    return !(*this == other || *this < other);
  }
};

typedef fibonacci_heap<heap_key, int> class_fibonacci_heap_t;

/* Verify that heap can handle a struct as key type.  */

static void
test_struct_key ()
{
  int value = 123456;
  class_fibonacci_heap_t *heap = new class_fibonacci_heap_t (INT_MIN);

  heap->insert (heap_key (1), &value);
  heap->insert (heap_key (10), &value);
  heap->insert (heap_key (100), &value);
  heap->insert (heap_key (1000), &value);

  ASSERT_EQ (1000, heap->min_key ().key);
  ASSERT_EQ (4, heap->nodes ());
  heap->extract_min ();
  heap->extract_min ();
  ASSERT_EQ (10, heap->min_key ().key);
  heap->extract_min ();
  ASSERT_EQ (&value, heap->min ());
  heap->extract_min ();
  ASSERT_TRUE (heap->empty ());

  delete heap;
}

/* Run all of the selftests within this file.  */

void
fibonacci_heap_c_tests ()
{
  test_empty_heap ();
  test_basic_heap_operations ();
  test_replace_key ();
  test_duplicate_keys ();
  test_union ();
  test_union_of_equal_heaps ();
  test_struct_key ();
}

} // namespace selftest

#endif /* #if CHECKING_P */
