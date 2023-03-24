/* { dg-do run } */
/* { dg-options "-O0 -fopenmp-simd" } */

#include <stdio.h>

#define ASSERT_EQ(var, val) if (var != val) { fprintf (stderr, "%s:%d: Unexpected value %d\n", __FILE__, __LINE__, var); \
    __builtin_abort (); }

#define ASSERT_EQ_PTR(var, ptr) if (var != ptr) { fprintf (stderr, "%s:%d: Unexpected value %p\n", __FILE__, __LINE__, var); \
    __builtin_abort (); }

int
test1 (int data[10])
{
  int iter = 0;
  int *i;
  #pragma omp tile sizes(5)
  for (i = data; i < data + 10 ; i++)
	{
	  ASSERT_EQ (*i, data[iter]);
	  ASSERT_EQ_PTR (i, data + iter);
	  iter++;
	}

  ASSERT_EQ_PTR (i, data + 10)
  return iter;
}

int
test2 (int data[10])
{
  int iter = 0;
  int *i;
  #pragma omp tile sizes(5)
  for (i = data; i < data + 10 ; i=i+2)
	{
	  ASSERT_EQ_PTR (i, data + 2 * iter);
	  ASSERT_EQ (*i, data[2 * iter]);
	  iter++;
	}

  ASSERT_EQ_PTR (i, data + 10)
  return iter;
}

int
test3 (int data[10])
{
  int iter = 0;
  int *i;
  #pragma omp tile sizes(5)
  for (i = data; i <= data + 9 ; i=i+2)
	{
	  ASSERT_EQ (*i, data[2 * iter]);
	  iter++;
	}

  ASSERT_EQ_PTR (i, data + 10)
  return iter;
}

int
test4 (int data[10])
{
  int iter = 0;
  int *i;
  #pragma omp tile sizes(5)
  for (i = data; i != data + 10 ; i=i+1)
	{
	  ASSERT_EQ (*i, data[iter]);
	  iter++;
	}

  ASSERT_EQ_PTR (i, data + 10)
  return iter;
}

int
test5 (int data[10])
{
  int iter = 0;
  int *i;
  #pragma omp tile sizes(3)
  for (i = data + 9; i >= data ; i--)
	{
	  ASSERT_EQ (*i, data[9 - iter]);
	  iter++;
	}

  ASSERT_EQ_PTR (i, data - 1)
  return iter;
}

int
test6 (int data[10])
{
  int iter = 0;
  int *i;
  #pragma omp tile sizes(3)
  for (i = data + 9; i > data - 1 ; i--)
	{
	  ASSERT_EQ (*i, data[9 - iter]);
	  iter++;
	}

  ASSERT_EQ_PTR (i, data - 1)
  return iter;
}

int
test7 (int data[10])
{
  int iter = 0;
  #pragma omp tile sizes(1)
  for (int *i = data + 9; i != data - 1 ; i--)
	{
	  ASSERT_EQ (*i, data[9 - iter]);
	  iter++;
	}

  return iter;
}

int
main ()
{
  int iter_count;
  int data[10] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };

  iter_count = test1 (data);
  ASSERT_EQ (iter_count, 10);

  iter_count = test2 (data);
  ASSERT_EQ (iter_count, 5);

  iter_count = test3 (data);
  ASSERT_EQ (iter_count, 5);

  iter_count = test4 (data);
  ASSERT_EQ (iter_count, 10);

  iter_count = test5 (data);
  ASSERT_EQ (iter_count, 10);

  iter_count = test6 (data);
  ASSERT_EQ (iter_count, 10);

  iter_count = test7 (data);
  ASSERT_EQ (iter_count, 10);
}
