/* { dg-do run } */
/* { dg-options "-O0 -fopenmp-simd" } */

#include <stdio.h>

#define ASSERT_EQ(var, val) \
  do									\
    {									\
      if ((var) != (val))						\
	{								\
	  fprintf (stderr, "%s:%d: Unexpected value %d, expected %d\n",	\
		   __FILE__, __LINE__, (var), (val));			\
	  __builtin_abort ();						\
	}								\
    }									\
  while (0)

int
test1 (void)
{
  int iter = 0;
  int i;
#pragma omp tile sizes(3)
  for (i = 0; i < 10; i = i + 2)
    {
      ASSERT_EQ (i, iter);
      iter = iter + 2;
    }

  ASSERT_EQ (i, 10);
  return iter;
}

int
test2 (void)
{
  int iter = 0;
  int i;
#pragma omp tile sizes(3)
  for (i = 0; i < 10; i = i + 2)
    {
      ASSERT_EQ (i, iter);
      iter = iter + 2;
    }

  ASSERT_EQ (i, 10);
  return iter;
}

int
test3 (void)
{
  int iter = 0;
  int i;
#pragma omp tile sizes(8)
  for (i = 0; i < 10; i = i + 2)
    {
      ASSERT_EQ (i, iter);
      iter = iter + 2;
    }

  ASSERT_EQ (i, 10);
  return iter;
}

int
test4 (void)
{
  int iter = 10;
  int i;
#pragma omp tile sizes(8)
  for (i = 10; i > 0; i = i - 2)
    {
      ASSERT_EQ (i, iter);
      iter = iter - 2;
    }
  ASSERT_EQ (i, 0);
  return iter;
}

int
test5 (void)
{
  int iter = 10;
  int i;
#pragma omp tile sizes(71)
  for (i = 10; i > 0; i = i - 2)
    {
      ASSERT_EQ (i, iter);
      iter = iter - 2;
    }

  ASSERT_EQ (i, 0);
  return iter;
}

int
test6 (void)
{
  int iter = 10;
  int i;
#pragma omp tile sizes(1)
  for (i = 10; i > 0; i = i - 2)
    {
      ASSERT_EQ (i, iter);
      iter = iter - 2;
    }
  ASSERT_EQ (i, 0);
  return iter;
}

int
test7 (void)
{
  int iter = 5;
  int i;
#pragma omp tile sizes(2)
  for (i = 5; i < -5; i = i - 3)
    {
      fprintf (stderr, "%d\n", i);
      __builtin_abort ();
      iter = iter - 3;
    }

  /* No iteration expected */
  return iter;
}

int
test8 (void)
{
  int iter = 5;
  int i;
#pragma omp tile sizes(2)
  for (i = 5; i > -5; i = i - 3)
    {
      ASSERT_EQ (i, iter);
      /* Expect only first iteration of the last tile to execute */
      if (iter != -4)
	iter = iter - 3;
    }

  ASSERT_EQ (i, -7);
  return iter;
}


int
test9 (void)
{
  int iter = 5;
  int i;
#pragma omp tile sizes(5)
  for (i = 5; i >= -5; i = i - 4)
    {
      ASSERT_EQ (i, iter);
      /* Expect only first iteration of the last tile to execute */
      if (iter != - 3)
	iter = iter - 4;
    }

  ASSERT_EQ (i, -7);
  return iter;
}

int
test10 (void)
{
  int iter = 5;
  int i;
#pragma omp tile sizes(5)
  for (i = 5; i >= -5; i--)
    {
      ASSERT_EQ (i, iter);
      iter--;
    }

  ASSERT_EQ (i, -6);
  return iter;
}

int
test11 (void)
{
  int iter = 5;
  int i;
#pragma omp tile sizes(15)
  for (i = 5; i != -5; i--)
    {
      ASSERT_EQ (i, iter);
      iter--;
    }
  ASSERT_EQ (i, -5);
  return iter;
}

int
test12 (void)
{
  int iter = 0;
  unsigned i;
#pragma omp tile sizes(3)
  for (i = 0; i != 5; i++)
    {
      ASSERT_EQ (i, iter);
      iter++;
    }

  ASSERT_EQ (i, 5);
  return iter;
}

int
test13 (void)
{
  int iter = -5;
  long long unsigned int i = 42;
#pragma omp tile sizes(15)
  for (int i = -5; i < 5; i = i + 3)
    {
      ASSERT_EQ (i, iter);
      iter += 3;
    }

  ASSERT_EQ (i, 42);
  return iter;
}

int
test14 (unsigned init, int step)
{
  int iter = init;
  long long unsigned int i;
#pragma omp tile sizes(8)
  for (i = init; i < 2 * init; i = i + step)
    iter++;

  if (init)
    ASSERT_EQ (i, 2 * init + (init == 5));
  return iter;
}

int
test15 (unsigned init, int step)
{
  int iter = init;
  int i;
#pragma omp tile sizes(8)
  for (unsigned i = init; i > 2 * init; i = i + step)
    iter++;

  return iter;
}

int
main ()
{
  int last_iter;

  last_iter = test1 ();
  ASSERT_EQ (last_iter, 10);

  last_iter = test2 ();
  ASSERT_EQ (last_iter, 10);

  last_iter = test3 ();
  ASSERT_EQ (last_iter, 10);

  last_iter = test4 ();
  ASSERT_EQ (last_iter, 0);

  last_iter = test5 ();
  ASSERT_EQ (last_iter, 0);

  last_iter = test6 ();
  ASSERT_EQ (last_iter, 0);

  last_iter = test7 ();
  ASSERT_EQ (last_iter, 5);

  last_iter = test8 ();
  ASSERT_EQ (last_iter, -4);

  last_iter = test9 ();
  ASSERT_EQ (last_iter, -3);

  last_iter = test10 ();
  ASSERT_EQ (last_iter, -6);

  last_iter = test11 ();
  ASSERT_EQ (last_iter, -5);

  last_iter = test12 ();
  ASSERT_EQ (last_iter, 5);

  last_iter = test13 ();
  ASSERT_EQ (last_iter, 7);

  last_iter = test14 (0, 1);
  ASSERT_EQ (last_iter, 0);

  last_iter = test14 (0, -1);
  ASSERT_EQ (last_iter, 0);

  last_iter = test14 (8, 2);
  ASSERT_EQ (last_iter, 12);

  last_iter = test14 (5, 3);
  ASSERT_EQ (last_iter, 7);

  last_iter = test15 (8, -1);
  ASSERT_EQ (last_iter, 8);

  last_iter = test15 (8, -2);
  ASSERT_EQ (last_iter, 8);

  last_iter = test15 (5, -3);
  ASSERT_EQ (last_iter, 5);
  return 0;
}
