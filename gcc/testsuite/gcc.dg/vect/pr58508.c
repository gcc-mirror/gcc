/* { dg-do compile } */


/* The GCC vectorizer generates loop versioning for the following loop
   since there may exist aliasing between A and B.  The predicate checks
   if A may alias with B across all iterations.  Then for the loop in
   the true body, we can assert that *B is a loop invariant so that
   we can hoist the load of *B before the loop body.  */

void test1 (int* a, int* b)
{
  int i;
  for (i = 0; i < 100000; ++i)
    a[i] = *b + 1;
}

/* A test case with nested loops.  The load of b[j+1] in the inner
   loop should be hoisted.  */

void test2 (int* a, int* b)
{
  int i, j;
  for (j = 0; j < 100000; ++j)
    for (i = 0; i < 100000; ++i)
      a[i] = b[j+1] + 1;
}

/* A test case with ifcvt transformation.  */

void test3 (int* a, int* b)
{
  int i, t;
  for (i = 0; i < 10000; ++i)
    {
      if (*b > 0)
	t = *b * 2;
      else
	t = *b / 2;
      a[i] = t;
    }
}

/* A test case in which the store in the loop can be moved outside
   in the versioned loop with alias checks.  Note this loop won't
   be vectorized.  */

void test4 (int* a, int* b)
{
  int i;
  for (i = 0; i < 100000; ++i)
    *a += b[i];
}

/* A test case in which the load and store in the loop to b
   can be moved outside in the versioned loop with alias checks.
   Note this loop won't be vectorized.  */

void test5 (int* a, int* b)
{
  int i;
  for (i = 0; i < 100000; ++i)
    {
      *b += a[i];
      a[i] = *b;
    }
}

/* { dg-final { scan-tree-dump-times "hoist" 8 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
