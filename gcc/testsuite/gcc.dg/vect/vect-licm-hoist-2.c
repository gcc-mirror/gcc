/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-additional-options "-fdump-tree-lim2-details -fdump-tree-vect-details" } */

/* Negative test: ensure we don't incorrectly hoist when
   a store invalidates the loaded value.  */

#define N 32000

float a[N];

/* Should NOT hoist: a[0] = 5.0f breaks the SSA dependency.  */

void
test_unsafe_hoist (void)
{
  for (int i = 0; i < N; i++)
    {
      float x = a[0];
      a[i] = x;
      a[0] = 5.0f;
    }
}

/* { dg-final { scan-tree-dump-not "independent \\(constant-indexed load" "lim2" } } */

