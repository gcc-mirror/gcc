/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-additional-options "-fdump-tree-lim2-details -fdump-tree-vect-details" } */

/* Test vectorization of "self write" pattern: a[i] = a[0].
   LICM should hoist a[0] by recognizing that even when i==0 causes
   aliasing, the stored value equals the loaded value (via SSA).  */

#define N 32000

float a[N];

/* Should vectorize.  */

void
test_safe_hoist (void)
{
  for (int i = 0; i < N; i++)
    a[i] = a[0];
}

/* { dg-final { scan-tree-dump "loop vectorized" "vect" } } */
/* { dg-final { scan-tree-dump "independent \\(self write\\)" "lim2" } } */

