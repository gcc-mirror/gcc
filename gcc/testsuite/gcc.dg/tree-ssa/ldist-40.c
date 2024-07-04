/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ldist-details" } */

void copy_a_to_b (char * __restrict b, char * a, int n)
{
  for (int i = 0; i < n; ++i)
    b[i] = a[i];
}

/* { dg-final { scan-tree-dump "generated memcpy" "ldist" } } */
