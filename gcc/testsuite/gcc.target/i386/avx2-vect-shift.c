/* { dg-do compile } */
/* { dg-options "-mavx2 -O3 -fdump-tree-vect-details" } */

#define N 32
typedef unsigned int u32;
u32 a[N];

void foo()
{
  int i;
  for (i=0; i<N;i++)
    a[i] = 1 << i;
}
/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */
