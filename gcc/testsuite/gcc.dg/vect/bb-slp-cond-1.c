/* { dg-require-effective-target vect_condition } */
/* { dg-additional-options "-fdump-tree-vect-details" } */

#include "tree-vect.h"

#define N 128

__attribute__((noinline, noclone)) void
foo (int *a, int stride)
{
  int i;

  for (i = 0; i < N/stride; i++, a += stride)
   {
     a[0] = a[0] ? 1 : 5;
     a[1] = a[1] ? 2 : 6;
     a[2] = a[2] ? 3 : 7;
     a[3] = a[3] ? 4 : 8;
   }
}


int a[N];
int main ()
{
  int i;

  check_vect ();

  for (i = 0; i < N; i++)
    a[i] = i;

  foo (a, 4);

  for (i = 1; i < N; i++)
    if (a[i] != i%4 + 1)
      abort ();

  if (a[0] != 5)
    abort ();

  return 0;
}

/* Basic blocks of if-converted loops are vectorized from within the loop
   vectorizer pass.  In this case it is really a deficiency in loop
   vectorization data dependence analysis that causes us to require
   basic block vectorization in the first place.  */

/* { dg-final { scan-tree-dump-times "basic block vectorized" 1 "vect" { target vect_element_align } } } */

