/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-optimized -O3" } */

/* Test that functions that have SIMD clone counterparts are not
   cloned by IPA-cp.  For example, special_add() below has SIMD clones
   created for it.  However, if IPA-cp later decides to clone a
   specialization of special_add(x, 666) when analyzing fillit(), we
   will forever keep the vectorizer from using the SIMD versions of
   special_add in a loop.

   If IPA-CP gets taught how to adjust the SIMD clones as well, this
   test could be removed.  */

#pragma omp declare simd simdlen(4)
static int  __attribute__ ((noinline))
special_add (int x, int y)
{
  if (y == 666)
    return x + y + 123;
  else
    return x + y;
}

void fillit(int *tot)
{
  int i;

  for (i=0; i < 10000; ++i)
    tot[i] = special_add (i, 666);
}

/* { dg-final { scan-tree-dump-not "special_add.constprop" "optimized" } } */
