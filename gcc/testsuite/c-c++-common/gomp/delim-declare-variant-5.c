/* { dg-do compile } */

/* Check that the simd trait is rejected in the match clause for
   "begin declare variant".  */

int foo (int a)
{
  return a;
}

int bar (int x)
{
  return x;
}

#pragma omp begin declare variant match (construct={target, simd})  /* { dg-error "the 'simd' selector is not permitted" } */
int foo (int a)
{
  return a + 1;
}

int bar (int x)
{
  return x * 2;
}
#pragma omp end declare variant
