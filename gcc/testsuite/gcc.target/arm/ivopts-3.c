/* { dg-options "-Os -fdump-tree-ivopts" } */

extern unsigned int foo2 (short*) __attribute__((pure));

unsigned int
tr3 (short array[], unsigned int n)
{
  int sum = 0;
  unsigned int x;
  for (x = 0; x < n; ++x)
    sum += foo2 (&array[x]);
  return sum;
}

/* { dg-final { scan-tree-dump-times "PHI <ivtmp" 1 "ivopts"} } */
/* { dg-final { scan-tree-dump-times "PHI <x" 0 "ivopts"} } */
/* { dg-final { scan-tree-dump-times ", x" 0 "ivopts"} } */
