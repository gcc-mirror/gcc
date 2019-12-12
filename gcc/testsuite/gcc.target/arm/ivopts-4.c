/* { dg-do assemble } */
/* { dg-options "-Os -fdump-tree-ivopts -save-temps" } */

extern unsigned int foo (int*) __attribute__((pure));

unsigned int
tr2 (int array[], int n)
{
  unsigned int sum = 0;
  int x;
  if (n > 0)
    for (x = 0; x < n; x++)
      sum += foo (&array[x]);
  return sum;
}

/* { dg-final { scan-tree-dump-times "PHI <ivtmp" 1 "ivopts"} } */
/* { dg-final { scan-tree-dump-times "PHI <x" 0 "ivopts"} } */
/* { dg-final { scan-tree-dump-times ", x" 0 "ivopts"} } */
/* { dg-final { object-size text <= 36 { target { arm_thumb2 && { ! arm*-*-uclinuxfdpiceabi } } } } } */
