/* { dg-do assemble } */
/* { dg-options "-Os -mthumb -fdump-tree-ivopts -save-temps" } */

extern unsigned int foo (int*) __attribute__((pure));

unsigned int
tr1 (int array[], unsigned int n)
{
  int sum = 0;
  unsigned int x;
  x = 0;
  while (1)
    {
      sum += foo (&array[x]);
      if (!(x < n))
        break;
      x++;
    }
  return sum;
}

/* { dg-final { scan-tree-dump-times "PHI <ivtmp" 1 "ivopts"} } */
/* { dg-final { scan-tree-dump-times "PHI <x" 0 "ivopts"} } */
/* { dg-final { scan-tree-dump-times ", x" 0 "ivopts"} } */
/* { dg-final { object-size text <= 30 { target arm_thumb2_ok } } } */
/* { dg-final { cleanup-tree-dump "ivopts" } } */
