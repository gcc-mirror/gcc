/* { dg-options "-O2 -msoft-stack" } */

int
f (void)
{
  int a = 0;
  return __sync_lock_test_and_set (&a, 1);
}

/* { dg-final { scan-assembler-times "atom.exch" 1 } } */

