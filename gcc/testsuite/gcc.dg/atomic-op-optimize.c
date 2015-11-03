/* Both these atomic operations should be optimized to an exchange operation.
   Test that it at happens on x86 by making sure there are 2 xchg's and no
   compare_exchange loop.  */

/* { dg-do compile { target { i?86-*-* x86_64-*-* } } } */
/* { dg-require-effective-target sync_int_long } */
/* { dg-final { scan-assembler-times "cmpxchg" 0 } } */
/* { dg-final { scan-assembler-times "xchg" 2 } } */

int x;

int f()
{
  return __atomic_fetch_and (&x, 0, __ATOMIC_RELAXED);
}

int g()
{
  return __atomic_fetch_or (&x, -1, __ATOMIC_RELAXED);
}
