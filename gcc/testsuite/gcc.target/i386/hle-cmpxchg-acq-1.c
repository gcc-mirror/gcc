/* { dg-do compile } */
/* { dg-options "-march=x86-64 -mhle" } */
/* { dg-final { scan-assembler "lock;?\[ \n\t\]+\(xacquire\|\.byte\[ \t\]+0xf2\)\[ \t\n\]+cmpxchg" } } */

int
hle_cmpxchg (int *p, int oldv, int newv)
{
  return __atomic_compare_exchange_n (p, &oldv, newv, 0, __ATOMIC_ACQUIRE | __ATOMIC_HLE_ACQUIRE, __ATOMIC_ACQUIRE);
}
