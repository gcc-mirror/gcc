/* { dg-do compile } */
/* { dg-options "-mhle" } */
/* { dg-final { scan-assembler "\[ \n\t\]+\(xacquire\|\.byte\[ \t\]+0xf2\)\[ \t\n\]+xchg" } } */

int
hle_xchg (int *p, int v)
{
  return __atomic_exchange_n (p, v, __ATOMIC_ACQUIRE | __ATOMIC_HLE_ACQUIRE);
}
