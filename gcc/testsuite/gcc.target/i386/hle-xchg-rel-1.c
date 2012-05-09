/* { dg-do compile } */
/* { dg-options "-mhle" } */
/* { dg-final { scan-assembler "\[ \n\t\]+\(xrelease\|\.byte\[ \t\]+0xf3\)\[ \t\n\]+xchg" } } */

int
hle_xchg (int *p, int v)
{
  return __atomic_exchange_n (p, v, __ATOMIC_RELEASE | __ATOMIC_HLE_RELEASE);
}
