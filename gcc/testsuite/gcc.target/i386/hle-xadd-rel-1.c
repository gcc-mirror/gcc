/* { dg-do compile } */
/* { dg-options "-mhle" } */
/* { dg-final { scan-assembler "lock\[ \n\t\]+\(xrelease\|\.byte\[ \t\]+0xf2\)\[ \t\n\]+xadd" } } */

int
hle_xadd (int *p, int v)
{
  return __atomic_fetch_add (p, v, __ATOMIC_RELEASE | __ATOMIC_HLE_RELEASE);
}
