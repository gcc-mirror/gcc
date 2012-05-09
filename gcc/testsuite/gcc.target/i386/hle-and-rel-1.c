/* { dg-do compile } */
/* { dg-options "-mhle" } */
/* { dg-final { scan-assembler "lock;?\[ \n\t\]+\(xrelease\|\.byte\[ \t\]+0xf3\)\[ \t\n\]+and" } } */

void
hle_and (int *p, int v)
{
  __atomic_fetch_and (p, v, __ATOMIC_RELEASE | __ATOMIC_HLE_RELEASE);
}
