/* { dg-do compile } */
/* { dg-options "-mhle" } */
/* { dg-final { scan-assembler "lock;?\[ \n\t\]+\(xrelease\|\.byte\[ \t\]+0xf3\)\[ \t\n\]+sub" } } */

void
hle_sub (int *p, int v)
{
  __atomic_fetch_sub (p, v, __ATOMIC_RELEASE | __ATOMIC_HLE_RELEASE);
}
