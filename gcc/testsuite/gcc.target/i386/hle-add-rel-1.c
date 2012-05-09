/* { dg-do compile } */
/* { dg-options "-mhle" } */
/* { dg-final { scan-assembler "lock;?\[ \n\t\]+\(xrelease\|\.byte\[ \t\]+0xf3\)\[ \t\n\]+add" } } */

void
hle_add (int *p, int v)
{
  __atomic_fetch_add (p, v, __ATOMIC_RELEASE | __ATOMIC_HLE_RELEASE);
}
