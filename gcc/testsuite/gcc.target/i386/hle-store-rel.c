/* { dg-do compile } */
/* { dg-options "-mhle" } */
/* { dg-final { scan-assembler "\[ \n\t\]+\(xrelease\|\.byte\[ \t\]+0xf3\)\[ \t\n\]+mov" } } */

void
hle_store (int *p, int v)
{
  __atomic_store_n (p, v, __ATOMIC_RELEASE | __ATOMIC_HLE_RELEASE);
}
