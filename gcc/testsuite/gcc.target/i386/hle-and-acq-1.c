/* { dg-do compile } */
/* { dg-options "-mhle" } */
/* { dg-final { scan-assembler "lock;?\[ \n\t\]+\(xacquire\|\.byte\[ \t\]+0xf2\)\[ \t\n\]+and" } } */

void
hle_and (int *p, int v)
{
  __atomic_fetch_and (p, v, __ATOMIC_ACQUIRE | __ATOMIC_HLE_ACQUIRE);
}
