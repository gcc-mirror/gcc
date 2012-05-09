/* { dg-do compile } */
/* { dg-options "-mhle" } */
/* { dg-final { scan-assembler "lock;?\[ \n\t\]+\(xacquire\|\.byte\[ \t\]+0xf2\)\[ \t\n\]+sub" } } */

void
hle_sub (int *p, int v)
{
  __atomic_fetch_sub (p, v, __ATOMIC_ACQUIRE | __ATOMIC_HLE_ACQUIRE);
}
