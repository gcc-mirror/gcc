/* { dg-do compile } */
/* { dg-options "-march=x86-64 -mhle" } */
/* { dg-final { scan-assembler "lock;?\[ \n\t\]+\(xacquire\|\.byte\[ \t\]+0xf2\)\[ \t\n\]+xadd" } } */

int
hle_xadd (int *p, int v)
{
  return __atomic_fetch_add (p, v, __ATOMIC_ACQUIRE | __ATOMIC_HLE_ACQUIRE);
}
