/* { dg-do compile } */
/* { dg-options "-mhle" } */
/* { dg-final { scan-assembler "lock;?\[ \n\t\]+\(xacquire\|\.byte\[ \t\]+0xf2\)\[ \t\n\]+xor" } } */

void
hle_xor (int *p, int v)
{
  __atomic_fetch_xor (p, v, __ATOMIC_ACQUIRE | __ATOMIC_HLE_ACQUIRE);
}
