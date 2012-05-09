/* { dg-do compile } */
/* { dg-options "-mhle" } */
/* { dg-final { scan-assembler "lock;?\[ \n\t\]+\(xacquire\|\.byte\[ \t\]+0xf2\)\[ \t\n\]+add" } } */

void
hle_add (int *p, int v)
{
  __atomic_fetch_add (p, v, __ATOMIC_ACQUIRE | __ATOMIC_HLE_ACQUIRE);
}
