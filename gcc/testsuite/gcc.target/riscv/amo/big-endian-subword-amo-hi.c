/* { dg-do compile } */
/* { dg-additional-options "-mbig-endian" } */
/* Verify that subword atomic operations use XOR for big-endian halfword alignment.  */
/* { dg-final { scan-assembler "xori\\s+\[a-z0-9\]+,\[a-z0-9\]+,2" } } */

short atomic_fetch_add_hi(short *ptr, short val)
{
  __atomic_fetch_add(ptr, val, __ATOMIC_RELAXED);
  return val;
}
