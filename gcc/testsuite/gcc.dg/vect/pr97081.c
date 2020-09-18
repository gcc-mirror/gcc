#include "tree-vect.h"

#define EXEC_ROR2(a, b, sz)     (a >> b) | (a << (64 - b))

#define TYPE __UINT64_TYPE__

void __attribute__((noipa))
exec_VRORudi_i(TYPE *__restrict__ pvd,
	       TYPE *__restrict__ const pva, unsigned char IMM)
{
  unsigned char I2 = IMM & 63;

  for (unsigned i = 0; i < 4; i++)
    pvd[i] = EXEC_ROR2(pva[i], I2, 8);
}

int main()
{
  check_vect ();

  TYPE pvd[4], pva[4] = { 0x0102030405060708, 0x0102030405060708, 0x0102030405060708, 0x0102030405060708 };
  exec_VRORudi_i (pvd, pva, 7);
  if (pvd[0] != 0x10020406080a0c0e)
    __builtin_abort ();
  return 0;
}
