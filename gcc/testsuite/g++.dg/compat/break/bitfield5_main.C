// The offset of `B::f2' is not ABI-compliant and may change in a future
// version of GCC.
// g++.dg/abi/bitfield5.C tests this with -Wabi.
// Split into pieces for binary compatibility testing October 2002

#include "bitfield5.h"

extern void bitfield5_x (void);

int
main ()
{
  bitfield5_x ();
}
