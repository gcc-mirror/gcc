// { dg-options "-w" }

// The size assigned to `U' may not be ABI-compliant and may change in a
// future version of GCC.
// g++.dg/abi/bitfield7.C tests this with -Wabi.
// Split into pieces for binary compatibility testing October 2002

#include "bitfield7.h"

extern void bitfield7_x (void);

int
main ()
{
  bitfield7_x ();
}
