// The offset of virtual base `B' is not ABI-compliant and may change in
// a future version of GCC.
// g++.dg/abi/vbase10.C tests this with -Wabi.
// Split into pieces for binary compatibility testing October 2002

#include "vbase10.h"

extern void vbase10_x (void);

int
main ()
{
  vbase10_x ();
}
