// `B::a' contains empty classes which may cause base classes to be
// placed at different locations in a future version of GCC.
// g++.dg/abi/empty6.C tests this with -Wabi.
// Split into pieces for binary compatibility testing October 2002

#include "empty6.h"

extern void empty6_x (void);

int
main ()
{
  empty6_x ();
}
