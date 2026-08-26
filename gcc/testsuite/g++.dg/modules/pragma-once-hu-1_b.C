// PR c++/127097
// { dg-additional-options "-fmodules" }

import "pragma-once-hu-1_a.H";
// Reuse the tracked sys directory for an alternate spelling without symlinks.
#include "sys/../pragma-once-hu-1_a.H"

int
main ()
{
  return pragma_once_hu_f () != 1;
}
