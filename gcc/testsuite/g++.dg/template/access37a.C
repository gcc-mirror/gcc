// PR c++/100502
// { dg-additional-options "-DDECLARE_FRIEND -Wno-non-template-friend" }

// Verify that access37.C is accepted if the appropriate friend relation
// is declared (controlled by the macro DECLARE_FRIEND).
#include "access37.C"
