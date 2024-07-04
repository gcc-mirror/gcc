/* { dg-options "" } */

#include <arm_sme.h>

inline void __attribute__((always_inline))
call_svzero () [[arm::inout("za"), arm::streaming_compatible]] // { dg-error "inlining failed" }
{
  svzero_za ();
}

void
sc_caller ()
{
  call_svzero ();
}
