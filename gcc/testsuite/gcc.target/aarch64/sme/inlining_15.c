/* { dg-options "" } */

#include <arm_sme.h>

inline void
call_svzero () [[arm::inout("za"), arm::streaming_compatible]]
{
  svzero_za ();
}

void
n_caller ()
{
  call_svzero (); // { dg-error "call to a function that shares 'za' state from a function that has no 'za' state" }
}

void
s_caller ()
{
  call_svzero (); // { dg-error "call to a function that shares 'za' state from a function that has no 'za' state" }
}

void
sc_caller ()
{
  call_svzero (); // { dg-error "call to a function that shares 'za' state from a function that has no 'za' state" }
}
