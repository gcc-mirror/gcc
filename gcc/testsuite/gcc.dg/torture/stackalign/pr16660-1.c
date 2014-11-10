/* { dg-do run } */
/* { dg-skip-if "Stack alignment is too small" { hppa*-*-hpux* } "*" "" } */
/* { dg-skip-if "Stack alignment causes use of alloca" { nvptx-*-* } "*" "" } */

#include "check.h"

void
f ()
{
  unsigned long tmp[4] __attribute__((aligned(64)));
  check (&tmp, 64);
}

int
main()
{
  f();
  return 0;
}
