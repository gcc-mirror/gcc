/* { dg-options "-w" } */
/* { dg-xfail-if "PR target/12916" "sparc*-*-*" "*" "" } */

#include "compat-common.h"
#include "vector-defs.h"
#include "vector-setup.h"

SETUP (2, sf);
SETUP (4, sf);
SETUP (16, sf);
SETUP (2, df);

void
vector_2_x (void)
{
  DEBUG_INIT

  CHECK (2, sf);
  CHECK (4, sf);
  CHECK (16, sf);
  CHECK (2, df);

  DEBUG_FINI

  if (fails != 0)
    abort ();
}
