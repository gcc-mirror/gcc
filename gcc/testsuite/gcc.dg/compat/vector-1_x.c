/* { dg-options "-w" } */
/* { dg-xfail-if "PR target/12916" "sparc*-*-*" "*" "" } */

#include "compat-common.h"
#include "vector-defs.h"
#include "vector-setup.h"

SETUP (8, qi);
SETUP (16, qi);
SETUP (2, hi);
SETUP (4, hi);
SETUP (8, hi);
SETUP (2, si);
SETUP (4, si);
SETUP (1, di);
SETUP (2, di);

void
vector_1_x (void)
{
  DEBUG_INIT

  CHECK (8, qi);
  CHECK (16, qi);
  CHECK (2, hi);
  CHECK (4, hi);
  CHECK (8, hi);
  CHECK (2, si);
  CHECK (4, si);
  CHECK (1, di);
  CHECK (2, di);

  DEBUG_FINI

  if (fails != 0)
    abort ();
}
