/* { dg-options "-w" } */

#ifndef SKIP_ATTRIBUTE

#include "compat-common.h"
#include "vector-defs.h"
#include "vector-setup.h"

SETUP (8, qi);
SETUP (16, qi);
SETUP (32, qi);
SETUP (2, hi);
SETUP (4, hi);
SETUP (8, hi);
SETUP (16, hi);
SETUP (2, si);
SETUP (4, si);
SETUP (8, si);
SETUP (1, di);
SETUP (2, di);
SETUP (4, di);

#endif

void
vector_1_x (void)
{
#ifndef SKIP_ATTRIBUTE
  DEBUG_INIT

  CHECK (8, qi);
  CHECK (16, qi);
  CHECK (32, qi);
  CHECK (2, hi);
  CHECK (4, hi);
  CHECK (8, hi);
  CHECK (16, hi);
  CHECK (2, si);
  CHECK (4, si);
  CHECK (8, si);
  CHECK (1, di);
  CHECK (2, di);
  CHECK (4, di);

  DEBUG_FINI

  if (fails != 0)
    abort ();
#endif
}
