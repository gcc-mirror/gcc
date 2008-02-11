/* { dg-options "-w" } */
/* { dg-options "-w -mno-mmx" { target { i?86-*-* x86_64-*-* } } } */

#ifndef SKIP_ATTRIBUTE

#include "compat-common.h"
#include "vector-defs.h"
#include "vector-setup.h"

SETUP (2, sf);
SETUP (4, sf);
SETUP (16, sf);
SETUP (2, df);

#endif

void
vector_2_x (void)
{
#ifndef SKIP_ATTRIBUTE
  DEBUG_INIT

  CHECK (2, sf);
  CHECK (4, sf);
  CHECK (16, sf);
  CHECK (2, df);

  DEBUG_FINI

  if (fails != 0)
    abort ();
#endif
}
