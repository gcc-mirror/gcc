/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-maltivec -mabi=altivec -std=gnu99 -mvsx" } */

#include "harness.h"

static void test()
{
  vector long vl = {0, 1};
  vector double vd = {0.0, 1.0};

  check (vec_extract (vl, 0) == 0, "vec_extract, vl, 0");
  check (vec_extract (vd, 1) == 1.0, "vec_extract, vd, 1");
  check (vl[0] == 0, "[], vl, 0");
  check (vd[1] == 1.0, "[], vd, 0");
}
