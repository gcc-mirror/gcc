/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-maltivec=be -mabi=altivec -std=gnu99 -mvsx" } */

#include "harness.h"

static void test()
{
  vector long long vl = {0, 1};
  vector double vd = {0.0, 1.0};

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  check (vec_extract (vl, 0) == 1, "vl, 0");
  check (vec_extract (vd, 1) == 0.0, "vd, 1");
#else
  check (vec_extract (vl, 0) == 0, "vl, 0");
  check (vec_extract (vd, 1) == 1.0, "vd, 1");
#endif
}
