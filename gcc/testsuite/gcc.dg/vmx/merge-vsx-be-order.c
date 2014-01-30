/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-maltivec=be -mabi=altivec -std=gnu99 -mvsx" } */

#include "harness.h"

static int vec_long_eq (vector long x, vector long y)
{
  return (x[0] == y[0] && x[1] == y[1]);
}

static void test()
{
  /* Input vectors.  */
  vector long vla = {-2,-1};
  vector long vlb = {0,1};
  vector double vda = {-2.0,-1.0};
  vector double vdb = {0.0,1.0};

  /* Result vectors.  */
  vector long vlh, vll;
  vector double vdh, vdl;

  /* Expected result vectors.  */
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  vector long vlrh = {1,-1};
  vector long vlrl = {0,-2};
  vector double vdrh = {1.0,-1.0};
  vector double vdrl = {0.0,-2.0};
#else
  vector long vlrh = {-2,0};
  vector long vlrl = {-1,1};
  vector double vdrh = {-2.0,0.0};
  vector double vdrl = {-1.0,1.0};
#endif

  vlh = vec_mergeh (vla, vlb);
  vll = vec_mergel (vla, vlb);
  vdh = vec_mergeh (vda, vdb);
  vdl = vec_mergel (vda, vdb);

  check (vec_long_eq (vlh, vlrh), "vlh");
  check (vec_long_eq (vll, vlrl), "vll");
  check (vec_all_eq (vdh, vdrh), "vdh" );
  check (vec_all_eq (vdl, vdrl), "vdl" );
}
