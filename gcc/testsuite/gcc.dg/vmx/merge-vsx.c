/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-maltivec -mabi=altivec -std=gnu99 -mvsx" } */

#include "harness.h"

static int vec_long_long_eq (vector long long x, vector long long y)
{
  return (x[0] == y[0] && x[1] == y[1]);
}

static int vec_double_eq (vector double x, vector double y)
{
  return (x[0] == y[0] && x[1] == y[1]);
}

static void test()
{
  /* Input vectors.  */
  vector long long vla = {-2,-1};
  vector long long vlb = {0,1};
  vector double vda = {-2.0,-1.0};
  vector double vdb = {0.0,1.0};

  /* Result vectors.  */
  vector long long vlh, vll;
  vector double vdh, vdl;

  /* Expected result vectors.  */
  vector long long vlrh = {-2,0};
  vector long long vlrl = {-1,1};
  vector double vdrh = {-2.0,0.0};
  vector double vdrl = {-1.0,1.0};

  vlh = vec_mergeh (vla, vlb);
  vll = vec_mergel (vla, vlb);
  vdh = vec_mergeh (vda, vdb);
  vdl = vec_mergel (vda, vdb);

  check (vec_long_long_eq (vlh, vlrh), "vlh");
  check (vec_long_long_eq (vll, vlrl), "vll");
  check (vec_double_eq (vdh, vdrh), "vdh" );
  check (vec_double_eq (vdl, vdrl), "vdl" );
}
