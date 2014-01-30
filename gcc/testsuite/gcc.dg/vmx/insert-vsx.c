/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-maltivec -mabi=altivec -std=gnu99 -mvsx" } */

#include "harness.h"

static int vec_long_eq (vector long x, vector long y)
{
  return (x[0] == y[0] && x[1] == y[1]);
}

static int vec_dbl_eq (vector double x, vector double y)
{
  return (x[0] == y[0] && x[1] == y[1]);
}

static void test()
{
  vector long vl = {0, 1};
  vector double vd = {0.0, 1.0};
  vector long vlr = vec_insert (2, vl, 0);
  vector double vdr = vec_insert (2.0, vd, 1);
  vector long vler = {2, 1};
  vector double vder = {0.0, 2.0};

  check (vec_long_eq (vlr, vler), "vl");
  check (vec_dbl_eq (vdr, vder), "vd");
}
