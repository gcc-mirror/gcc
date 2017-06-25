/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-maltivec -mabi=altivec -std=gnu99 -mvsx" } */

#include "harness.h"

static void test()
{
  /* Input vectors.  */
  vector unsigned int vui = {0,1,2,3};
  vector signed int vsi = {-2,-1,0,1};
  vector float vf = {-2.0,-1.0,0.0,1.0};

  /* Result vectors.  */
  vector unsigned int vuir;
  vector signed int vsir;
  vector float vfr;

  /* Expected result vectors.  */
  vector unsigned int vuier = {2,2,2,2};
  vector signed int vsier = {1,1,1,1};
  vector float vfer = {-1.0,-1.0,-1.0,-1.0};

  vuir = vec_splat (vui, 2);
  vsir = vec_splat (vsi, 3);
  vfr  = vec_splat (vf,  1);

  check (vec_all_eq (vuir, vuier), "vui");
  check (vec_all_eq (vsir, vsier), "vsi");
  check (vec_all_eq (vfr,  vfer ), "vf");
}
