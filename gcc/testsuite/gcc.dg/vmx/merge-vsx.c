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
  vector unsigned int vuia = {0,1,2,3};
  vector unsigned int vuib = {4,5,6,7};
  vector signed int vsia = {-4,-3,-2,-1};
  vector signed int vsib = {0,1,2,3};
  vector float vfa = {-4.0,-3.0,-2.0,-1.0};
  vector float vfb = {0.0,1.0,2.0,3.0};

  /* Result vectors.  */
  vector long long vlh, vll;
  vector double vdh, vdl;
  vector unsigned int vuih, vuil;
  vector signed int vsih, vsil;
  vector float vfh, vfl;

  /* Expected result vectors.  */
  vector long long vlrh = {-2,0};
  vector long long vlrl = {-1,1};
  vector double vdrh = {-2.0,0.0};
  vector double vdrl = {-1.0,1.0};
  vector unsigned int vuirh = {0,4,1,5};
  vector unsigned int vuirl = {2,6,3,7};
  vector signed int vsirh = {-4,0,-3,1};
  vector signed int vsirl = {-2,2,-1,3};
  vector float vfrh = {-4.0,0.0,-3.0,1.0};
  vector float vfrl = {-2.0,2.0,-1.0,3.0};

  vlh = vec_mergeh (vla, vlb);
  vll = vec_mergel (vla, vlb);
  vdh = vec_mergeh (vda, vdb);
  vdl = vec_mergel (vda, vdb);
  vuih = vec_mergeh (vuia, vuib);
  vuil = vec_mergel (vuia, vuib);
  vsih = vec_mergeh (vsia, vsib);
  vsil = vec_mergel (vsia, vsib);
  vfh  = vec_mergeh (vfa,  vfb );
  vfl  = vec_mergel (vfa,  vfb );

  check (vec_long_long_eq (vlh, vlrh), "vlh");
  check (vec_long_long_eq (vll, vlrl), "vll");
  check (vec_double_eq (vdh, vdrh), "vdh" );
  check (vec_double_eq (vdl, vdrl), "vdl" );
  check (vec_all_eq (vuih, vuirh), "vuih");
  check (vec_all_eq (vuil, vuirl), "vuil");
  check (vec_all_eq (vsih, vsirh), "vsih");
  check (vec_all_eq (vsil, vsirl), "vsil");
  check (vec_all_eq (vfh,  vfrh),  "vfh");
  check (vec_all_eq (vfl,  vfrl),  "vfl");
}
