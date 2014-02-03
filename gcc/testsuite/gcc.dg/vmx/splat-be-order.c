/* { dg-options "-maltivec=be -mabi=altivec -std=gnu99 -mno-vsx" } */

#include "harness.h"

static void test()
{
  /* Input vectors.  */
  vector unsigned char vuc = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
  vector signed char vsc = {-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7};
  vector unsigned short vus = {0,1,2,3,4,5,6,7};
  vector signed short vss = {-4,-3,-2,-1,0,1,2,3};
  vector unsigned int vui = {0,1,2,3};
  vector signed int vsi = {-2,-1,0,1};
  vector float vf = {-2.0,-1.0,0.0,1.0};

  /* Result vectors.  */
  vector unsigned char vucr;
  vector signed char vscr;
  vector unsigned short vusr;
  vector signed short vssr;
  vector unsigned int vuir;
  vector signed int vsir;
  vector float vfr;

  /* Expected result vectors.  */
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  vector unsigned char vucer = {14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14};
  vector signed char vscer = {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};
  vector unsigned short vuser = {0,0,0,0,0,0,0,0};
  vector signed short vsser = {3,3,3,3,3,3,3,3};
  vector unsigned int vuier = {1,1,1,1};
  vector signed int vsier = {-2,-2,-2,-2};
  vector float vfer = {0.0,0.0,0.0,0.0};
#else
  vector unsigned char vucer = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
  vector signed char vscer = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
  vector unsigned short vuser = {7,7,7,7,7,7,7,7};
  vector signed short vsser = {-4,-4,-4,-4,-4,-4,-4,-4};
  vector unsigned int vuier = {2,2,2,2};
  vector signed int vsier = {1,1,1,1};
  vector float vfer = {-1.0,-1.0,-1.0,-1.0};
#endif

  vucr = vec_splat (vuc, 1);
  vscr = vec_splat (vsc, 8);
  vusr = vec_splat (vus, 7);
  vssr = vec_splat (vss, 0);
  vuir = vec_splat (vui, 2);
  vsir = vec_splat (vsi, 3);
  vfr  = vec_splat (vf,  1);

  check (vec_all_eq (vucr, vucer), "vuc");
  check (vec_all_eq (vscr, vscer), "vsc");
  check (vec_all_eq (vusr, vuser), "vus");
  check (vec_all_eq (vssr, vsser), "vss");
  check (vec_all_eq (vuir, vuier), "vui");
  check (vec_all_eq (vsir, vsier), "vsi");
  check (vec_all_eq (vfr,  vfer ), "vf");
}
