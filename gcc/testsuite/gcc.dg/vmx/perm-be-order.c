/* { dg-options "-maltivec=be -mabi=altivec -std=gnu99 -mno-vsx" } */

#include "harness.h"

static void test()
{
  /* Input vectors.  */
  vector unsigned char vuca = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
  vector unsigned char vucb = {16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31};
  vector signed char vsca = {-16,-15,-14,-13,-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1};
  vector signed char vscb = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
  vector unsigned short vusa = {0,1,2,3,4,5,6,7};
  vector unsigned short vusb = {8,9,10,11,12,13,14,15};
  vector signed short vssa = {-8,-7,-6,-5,-4,-3,-2,-1};
  vector signed short vssb = {0,1,2,3,4,5,6,7};
  vector unsigned int vuia = {0,1,2,3};
  vector unsigned int vuib = {4,5,6,7};
  vector signed int vsia = {-4,-3,-2,-1};
  vector signed int vsib = {0,1,2,3};
  vector float vfa = {-4.0,-3.0,-2.0,-1.0};
  vector float vfb = {0.0,1.0,2.0,3.0};

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  vector unsigned char vucp = {15,16,14,17,13,18,12,19,11,20,10,21,9,22,8,23};
  vector unsigned char vscp = {15,16,14,17,13,18,12,19,11,20,10,21,9,22,8,23};
  vector unsigned char vusp = {15,14,17,16,13,12,19,18,11,10,21,20,9,8,23,22};
  vector unsigned char vssp = {15,14,17,16,13,12,19,18,11,10,21,20,9,8,23,22};
  vector unsigned char vuip = {15,14,13,12,19,18,17,16,11,10,9,8,23,22,21,20};
  vector unsigned char vsip = {15,14,13,12,19,18,17,16,11,10,9,8,23,22,21,20};
  vector unsigned char vfp  = {15,14,13,12,19,18,17,16,11,10,9,8,23,22,21,20};
#else
  vector unsigned char vucp = {0,31,1,30,2,29,3,28,4,27,5,26,6,25,7,24};
  vector unsigned char vscp = {0,31,1,30,2,29,3,28,4,27,5,26,6,25,7,24};
  vector unsigned char vusp = {0,1,30,31,2,3,28,29,4,5,26,27,6,7,24,25};
  vector unsigned char vssp = {0,1,30,31,2,3,28,29,4,5,26,27,6,7,24,25};
  vector unsigned char vuip = {0,1,2,3,28,29,30,31,4,5,6,7,24,25,26,27};
  vector unsigned char vsip = {0,1,2,3,28,29,30,31,4,5,6,7,24,25,26,27};
  vector unsigned char vfp  = {0,1,2,3,28,29,30,31,4,5,6,7,24,25,26,27};
#endif

  /* Result vectors.  */
  vector unsigned char vuc;
  vector signed char vsc;
  vector unsigned short vus;
  vector signed short vss;
  vector unsigned int vui;
  vector signed int vsi;
  vector float vf;

  /* Expected result vectors.  */
  vector unsigned char vucr = {0,31,1,30,2,29,3,28,4,27,5,26,6,25,7,24};
  vector signed char vscr = {-16,15,-15,14,-14,13,-13,12,-12,11,-11,10,-10,9,-9,8};
  vector unsigned short vusr = {0,15,1,14,2,13,3,12};
  vector signed short vssr = {-8,7,-7,6,-6,5,-5,4};
  vector unsigned int vuir = {0,7,1,6};
  vector signed int vsir = {-4,3,-3,2};
  vector float vfr = {-4.0,3.0,-3.0,2.0};

  vuc = vec_perm (vuca, vucb, vucp);
  vsc = vec_perm (vsca, vscb, vscp);
  vus = vec_perm (vusa, vusb, vusp);
  vss = vec_perm (vssa, vssb, vssp);
  vui = vec_perm (vuia, vuib, vuip);
  vsi = vec_perm (vsia, vsib, vsip);
  vf  = vec_perm (vfa,  vfb,  vfp );

  check (vec_all_eq (vuc, vucr), "vuc");
  check (vec_all_eq (vsc, vscr), "vsc");
  check (vec_all_eq (vus, vusr), "vus");
  check (vec_all_eq (vss, vssr), "vss");
  check (vec_all_eq (vui, vuir), "vui");
  check (vec_all_eq (vsi, vsir), "vsi");
  check (vec_all_eq (vf,  vfr),  "vf" );
}
