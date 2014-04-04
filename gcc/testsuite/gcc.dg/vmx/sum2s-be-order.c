/* { dg-options "-maltivec=be -mabi=altivec -std=gnu99 -mno-vsx" } */

#include "harness.h"

static void test()
{
  vector signed int vsia = {-10,1,2,3};
  vector signed int vsib = {100,101,102,-103};
  vector signed int vsir;
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  vector signed int vsier = {91,0,107,0};
#else
  vector signed int vsier = {0,92,0,-98};
#endif

  vsir = vec_sum2s (vsia, vsib);

  check (vec_all_eq (vsir, vsier), "vsir");
}
