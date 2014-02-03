/* { dg-options "-maltivec=be -mabi=altivec -std=gnu99 -mno-vsx" } */

#include "harness.h"

static void test()
{
  vector signed int va = {-7,11,-13,17};

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  vector signed int vb = {128,0,0,0};
#else
  vector signed int vb = {0,0,0,128};
#endif

  vector signed int vd = vec_sums (va, vb);
  signed int r = vec_extract (vd, 3);

  check (r == 136, "sums");
}
