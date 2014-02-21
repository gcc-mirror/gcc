/* { dg-options "-maltivec=be -mabi=altivec -std=gnu99 -mno-vsx" } */

#include "harness.h"

static void test()
{
  vector signed int va = {-7,11,-13,17};

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  vector signed int vb = {128,0,0,0};
  vector signed int evd = {136,0,0,0};
#else
  vector signed int vb = {0,0,0,128};
  vector signed int evd = {0,0,0,136};
#endif

  vector signed int vd = vec_sums (va, vb);

  check (vec_all_eq (vd, evd), "sums");
}
