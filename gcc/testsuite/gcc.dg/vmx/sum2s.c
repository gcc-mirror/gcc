#include "harness.h"

static void test()
{
  vector signed int vsia = {-10,1,2,3};
  vector signed int vsib = {100,101,102,-103};
  vector signed int vsir;
  vector signed int vsier = {0,92,0,-98};

  vsir = vec_sum2s (vsia, vsib);

  check (vec_all_eq (vsir, vsier), "vsir");
}
