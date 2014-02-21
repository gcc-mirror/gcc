#include "harness.h"

static void test()
{
  vector signed int va = {-7,11,-13,17};
  vector signed int vb = {0,0,0,128};
  vector signed int evd = {0,0,0,136};

  vector signed int vd = vec_sums (va, vb);

  check (vec_all_eq (vd, evd), "sums");
}
