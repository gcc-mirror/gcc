#include "harness.h"

static void test()
{
  vector unsigned char va = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
  vector signed char vb = {-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7};
  vector unsigned short vc = {0,1,2,3,4,5,6,7};
  vector signed short vd = {-4,-3,-2,-1,0,1,2,3};
  vector unsigned int ve = {0,1,2,3};
  vector signed int vf = {-2,-1,0,1};
  vector float vg = {-2.0f,-1.0f,0.0f,1.0f};

  check (vec_extract (va, 5) == 5, "vec_extract (va, 5)");
  check (vec_extract (vb, 0) == -8, "vec_extract (vb, 0)");
  check (vec_extract (vc, 7) == 7, "vec_extract (vc, 7)");
  check (vec_extract (vd, 3) == -1, "vec_extract (vd, 3)");
  check (vec_extract (ve, 2) == 2, "vec_extract (ve, 2)");
  check (vec_extract (vf, 1) == -1, "vec_extract (vf, 1)");
  check (vec_extract (vg, 0) == -2.0f, "vec_extract (vg, 0)");
}

