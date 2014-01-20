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

  check (vec_all_eq (vec_insert (16, va, 5),
		     ((vector unsigned char)
		      {0,1,2,3,4,16,6,7,8,9,10,11,12,13,14,15})),
	 "vec_insert (va)");
  check (vec_all_eq (vec_insert (-16, vb, 0),
		     ((vector signed char)
		      {-16,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7})),
	 "vec_insert (vb)");
  check (vec_all_eq (vec_insert (16, vc, 7),
		     ((vector unsigned short){0,1,2,3,4,5,6,16})),
	 "vec_insert (vc)");
  check (vec_all_eq (vec_insert (-16, vd, 3),
		     ((vector signed short){-4,-3,-2,-16,0,1,2,3})),
	 "vec_insert (vd)");
  check (vec_all_eq (vec_insert (16, ve, 2),
		     ((vector unsigned int){0,1,16,3})),
	 "vec_insert (ve)");
  check (vec_all_eq (vec_insert (-16, vf, 1),
		     ((vector signed int){-2,-16,0,1})),
	 "vec_insert (vf)");
  check (vec_all_eq (vec_insert (-16.0f, vg, 0),
		     ((vector float){-16.0f,-1.0f,0.0f,1.0f})),
	 "vec_insert (vg)");
}

