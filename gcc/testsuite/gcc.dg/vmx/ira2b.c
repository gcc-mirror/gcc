#include "harness.h"

static vector float
f(vector float f32a, vector float f32b, vector float f32c)
{
  f32c = vec_ceil(f32a);
  return vec_vmaddfp(f32a, f32b, f32c);
}

static void test()
{
  check(vec_all_eq(f(((vector float){2,3,5,7}),
		     ((vector float){11,13,17,19}),
		     ((vector float){23,29,31,37})),
		   ((vector float){24, 42, 90, 140})),
	"test");
}
