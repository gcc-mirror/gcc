#include "harness.h"

/* Small expression involving non-overloaded specific vector intrinsics.  */

vector float
f(vector float a, vector float b, vector float c) 
{
  vector float q = vec_expte(a);
  vector float r = vec_vsubfp(c, q);
  vector float s = vec_re(b);
  vector float t = vec_nmsub(s, c, r);
  return t;
}

static void test()
{
  check(vec_all_eq(f(((vector float){2,3,5,7}),
		     ((vector float){11,13,17,19}),
		     ((vector float){23,29,31,37})),
		   ((vector float){16.9092026, 18.7693329, -2.8233242, -92.9472198})),
		   "f");
}
