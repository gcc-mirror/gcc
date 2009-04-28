#include "harness.h"

/* Small expression involving non-overloaded vector intrinsics.  */

vector float
f(vector float a, vector float b, vector float c) 
{
  return vec_vmaddfp(a, vec_re(b), vec_vmaxfp(c, vec_expte(a)));
}

static void test()
{
  check(vec_all_gt(f(((vector float){2,3,5,7}),
		     ((vector float){11,13,17,19}),
		     ((vector float){23,29,31,37})),
		   ((vector float){23.18, 29.23, 32.29, 128.36}))
	&& vec_all_lt(f(((vector float){2,3,5,7}),
			((vector float){11,13,17,19}),
			((vector float){23,29,31,37})),
		      ((vector float){23.19, 29.24, 32.30, 128.37})),
	"f");
}
