#include "harness.h"

/* Small expression involving non-overloaded specific vector intrinsics.  */

vector float
f(vector float a, vector float b, vector float c) 
{
  return vec_nmsub(a, vec_vrefp(b), vec_nmsub(b, c, vec_vexptefp(a)));
}

static void test()
{
  check(vec_all_eq(f(((vector float){2,3,5,7}),
		     ((vector float){11,13,17,19}),
		     ((vector float){23,29,31,37})),
		   ((vector float){-249.181808, -369.230774, -495.294098, -575.368408})),
	"f");
}
