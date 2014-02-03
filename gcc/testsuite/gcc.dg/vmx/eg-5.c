#include "harness.h"

static vector float
matvecmul4 (vector float c0, vector float c1, vector float c2,
	    vector float c3, vector float v)
{
  /* Set result to a vector of f32 0's */
  vector float result = ((vector float){0.,0.,0.,0.});
  result  = vec_madd (c0, vec_splat (v, 0), result);
  result  = vec_madd (c1, vec_splat (v, 1), result);
  result  = vec_madd (c2, vec_splat (v, 2), result);
  result  = vec_madd (c3, vec_splat (v, 3), result);
  return result;
}

static void test()
{
  check(vec_all_eq(matvecmul4(((vector float){2,3,5,7}),
			      ((vector float){11,13,17,19}),
			      ((vector float){23,29,31,37}),
			      ((vector float){41,43,47,53}),
			      ((vector float){59,61,67,71})),
		   ((vector float){5241, 5966, 6746, 7814})),
	"matvecmul4");
}
