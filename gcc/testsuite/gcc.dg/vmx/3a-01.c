#include "harness.h"
/* Simple use of a non-overloaded generic vector intrinsic.  */

static vector unsigned int
f(vector unsigned int a, vector unsigned int b) 
{
  return vec_addc(a,b);
}

static void test()
{
  check(vec_all_eq(f(((vector unsigned int){1,1,3,2}),
		     ((vector unsigned int){-1,-2,3,-4})),
		   ((vector unsigned int){1,0,0,0})),
	"f");
}
