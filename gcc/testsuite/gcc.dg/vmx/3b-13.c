#include "harness.h"

vector signed int 
f(vector float a, vector signed int b) 
{
  return vec_splat(vec_cts(vec_ctf(vec_ctu(a, 31),0),9),30);
}

static void test()
{
  check(vec_all_eq(f(((vector float){1,2,3,4}),
		     ((vector signed int){2,4,6,8})),
		   ((vector signed int){2147483647, 2147483647, 2147483647, 2147483647})),
	"f");
}
