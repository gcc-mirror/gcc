#include "harness.h"

vector unsigned int
f(vector unsigned int a, vector unsigned int b) 
{
  return vec_vaddcuw(vec_vaddcuw(a,b),b);
}

static void test()
{
  check(vec_all_eq(f(((vector unsigned int){2,4,6,8}),
		     ((vector unsigned int){-1,-2,-3,-4})),
		   ((vector unsigned int){1,0,0,0})),
	"f");
}
