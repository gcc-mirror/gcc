#include "harness.h"

/* Simple use of a overloaded generic vector intrinsic.  */

vector unsigned int
f(vector unsigned int a, vector unsigned int b) 
{
  return vec_subs(a,b);
}

static void test()
{
  static vector unsigned int zero;
  check(vec_all_eq(f(((vector unsigned int){2,4,6,8}),
		     ((vector unsigned int){2,4,6,8})),
		   zero),
	"f");
}
