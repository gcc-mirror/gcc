#include "harness.h"

vector unsigned char
f(vector unsigned char a, vector unsigned char b) 
{
  return vec_vsububs(a,b);
}

static void test()
{
  static vector unsigned char zero;
  check(vec_all_eq(f(((vector unsigned char){2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}),
		     ((vector unsigned char){2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2})),
		   zero),
	"f");
}
