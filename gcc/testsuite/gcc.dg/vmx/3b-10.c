#include "harness.h"

typedef vector unsigned int x;

x f (x a)
{
  return vec_addc(a,a); 
}

void g (int b) 
{
  vec_dst(&b, 3, 3); 
  vec_dst(&b, 1, 1);
}

static void test()
{
  check(vec_all_eq(f(((vector unsigned int){0x80000000,0x7fffffff,3,4})),
		   ((vector unsigned int){1,0,0,0})),
	"f");
}
