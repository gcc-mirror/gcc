#include "harness.h"

static vector unsigned char 
zero() 
{
  /* MCC allocates a stack slot for and loads an uninitialized local
     variable.  */
  vector unsigned char a;
  return vec_sub(a,a);
}

static void test()
{
  static vector unsigned char zerov;
  check(vec_all_eq(zero(), zerov), "zero");
}
