/* { dg-do run } */
/* { dg-require-effective-target arm32 } */

/* Test to catch off-by-one errors in arm/pr-support.c.  */

#if defined (__VFP_FP__) && !defined (__SOFTFP__)

#include <iostream>
#include <stdlib.h>

using namespace std;

static void donkey ()
{
#if __ARM_FP & 8
  asm volatile ("vmov.f64 d9, %P0" : : "w" (1.2345) : "d9");
#else
  asm volatile ("vmov.f32 s18, %0" : : "t" (1.2345f) : "s18");
#endif
  throw 1;
}

int main()
{
  try
    {
      donkey ();
    }
  catch (int foo)
    {
      return 0;
    }
  return 1;
}

#else

int main()
{
  return 0;
}

#endif

