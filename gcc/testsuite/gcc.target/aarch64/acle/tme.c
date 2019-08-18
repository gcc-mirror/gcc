/* Test the TME intrinsics.  */

/* { dg-do compile } */
/* { dg-options "-save-temps -O2 -march=armv8-a+tme" } */

#include "arm_acle.h"

#define tcancel_reason 0x234

unsigned
check_tme (void)
{
  unsigned status = __tstart ();
  if (status == 0)
    {
      if (__ttest () == 2)
	{
	  __tcancel (tcancel_reason & _TMFAILURE_REASON);
	  return tcancel_reason;
	}

      __tcommit ();
      return 0;
    }
  else if (status & _TMFAILURE_NEST)
    return _TMFAILURE_NEST;
  else if (status & _TMFAILURE_TRIVIAL)
    return _TMFAILURE_TRIVIAL;
}

/* { dg-final { scan-assembler "tstart\tx..?\n" } } */
/* { dg-final { scan-assembler "tcancel\t#564\n" } } */
/* { dg-final { scan-assembler "ttest\tx..?\n" } } */
/* { dg-final { scan-assembler "tcommit\n" } } */
