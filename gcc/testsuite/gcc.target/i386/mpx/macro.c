/* { dg-do compile } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#include "mpx-check.h"

#ifndef __MPX__
#error -mmpx is required
#endif

#ifndef __CHKP__
#error -fcheck-pointer-bounds is required
#endif

int mpx_test (int argc, const char **argv)
{
  return 0;
}
