/* Test DFP macros not defined in <float.h> if no DFP support.  Test
   with feature test macros defined.  */
/* { dg-do compile { target { ! dfp } } } */
/* { dg-options "-std=c23" } */

#define __STDC_WANT_DEC_FP__
#define __STDC_WANT_IEC_60559_DFP_EXT__

#include "c23-float-no-dfp-1.c"
