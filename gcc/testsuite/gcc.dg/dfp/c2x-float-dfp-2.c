/* Test DFP macros defined in <float.h> with DFP support.  TS 18661-2
   feature test macro does not change what is defined.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x" } */

#define __STDC_WANT_IEC_60559_DFP_EXT__

#include "c2x-float-dfp-1.c"
