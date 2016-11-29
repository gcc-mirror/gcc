/* Test TS 18661-1 CR_DECIMAL_DIG.  */
/* { dg-do compile } */
/* { dg-options "-std=c11" } */

#define __STDC_WANT_IEC_60559_BFP_EXT__
#include <float.h>

#ifndef CR_DECIMAL_DIG
#error "CR_DECIMAL_DIG not defined"
#endif

#if CR_DECIMAL_DIG < DECIMAL_DIG + 3
#error "CR_DECIMAL_DIG too small"
#endif
