/* Test TS 18661-1 CR_DECIMAL_DIG: not in C2X without
   __STDC_WANT_IEC_60559_BFP_EXT__ defined.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x" } */

#include <float.h>

#ifdef CR_DECIMAL_DIG
#error "CR_DECIMAL_DIG defined"
#endif
