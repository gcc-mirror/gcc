/* Test C2x CR_DECIMAL_DIG: defined for __STDC_WANT_IEC_60559_EXT__.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x" } */

#define __STDC_WANT_IEC_60559_EXT__
#include <float.h>

#ifndef CR_DECIMAL_DIG
#error "CR_DECIMAL_DIG not defined"
#endif

#if CR_DECIMAL_DIG < DECIMAL_DIG + 3
#error "CR_DECIMAL_DIG too small"
#endif
