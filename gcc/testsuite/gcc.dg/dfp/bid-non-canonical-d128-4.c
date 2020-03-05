/* Test non-canonical BID significands: _Decimal128, case where
   combination field starts 11.  Bug 91226.  */
/* { dg-do run { target { { i?86-*-* x86_64-*-* } && lp64 } } } */
/* { dg-options "-std=gnu2x -O0" } */

#include "bid-non-canonical-d128-2.c"
