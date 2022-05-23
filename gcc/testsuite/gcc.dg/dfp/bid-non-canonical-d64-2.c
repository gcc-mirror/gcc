/* Test non-canonical BID significands: _Decimal64.  Bug 91226.  */
/* { dg-do run } */
/* { dg-require-effective-target dfp_bid } */
/* { dg-options "-std=gnu2x -O0" } */

#include "bid-non-canonical-d64-1.c"
