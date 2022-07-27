/* Test non-canonical BID significands: _Decimal32.  Bug 91226.  */
/* { dg-do run } */
/* { dg-require-effective-target dfp_bid } */
/* { dg-options "-std=gnu2x -O0" } */

#include "bid-non-canonical-d32-1.c"
