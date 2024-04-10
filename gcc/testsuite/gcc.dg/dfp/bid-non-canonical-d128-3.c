/* Test non-canonical BID significands: _Decimal128.  Bug 91226.  */
/* { dg-do run { target { lp64 && dfprt } } } */
/* { dg-require-effective-target dfp_bid } */
/* { dg-options "-std=gnu23 -O0" } */

#include "bid-non-canonical-d128-1.c"
