/* Test that default-initialized DFP values consistently have the least quantum
   exponent.  */
/* { dg-do run } */
/* { dg-require-effective-target dfp } */

#define TYPE _Decimal64
#define ZEROFP 0e-398DD
#include "dfp-default-init-1.c"
