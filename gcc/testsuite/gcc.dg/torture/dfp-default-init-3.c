/* Test that default-initialized DFP values consistently have the least quantum
   exponent.  */
/* { dg-do run } */
/* { dg-require-effective-target dfp } */

#define TYPE _Decimal128
#define ZEROFP 0e-6176DL
#include "dfp-default-init-1.c"
