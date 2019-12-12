/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -march=z13 -mzvector -fno-asynchronous-unwind-tables" } */

#include <vecintrin.h>

vector unsigned char test(void)
{
   vector unsigned char a = { 0 };
   return __builtin_s390_vec_addc_u128 (a, a);
}
