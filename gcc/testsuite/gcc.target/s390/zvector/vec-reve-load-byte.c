/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=arch13 -mzvector" } */

/* The vector byte element reversal is actually implemented with a 128
   bit bswap.  */

#include <vecintrin.h>

vector signed char
test (vector signed char x)
{
  return vec_reve (x);
}

/* { dg-final { scan-assembler-times "vperm\t" 1 } } */


vector signed char
test2 (vector signed char *x)
{
  return vec_reve (*x);
}

vector signed char
test3 (signed char *x)
{
  return vec_reve (vec_xl (0, x));
}

/* { dg-final { scan-assembler-times "vlbrq\t" 2 } } */
