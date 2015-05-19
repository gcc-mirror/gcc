/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=z13 -mzvector" } */

#include <vecintrin.h>


vector unsigned char a, b, c, d;

int
foo ()
{
  a = vec_genmask (0);
  b = vec_genmask (65535);
  c = vec_genmask (43605);
  d = vec_genmask (37830);
}

/* { dg-final { scan-assembler-times "vzero" 1 } } */
/* { dg-final { scan-assembler-times "vone" 1 } } */
/* { dg-final { scan-assembler-times "vgbm\t%v.*,43605" 1 } } */
/* { dg-final { scan-assembler-times "vgbm\t%v.*,37830" 1 } } */
