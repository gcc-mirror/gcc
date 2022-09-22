/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -mzvector -march=z15 -fdump-rtl-subreg1" } */

/* { dg-final { scan-rtl-dump-times "Skipping mode V2DI for copy lowering" 2 "subreg1" } } */

#include <vecintrin.h>

#define STYPE long long
#define VTYPE __attribute__ ((vector_size (16))) STYPE

STYPE
foo1 (VTYPE a)
{
  /* { dg-final { scan-assembler-not "vst\t.*" } } */
  /* { dg-final { scan-assembler-not "lg\t.*" } } */
  /* { dg-final { scan-assembler-not "lgr\t.*" } } */
  return a[0] + a[1];
}
