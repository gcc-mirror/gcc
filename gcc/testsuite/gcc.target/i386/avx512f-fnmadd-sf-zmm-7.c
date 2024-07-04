/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vbroadcastss\[^\n\]*%zmm\[0-9\]+" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vfnmadd...ps\[^\n\]*%zmm\[0-9\]+" 1 } } */

#define type __m512
#define vec 512
#define op fnmadd
#define suffix ps
#define SCALAR float

#include "avx512-fma-7.h"
