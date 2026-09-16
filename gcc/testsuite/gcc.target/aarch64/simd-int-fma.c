/* { dg-do compile } */
/* { dg-options "-O2" } */

#include "simd-int-fma.h"

/* { dg-final { scan-assembler-times {\tmla\tv} 24 } } */
/* { dg-final { scan-assembler-times {\tmls\tv} 24 } } */
