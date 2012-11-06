/* { dg-do compile } */
/* { dg-options "-O2" } */

#define GPF double
#define SUFFIX(x) x
#define GPI long

#include "fcvt.x"

/* { dg-final { scan-assembler-times "fcvtzs\tx\[0-9\]+, *d\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "fcvtps\tx\[0-9\]+, *d\[0-9\]" 3 } } */
/* { dg-final { scan-assembler-times "fcvtms\tx\[0-9\]+, *d\[0-9\]" 3 } } */
/* { dg-final { scan-assembler-times "fcvtas\tx\[0-9\]+, *d\[0-9\]" 2 } } */
