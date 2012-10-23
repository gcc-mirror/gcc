/* { dg-do compile } */
/* { dg-options "-O2" } */

#define GPF float
#define SUFFIX(x) x##f
#define GPI int

#include "fcvt.x"

/* { dg-final { scan-assembler-times "fcvtzs\tw\[0-9\]+, *s\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "fcvtps\tx\[0-9\]+, *s\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "fcvtps\tw\[0-9\]+, *s\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "fcvtms\tx\[0-9\]+, *s\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "fcvtms\tw\[0-9\]+, *s\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "fcvtas\tw\[0-9\]+, *s\[0-9\]" 2 } } */
