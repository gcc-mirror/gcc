/* { dg-do compile } */
/* { dg-options "-O2" } */

#define GPF float
#define SUFFIX(x) x##f
#define GPI unsigned long

#include "fcvt.x"

/* { dg-final { scan-assembler-times "fcvtzu\tx\[0-9\]+, *s\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "fcvtps\tx\[0-9\]+, *s\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "fcvtpu\tx\[0-9\]+, *s\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "fcvtms\tx\[0-9\]+, *s\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "fcvtmu\tx\[0-9\]+, *s\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "fcvtau\tx\[0-9\]+, *s\[0-9\]" 2 } } */
