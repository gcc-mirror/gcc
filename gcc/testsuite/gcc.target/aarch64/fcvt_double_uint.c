/* { dg-do compile } */
/* { dg-options "-O2" } */

#define GPF double
#define SUFFIX(x) x
#define GPI unsigned int

#include "fcvt.x"

/* { dg-final { scan-assembler-times "fcvtzu\tw\[0-9\]+, *d\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "fcvtps\tx\[0-9\]+, *d\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "fcvtpu\tw\[0-9\]+, *d\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "fcvtms\tx\[0-9\]+, *d\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "fcvtmu\tw\[0-9\]+, *d\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "fcvtau\tw\[0-9\]+, *d\[0-9\]" 2 } } */
