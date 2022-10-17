/* { dg-do compile } */
/* { dg-options "-O2" } */

#define GPF float
#define SUFFIX(x) x##f

#include "frint.x"

/* { dg-final { scan-assembler-times "frintz\ts\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "frintp\ts\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "frintm\ts\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "frinti\ts\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "frintx\ts\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "frinta\ts\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "frintn\ts\[0-9\]" 2 } } */
