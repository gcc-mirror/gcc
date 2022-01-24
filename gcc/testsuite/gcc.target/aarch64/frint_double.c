/* { dg-do compile } */
/* { dg-options "-O2" } */

#define GPF double
#define SUFFIX(x) x

#include "frint.x"

/* { dg-final { scan-assembler-times "frintz\td\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "frintp\td\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "frintm\td\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "frinti\td\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "frintx\td\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "frinta\td\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "frintn\td\[0-9\]" 2 } } */
