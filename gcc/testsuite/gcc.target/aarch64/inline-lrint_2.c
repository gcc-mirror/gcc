/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O3 -fno-math-errno" } */

#include "lrint-matherr.h"

TEST (dld, double, long, )
TEST (flf, float , long, )

TEST (did, double, int, )
TEST (fif, float , int, )

TEST (dlld, double, long long, l)
TEST (fllf, float , long long, l)

/* { dg-final { scan-assembler-times "frintx\td\[0-9\]+, d\[0-9\]+" 3 } } */
/* { dg-final { scan-assembler-times "frintx\ts\[0-9\]+, s\[0-9\]+" 3 } } */
/* { dg-final { scan-assembler-times "fcvtzs\tx\[0-9\]+, d\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "fcvtzs\tx\[0-9\]+, s\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "fcvtzs\tw\[0-9\]+, d\[0-9\]+" 2 } } */
/* { dg-final { scan-assembler-times "fcvtzs\tw\[0-9\]+, s\[0-9\]+" 2 } } */
/* { dg-final { scan-assembler-not "bl"    } } */
