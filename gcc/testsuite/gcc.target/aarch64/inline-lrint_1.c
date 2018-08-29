/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O3 -fno-math-errno" } */

#include "lrint-matherr.h"

TEST (dld, double, long, )
TEST (flf, float , long, )

TEST (did, double, int, )
TEST (fif, float , int, )

TEST (dlld, double, long long, l)
TEST (fllf, float , long long, l)

/* { dg-final { scan-assembler-times "frintx\t\[d,s\]\[0-9\]+, \[d,s\]\[0-9\]+" 6 } } */
/* { dg-final { scan-assembler-times "fcvtzs\tx\[0-9\]+, \[d,s\]\[0-9\]+" 6 } } */
/* { dg-final { scan-assembler-not "bl"    } } */
