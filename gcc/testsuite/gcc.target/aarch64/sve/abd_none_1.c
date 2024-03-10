/* { dg-do compile } */
/* { dg-options "-O3" } */

#define N 1024

#define ABD_ABS
#include "../abd.h"

TEST1(unsigned, int)
TEST3(unsigned, char, int, short)

/* { dg-final { scan-assembler-not {\tsabd\t} } } */
/* { dg-final { scan-assembler-not {\tuabd\t} } } */
