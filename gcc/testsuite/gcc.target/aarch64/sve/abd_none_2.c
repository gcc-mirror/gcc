/* { dg-do compile } */
/* { dg-options "-O3" } */

#define N 1024

#define ABD_IDIOM
#include "../abd.h"

TEST1(signed, short)
TEST1(signed, char)

TEST3(signed, char, int, short)

TEST1(unsigned, int)
TEST1(unsigned, short)
TEST1(unsigned, char)

TEST3(unsigned, char, int, short)

/* { dg-final { scan-assembler-not {\tsabd\t} } } */
/* { dg-final { scan-assembler-not {\tuabd\t} } } */
