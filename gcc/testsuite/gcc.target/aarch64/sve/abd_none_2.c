/* { dg-do compile } */
/* { dg-options "-O3" } */

#define N 1024

#define ABD_IDIOM
#include "../abd.h"

TEST1(signed, short)
TEST1(signed, char)

TEST2(signed, long, char)
TEST2(signed, long, short)
TEST2(signed, long, int)
TEST2(signed, int, short)
TEST2(signed, int, char)
TEST2(signed, short, char)

TEST3(signed, char, int, short)
TEST3(signed, char, long, short)
TEST3(signed, char, long, int)
TEST3(signed, char, short, char)
TEST3(signed, char, int, char)
TEST3(signed, char, long, char)

TEST3(signed, short, int, char)
TEST3(signed, short, long, char)
TEST3(signed, short, long, int)
TEST3(signed, short, char, short)
TEST3(signed, short, int, short)
TEST3(signed, short, long, short)

TEST3(signed, int, char, short)
TEST3(signed, int, short, char)
TEST3(signed, int, long, char)
TEST3(signed, int, long, short)
TEST3(signed, int, long, int)

TEST3(signed, long, char, short)
TEST3(signed, long, short, char)
TEST3(signed, long, char, int)
TEST3(signed, long, int, char)
TEST3(signed, long, short, int)
TEST3(signed, long, int, short)

TEST1(unsigned, int)
TEST1(unsigned, short)
TEST1(unsigned, char)
TEST1(unsigned, long)

TEST2(unsigned, int, long)

TEST2(unsigned, long, char)
TEST2(unsigned, long, short)
TEST2(unsigned, long, int)
TEST2(unsigned, int, short)
TEST2(unsigned, int, char)
TEST2(unsigned, short, char)

TEST3(unsigned, char, int, short)
TEST3(unsigned, char, long, short)
TEST3(unsigned, char, int, long)
TEST3(unsigned, char, long, int)
TEST3(unsigned, char, short, char)
TEST3(unsigned, char, int, char)
TEST3(unsigned, char, long, char)

TEST3(unsigned, short, int, char)
TEST3(unsigned, short, long, char)
TEST3(unsigned, short, int, long)
TEST3(unsigned, short, long, int)
TEST3(unsigned, short, char, short)
TEST3(unsigned, short, int, short)
TEST3(unsigned, short, long, short)

TEST3(unsigned, int, char, short)
TEST3(unsigned, int, short, char)
TEST3(unsigned, int, char, long)
TEST3(unsigned, int, long, char)
TEST3(unsigned, int, short, long)
TEST3(unsigned, int, long, short)
TEST3(unsigned, int, char, int)
TEST3(unsigned, int, short, int)
TEST3(unsigned, int, long, int)

TEST3(unsigned, long, char, short)
TEST3(unsigned, long, short, char)
TEST3(unsigned, long, char, int)
TEST3(unsigned, long, int, char)
TEST3(unsigned, long, short, int)
TEST3(unsigned, long, int, short)
TEST3(unsigned, long, char, long)
TEST3(unsigned, long, short, long)
TEST3(unsigned, long, int, long)

/* { dg-final { scan-assembler-not {\tsabd\t} } } */
/* { dg-final { scan-assembler-not {\tsabdl\t} } } */
/* { dg-final { scan-assembler-not {\tsabdl2\t} } } */
/* { dg-final { scan-assembler-not {\tuabd\t} } } */
/* { dg-final { scan-assembler-not {\tuabdl\t} } } */
/* { dg-final { scan-assembler-not {\tuabdl2\t} } } */
