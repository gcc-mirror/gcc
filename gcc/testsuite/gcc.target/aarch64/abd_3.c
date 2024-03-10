/* { dg-do compile } */
/* { dg-options "-Ofast" } */

#pragma GCC target "arch=armv8-a"
#define N 1024

#define ABD_ABS
#include "abd.h"

TEST1(signed, int)
TEST1(signed, short)
TEST1(signed, char)

TEST2(signed, short, char)
TEST2(signed, int, short)
TEST2(signed, int, char)
TEST2(signed, int, long)

TEST3(signed, char, short, char)
TEST3(signed, char, int, long)
TEST3(signed, char, int, short)
TEST3(signed, char, int, char)

TEST3(signed, short, int, char)
TEST3(signed, short, char, short)
TEST3(signed, short, int, short)
TEST3(signed, short, int, long)

TEST3(signed, int, char, short)
TEST3(signed, int, short, char)
TEST3(signed, int, char, int)
TEST3(signed, int, short, int)
TEST3(signed, int, char, long)
TEST3(signed, int, short, long)

TEST1(unsigned, short)
TEST1(unsigned, char)

TEST2(unsigned, short, char)

TEST3(unsigned, char, short, char)
TEST3(unsigned, short, char, short)

/* { dg-final { scan-assembler-times "sabd\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s, v\[0-9\]+\.4s" 48 } } */
/* { dg-final { scan-assembler-times "sabd\\tv\[0-9\]+\.8h, v\[0-9\]+\.8h, v\[0-9\]+\.8h" 7 } } */
/* { dg-final { scan-assembler-times "sabd\\tv\[0-9\]+\.16b, v\[0-9\]+\.16b, v\[0-9\]+\.16b" 1 } } */

/* { dg-final { scan-assembler-times "uabd\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s, v\[0-9\]+\.4s" 0 } } */
/* { dg-final { scan-assembler-times "uabd\\tv\[0-9\]+\.8h, v\[0-9\]+\.8h, v\[0-9\]+\.8h" 7 } } */
/* { dg-final { scan-assembler-times "uabd\\tv\[0-9\]+\.16b, v\[0-9\]+\.16b, v\[0-9\]+\.16b" 1 } } */

/* { dg-final { scan-assembler-not {\tsabdl\t} } } */
/* { dg-final { scan-assembler-not {\tsabdl2\t} } } */
/* { dg-final { scan-assembler-not {\tuabdl\t} } } */
/* { dg-final { scan-assembler-not {\tuabdl2\t} } } */
/* { dg-final { scan-assembler-not {\tabs\t} } } */
