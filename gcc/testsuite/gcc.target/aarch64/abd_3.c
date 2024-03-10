/* { dg-do compile } */
/* { dg-options "-Ofast" } */

#pragma GCC target "arch=armv8-a"
#define N 1024

#define ABD_ABS
#include "abd.h"

TEST1(signed, int)
TEST1(signed, short)
TEST1(signed, char)

TEST2(signed, char, short)
TEST2(signed, char, int)
TEST2(signed, short, int)

TEST3(signed, char, int, short)
TEST3(signed, char, short, int)

TEST1(unsigned, short)
TEST1(unsigned, char)

TEST2(unsigned, char, short)
TEST2(unsigned, char, int)
TEST2(unsigned, short, int)

TEST3(unsigned, char, short, int)

/* { dg-final { scan-assembler-times "sabd\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s, v\[0-9\]+\.4s" 5 } } */
/* { dg-final { scan-assembler-times "sabd\\tv\[0-9\]+\.8h, v\[0-9\]+\.8h, v\[0-9\]+\.8h" 4 } } */
/* { dg-final { scan-assembler-times "sabd\\tv\[0-9\]+\.16b, v\[0-9\]+\.16b, v\[0-9\]+\.16b" 3 } } */
/* { dg-final { scan-assembler-times "uabd\\tv\[0-9\]+\.8h, v\[0-9\]+\.8h, v\[0-9\]+\.8h" 4 } } */
/* { dg-final { scan-assembler-times "uabd\\tv\[0-9\]+\.16b, v\[0-9\]+\.16b, v\[0-9\]+\.16b" 3 } } */

/* { dg-final { scan-assembler-not {\tabs\t} } } */
