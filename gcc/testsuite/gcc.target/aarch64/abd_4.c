/* { dg-do compile } */
/* { dg-options "-O3" } */

#pragma GCC target "+nosve"
#define N 1024

#define ABD_IDIOM
#include "abd.h"

TEST1(signed, int)

TEST2(signed, char, short)
TEST2(signed, char, int)
TEST2(signed, short, int)

TEST3(signed, char, short, int)

TEST2(unsigned, char, short)
TEST2(unsigned, char, int)
TEST2(unsigned, short, int)

TEST3(unsigned, char, short, int)

/* { dg-final { scan-assembler-times "sabd\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s, v\[0-9\]+\.4s" 1 } } */
/* { dg-final { scan-assembler-times "sabd\\tv\[0-9\]+\.8h, v\[0-9\]+\.8h, v\[0-9\]+\.8h" 3 } } */
/* { dg-final { scan-assembler-times "sabd\\tv\[0-9\]+\.16b, v\[0-9\]+\.16b, v\[0-9\]+\.16b" 2 } } */
/* { dg-final { scan-assembler-times "uabd\\tv\[0-9\]+\.8h, v\[0-9\]+\.8h, v\[0-9\]+\.8h" 3 } } */
/* { dg-final { scan-assembler-times "uabd\\tv\[0-9\]+\.16b, v\[0-9\]+\.16b, v\[0-9\]+\.16b" 2 } } */

/* { dg-final { scan-assembler-not {\tabs\t} } } */
