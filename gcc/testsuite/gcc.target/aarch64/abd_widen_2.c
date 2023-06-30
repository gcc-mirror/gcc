/* { dg-do compile } */
/* { dg-options "-O3" } */

#pragma GCC target "+nosve"
#define N 1024

#define ABD_ABS
#include "abd.h"

TEST2(signed, char, short)
TEST2(signed, char, int)
TEST2(signed, char, long)
TEST2(signed, short, int)
TEST2(signed, short, long)

TEST3(signed, char, short, int)
TEST3(signed, char, short, long)

TEST3(signed, short, char, int)
TEST3(signed, short, char, long)

TEST2(unsigned, char, short)
TEST2(unsigned, char, int)
TEST2(unsigned, char, long)
TEST2(unsigned, short, int)
TEST2(unsigned, short, long)

TEST3(unsigned, char, short, int)
TEST3(unsigned, char, short, long)

TEST3(unsigned, short, char, int)
TEST3(unsigned, short, char, long)

/* { dg-final { scan-assembler-times "sabdl\\tv\[0-9\]+\.2d, v\[0-9\]+\.2s, v\[0-9\]+\.2s" 0 } } */
/* { dg-final { scan-assembler-times "sabdl2\\tv\[0-9\]+\.2d, v\[0-9\]+\.4s, v\[0-9\]+\.4s" 0 } } */
/* { dg-final { scan-assembler-times "sabdl\\tv\[0-9\]+\.4s, v\[0-9\]+\.4h, v\[0-9\]+\.4h" 10 } } */
/* { dg-final { scan-assembler-times "sabdl2\\tv\[0-9\]+\.4s, v\[0-9\]+\.8h, v\[0-9\]+\.8h" 10 } } */
/* { dg-final { scan-assembler-times "sabdl\\tv\[0-9\]+\.8h, v\[0-9\]+\.8b, v\[0-9\]+\.8b" 3 } } */
/* { dg-final { scan-assembler-times "sabdl2\\tv\[0-9\]+\.8h, v\[0-9\]+\.16b, v\[0-9\]+\.16b" 3 } } */

/* { dg-final { scan-assembler-times "uabdl\\tv\[0-9\]+\.2d, v\[0-9\]+\.2s, v\[0-9\]+\.2s" 0 } } */
/* { dg-final { scan-assembler-times "uabdl2\\tv\[0-9\]+\.2d, v\[0-9\]+\.4s, v\[0-9\]+\.4s" 0 } } */
/* { dg-final { scan-assembler-times "uabdl\\tv\[0-9\]+\.4s, v\[0-9\]+\.4h, v\[0-9\]+\.4h" 10 } } */
/* { dg-final { scan-assembler-times "uabdl2\\tv\[0-9\]+\.4s, v\[0-9\]+\.8h, v\[0-9\]+\.8h" 10 } } */
/* { dg-final { scan-assembler-times "uabdl\\tv\[0-9\]+\.8h, v\[0-9\]+\.8b, v\[0-9\]+\.8b" 3 } } */
/* { dg-final { scan-assembler-times "uabdl2\\tv\[0-9\]+\.8h, v\[0-9\]+\.16b, v\[0-9\]+\.16b" 3 } } */

/* { dg-final { scan-assembler-not {\tsabd\t} } } */
/* { dg-final { scan-assembler-not {\tuabd\t} } } */
/* { dg-final { scan-assembler-not {\tabs\t} } } */
