/* { dg-do compile } */
/* { dg-options "-O3" } */

#define N 1024

#define ABD_ABS
#include "../abd.h"

TEST1(signed, int)
TEST1(signed, short)
TEST1(signed, char)

TEST2(signed, char, int)
TEST2(signed, char, short)
TEST2(signed, short, int)

TEST3(signed, char, int, short)
TEST3(signed, char, short, int)

TEST1(unsigned, short)
TEST1(unsigned, char)

TEST2(unsigned, char, short)
TEST2(unsigned, char, int)
TEST2(unsigned, short, int)

TEST3(unsigned, char, short, int)

/* { dg-final { scan-assembler-times "sabd\\tz\[0-9\]+\.s, p\[0-9\]/m, z\[0-9\]+\.s, z\[0-9\]+\.s" 2 } } */
/* { dg-final { scan-assembler-times "sabd\\tz\[0-9\]+\.h, p\[0-9\]/m, z\[0-9\]+\.h, z\[0-9\]+\.h" 3 } } */
/* { dg-final { scan-assembler-times "sabd\\tz\[0-9\]+\.b, p\[0-9\]/m, z\[0-9\]+\.b, z\[0-9\]+\.b" 3 } } */
/* { dg-final { scan-assembler-times "uabd\\tz\[0-9\]+\.h, p\[0-9\]/m, z\[0-9\]+\.h, z\[0-9\]+\.h" 3 } } */
/* { dg-final { scan-assembler-times "uabd\\tz\[0-9\]+\.b, p\[0-9\]/m, z\[0-9\]+\.b, z\[0-9\]+\.b" 3 } } */

/* { dg-final { scan-assembler-not {\tabs\t} } } */
