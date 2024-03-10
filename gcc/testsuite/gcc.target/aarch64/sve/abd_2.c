/* { dg-do compile } */
/* { dg-options "-O3" } */

#define N 1024

#define ABD_IDIOM
#include "../abd.h"

TEST1(signed, int)
TEST1(signed, long)

TEST2(signed, char, short)
TEST2(signed, char, int)
TEST2(signed, char, long)
TEST2(signed, short, int)
TEST2(signed, short, long)
TEST2(signed, int, long)

TEST3(signed, char, short, int)
TEST3(signed, char, short, long)
TEST3(signed, char, int, long)

TEST3(signed, short, char, int)
TEST3(signed, short, char, long)
TEST3(signed, short, int, long)

TEST3(signed, int, char, long)
TEST3(signed, int, short, long)
TEST3(signed, int, char, int)
TEST3(signed, int, short, int)

TEST3(signed, long, char, long)
TEST3(signed, long, short, long)
TEST3(signed, long, int, long)

TEST2(unsigned, char, short)
TEST2(unsigned, char, int)
TEST2(unsigned, char, long)
TEST2(unsigned, short, int)
TEST2(unsigned, short, long)

TEST3(unsigned, char, short, int)
TEST3(unsigned, char, short, long)

TEST3(unsigned, short, char, int)
TEST3(unsigned, short, char, long)

/* { dg-final { scan-assembler-times "sabd\\tz\[0-9\]+\.d, p\[0-9\]/m, z\[0-9\]+\.d, z\[0-9\]+\.d" 4 } } */
/* { dg-final { scan-assembler-times "sabd\\tz\[0-9\]+\.s, p\[0-9\]/m, z\[0-9\]+\.s, z\[0-9\]+\.s" 8 } } */
/* { dg-final { scan-assembler-times "sabd\\tz\[0-9\]+\.h, p\[0-9\]/m, z\[0-9\]+\.h, z\[0-9\]+\.h" 6 } } */
/* { dg-final { scan-assembler-times "sabd\\tz\[0-9\]+\.b, p\[0-9\]/m, z\[0-9\]+\.b, z\[0-9\]+\.b" 3 } } */

/* { dg-final { scan-assembler-times "uabd\\tz\[0-9\]+\.d, p\[0-9\]/m, z\[0-9\]+\.d, z\[0-9\]+\.d" 0 } } */
/* { dg-final { scan-assembler-times "uabd\\tz\[0-9\]+\.s, p\[0-9\]/m, z\[0-9\]+\.s, z\[0-9\]+\.s" 0 } } */
/* { dg-final { scan-assembler-times "uabd\\tz\[0-9\]+\.h, p\[0-9\]/m, z\[0-9\]+\.h, z\[0-9\]+\.h" 6 } } */
/* { dg-final { scan-assembler-times "uabd\\tz\[0-9\]+\.b, p\[0-9\]/m, z\[0-9\]+\.b, z\[0-9\]+\.b" 3 } } */

/* { dg-final { scan-assembler-not {\tsabdl\t} } } */
/* { dg-final { scan-assembler-not {\tsabdl2\t} } } */
/* { dg-final { scan-assembler-not {\tuabdl\t} } } */
/* { dg-final { scan-assembler-not {\tuabdl2\t} } } */
/* { dg-final { scan-assembler-not {\tabs\t} } } */
