/* { dg-do compile } */
/* { dg-options "-march=rv32gcb_zicond -mabi=ilp32 -mbranch-cost=4" { target { rv32 } } } */
/* { dg-options "-march=rv64gcb_zicond -mabi=lp64d -mbranch-cost=4" { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-O1" "-Os" "-Oz" } } */

#define TEST(NAME,OP) int f_int_##NAME(int a, int b, int c) { if (c) a OP b; return a; } \
                      long f_long_##NAME(long a, long b, long c) { if (c) a OP b; return a; }

TEST(xor, ^=)
TEST(ior, |=)
TEST(and, &=)
TEST(plus, +=)
TEST(minus, -=)
TEST(rshift, >>=)
TEST(lshift, <<=)

/* AND and MULT can be handled too, but need a different neutral element that
   we aren't handling yet.  */

/* Each test should have precisely one czero.  But for rv64, the int tests
   generate 2 for int cases where the op is widened.  */
/* { dg-final { scan-assembler-times "czero" 14 { target rv32 } } } */
/* { dg-final { scan-assembler-times "czero" 18 { target rv64 } } } */
