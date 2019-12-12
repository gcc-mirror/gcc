/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O3 -fno-math-errno -fno-fp-int-builtin-inexact" } */

#define TEST(name, float_type, int_type, fn) void f_##name (float_type x) \
{									  \
  volatile int_type   b = __builtin_##fn (x);				  \
}

TEST (dld, double, long, lrint)
TEST (flf, float , long, lrintf)

TEST (did, double, int, lrint)
TEST (fif, float , int, lrintf)

/* { dg-final { scan-assembler-times "fcvtzs\tw\[0-9\]+, \[d,s\]\[0-9\]+" 2 } } */
/* { dg-final { scan-assembler-times "bl\tlrint" 2 } } */
