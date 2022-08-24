/* { dg-do compile } */
/* { dg-options "-O2 -mssse3" } */

typedef long long __attribute__ ((__vector_size__ (8))) T;

T x;
T y;
T z;

void foo()
{
  z = __builtin_ia32_palignr (x, y, 0);
}

void bar()
{
  z = __builtin_ia32_palignr (x, y, 64);
}
/* { dg-final { scan-assembler-not "punpcklqdq" } } */
/* { dg-final { scan-assembler-not "pshufd" } } */
/* { dg-final { scan-assembler-not "psrldq" } } */
