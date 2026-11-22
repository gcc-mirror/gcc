/* { dg-do compile } */
/* { dg-options "-O2 -mfpmath=sse -march=skylake" } */

float a, b, c;
float x;
float y;
float z;

void foo()
{
  x = -a;
  y = -b;
  z = -c;
}

void bar()
{
  x = __builtin_fabsf(a);
  y = __builtin_fabsf(b);
  z = __builtin_fabsf(c);
}

/* { dg-final { scan-assembler-times "\[lL\]C0\[,(\]" 1 } } */
/* { dg-final { scan-assembler-times "\[lL\]C1\[,(\]" 1 } } */
