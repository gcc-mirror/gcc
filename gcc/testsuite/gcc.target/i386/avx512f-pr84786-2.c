/* PR target/84786 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mavx512f -mno-avx512vl -O2" } */

typedef double V __attribute__((vector_size (16)));

__attribute__((noipa)) V
foo (V x, double y)
{
  register double z __asm ("xmm18");
  asm volatile ("" : "=v" (z) : "0" (y));
  x[1] = z;
  return x;
}

/* { dg-final { scan-assembler-not "vunpcklpd\[\^\n\r]*xmm(1\[6-9]|\[23]\[0-9])" } } */
