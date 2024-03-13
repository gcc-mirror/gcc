/* PR middle-end/114319 */
/* { dg-do compile } */
/* { dg-options "-O2 -masm=att -mno-movbe" } */
/* { dg-additional-options "-march=i486" { target ia32 } } */
/* { dg-final { scan-assembler-times "\tbswap\t%r" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "\tbswap\t%\[er]" 2 { target ia32 } } } */

void
foo (unsigned long long x, unsigned char *y)
{
  y[0] = x >> 56;
  y[1] = x >> 48;
  y[2] = x >> 40;
  y[3] = x >> 32;
  y[4] = x >> 24;
  y[5] = x >> 16;
  y[6] = x >> 8;
  y[7] = x;
}
