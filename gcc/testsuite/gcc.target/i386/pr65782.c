/* PR target/65782 */
/* { dg-do assemble { target { avx512vl && { ! ia32 } } } } */
/* { dg-options "-O2 -mavx512vl" } */

void
foo (void)
{
  register double x __asm ("xmm14");
  register double y __asm ("xmm18");
  asm ("" : "=x" (x));
  asm ("" : "=v" (y));
  x += y;
  y += x;
  asm ("" : : "x" (x));
  asm ("" : : "v" (y));
}
