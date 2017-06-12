/* PR target/80706 */
/* { dg-do run { target sse2_runtime } } */
/* { dg-options "-O2 -msse2" } */

union U { double value; struct S { int lsw; int msw; } parts; };

__attribute__((noinline, noclone)) double
foo (void)
{
  __asm volatile ("" : : : "memory");
  return 2.0;
}

__attribute__((noinline, noclone)) double
bar (void)
{
  double s = foo ();
  union U z;
  z.value = s;
  z.parts.lsw = 0;
  return z.value * z.value + s * s;
}

int
main ()
{
  if (bar () != 8.0)
    __builtin_abort ();
  return 0;
}
