/* PR target/103973 */
/* { dg-do run } */
/* { dg-options "-O2 -save-temps" } */
/* { dg-final { scan-assembler-not "'\t\[v\]?ucomisd" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "\t\[v\]?comisd" 4 { target { ! ia32 } } } } */

__attribute__((noipa)) int m1 (void) { return -1; }
__attribute__((noipa)) int p0 (void) { return 0; }
__attribute__((noipa)) int p1 (void) { return 1; }
__attribute__((noipa)) int p2 (void) { return 2; }

__attribute__((noipa)) int
foo (double a, double b)
{
  if (a == b)
    return 0;
  if (a < b)
    return -1;
  if (a > b)
    return 1;
  return 2;
}

__attribute__((noipa)) int
bar (double a, double b)
{
  if (a == b)
    return p0 ();
  if (a < b)
    return m1 ();
  if (a > b)
    return p1 ();
  return p2 ();
}

__attribute__((noipa)) int
baz (double a, double b)
{
  if (a == b)
    return p0 ();
  if (b < a)
    return p1 ();
  if (a < b)
    return m1 ();
  return p2 ();
}

__attribute__((noipa)) int
qux (double a)
{
  if (a != 0.0f)
    {
      if (a <= 0.0f)
	return -1;
      if (a >= 0.0f)
	return 1;
      return 2;
    }
  return 0;
}

int
main ()
{
  double m5 = -5.0f;
  double p5 = 5.0f;
  volatile double p0 = 0.0f;
  double nan = p0 / p0;
  if (foo (p5, p5) != 0 || foo (m5, m5) != 0)
    __builtin_abort ();
  if (foo (m5, p5) != -1 || foo (p5, m5) != 1)
    __builtin_abort ();
  if (foo (m5, nan) != 2 || foo (nan, p5) != 2)
    __builtin_abort ();
  if (foo (nan, nan) != 2)
    __builtin_abort ();
  if (bar (p5, p5) != 0 || bar (m5, m5) != 0)
    __builtin_abort ();
  if (bar (m5, p5) != -1 || bar (p5, m5) != 1)
    __builtin_abort ();
  if (bar (m5, nan) != 2 || bar (nan, p5) != 2)
    __builtin_abort ();
  if (bar (nan, nan) != 2)
    __builtin_abort ();
  if (baz (p5, p5) != 0 || baz (m5, m5) != 0)
    __builtin_abort ();
  if (baz (m5, p5) != -1 || baz (p5, m5) != 1)
    __builtin_abort ();
  if (baz (m5, nan) != 2 || baz (nan, p5) != 2)
    __builtin_abort ();
  if (baz (nan, nan) != 2)
    __builtin_abort ();
  if (qux (p0) != 0 || qux (nan) != 2)
    __builtin_abort ();
  if (qux (m5) != -1 || qux (p5) != 1)
    __builtin_abort ();
  return 0;
}
