// PR target/103973
// { dg-do run }
// { dg-options "-O2 -std=c++20 -save-temps" }
// { dg-final { scan-assembler-not "'\t\[v\]?ucomisd" { target { ! ia32 } } } }
// { dg-final { scan-assembler-times "\t\[v\]?comisd" 2 { target { ! ia32 } } } }

#include <compare>

#ifndef double_type
#define double_type double
#endif

__attribute__((noipa)) auto
foo (double_type a, double_type b)
{
  return a <=> b;
}

__attribute__((noipa)) int
bar (double_type a, double_type b)
{
  auto c = foo (a, b);
  if (c == std::partial_ordering::less)
    return -1;
  if (c == std::partial_ordering::equivalent)
    return 0;
  if (c == std::partial_ordering::greater)
    return 1;
  return 2;
}

__attribute__((noipa)) auto
baz (double_type a)
{
  return a <=> 0.0f;
}

__attribute__((noipa)) int
qux (double_type a)
{
  auto c = baz (a);
  if (c == std::partial_ordering::less)
    return -1;
  if (c == std::partial_ordering::equivalent)
    return 0;
  if (c == std::partial_ordering::greater)
    return 1;
  return 2;
}

int
main ()
{
  double_type m5 = -5.0;
  double_type p5 = 5.0;
  volatile double_type p0 = 0.0;
  double_type nan = p0 / p0;
  if (bar (p5, p5) != 0 || bar (m5, m5) != 0)
    __builtin_abort ();
  if (bar (m5, p5) != -1 || bar (p5, m5) != 1)
    __builtin_abort ();
  if (bar (m5, nan) != 2 || bar (nan, p5) != 2)
    __builtin_abort ();
  if (bar (nan, nan) != 2)
    __builtin_abort ();
  if (qux (p0) != 0 || qux (nan) != 2)
    __builtin_abort ();
  if (qux (m5) != -1 || qux (p5) != 1)
    __builtin_abort ();
  return 0;
}
