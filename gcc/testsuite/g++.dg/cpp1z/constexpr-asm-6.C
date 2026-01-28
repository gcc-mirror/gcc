/* { dg-do compile { target c++17 } } */
/* { dg-skip-if "requires hosted libstdc++ for string" { ! hostedlib } } */
// { dg-require-iconv "IBM1047" }
// { dg-options "-fexec-charset=IBM1047" }

#include <string>

constexpr std::string_view genfoo ()
{
  return "foo %1,%0";
}

constexpr std::string_view genoutput ()
{
  return "=r";
}

constexpr std::string_view geninput ()
{
  return "r";
}

constexpr std::string_view genclobber ()
{
  return "memory";
}

void f()
{
  int a;
  asm((genfoo ()) : (genoutput ()) (a) : (geninput ()) (1) : (genclobber ()));
}

/* { dg-final { scan-assembler "foo" } } */
