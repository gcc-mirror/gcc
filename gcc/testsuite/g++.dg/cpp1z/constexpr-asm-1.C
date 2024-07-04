/* { dg-do compile } */
/* { dg-options "-std=gnu++17" } */

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
