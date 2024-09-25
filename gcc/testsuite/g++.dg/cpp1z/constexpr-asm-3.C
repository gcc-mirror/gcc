/* { dg-do compile } */
/* { dg-options "-std=gnu++17" } */
/* { dg-skip-if "requires hosted libstdc++ for string" { ! hostedlib } } */

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
  asm(genfoo () : /* { dg-error "expected string-literal or constexpr in parentheses" } */
      genoutput() (a) :
      geninput() (1) :
      genclobber());
}
