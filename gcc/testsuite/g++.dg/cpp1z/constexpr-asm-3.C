/* { dg-do compile } */
/* { dg-options "-std=gnu++11" } */

constexpr const char *genfoo ()
{
  return "foo %1,%0";
}

constexpr const char *genoutput ()
{
  return "=r";
}

constexpr const char *geninput ()
{
  return "r";
}

constexpr const char *genclobber ()
{
  return "memory";
}

void f()
{
  int a;
  asm(genfoo () : /* { dg-error "expected string-literal or constexpr in brackets" } */
      genoutput() (a) :
      geninput() (1) :
      genclobber());
}
