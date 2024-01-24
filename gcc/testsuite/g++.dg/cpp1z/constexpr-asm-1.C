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
  asm((genfoo ()) : (genoutput ()) (a) : (geninput ()) (1) : (genclobber ()));
}

/* { dg-final { scan-assembler "foo" } } */
