/* PR middle-end/100684 - spurious -Wnonnull with -O1 on a C++ lambda
   { dg-do compile { target c++11 } }
   { dg-options "-O1 -Wall -fsanitize=undefined" } */

#define NONNULL  __attribute__ ((nonnull))

typedef int F (const char *);

NONNULL int f (const char *);

int nowarn_O1 ()
{
  return static_cast<F*>([](const char *s){ return f (s); })("O1");
  // { dg-bogus "\\\[-Wnonnull" "" { target *-*-* } .-1 }
}

int warn_O1 ()
{
  return static_cast<F*>([] NONNULL (const char *){ return 0; })(0);
  // { dg-warning "\\\[-Wnonnull" "" { target *-*-* } .-1 }
}

int warn_O1_inline ()
{
  const char *p = 0;
  return static_cast<F*>([](const char *s){ return f (s); })(p);
  // { dg-warning "\\\[-Wnonnull" "lambda not inlined" { xfail *-*-* } .-1 }
}
