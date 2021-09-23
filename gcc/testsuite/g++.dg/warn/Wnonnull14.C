/* PR middle-end/100684 - spurious -Wnonnull with -O1 on a C++ lambda
   { dg-do compile { target c++11 } }
   { dg-options "-Og -Wall -fsanitize=undefined" } */

#define NONNULL  __attribute__ ((nonnull))

typedef int F (const char *);

__attribute__ ((nonnull)) int f (const char *);

int nowarn_Og ()
{
  return static_cast<F*>([](const char *s){ return f (s); })("Og");
  // { dg-bogus "'this' pointer is null" "" { target *-*-* } .-1 }
}

int warn_Og ()
{
  return static_cast<F*>([] NONNULL (const char *){ return 0; })(0);
  // { dg-warning "\\\[-Wnonnull" "" { target *-*-* } .-1 }
}

int warn_Og_inline ()
{
  const char *p = 0;
  return static_cast<F*>([](const char *s){ return f (s); })(p);
  // { dg-warning "\\\[-Wnonnull" "lambda not inlined" { xfail *-*-* } .-1 }
}
