/* PR77336 - -Wsuggest-attribute=format warning overly simplistic */
/* { dg-do compile } */
/* { dg-options "-Wsuggest-attribute=format" } */

#include "format.h"

const char format[] = "%i";

void foo (char *d, unsigned n, va_list va)
{
  (void)&n;

  /* The following calls don't imply that the enclosing function is
     a candiate for the format attribute because it uses a string
     constant as the format.  */
  vsnprintf (d, n, "%i", va);

  vsnprintf (d, n, format, va);

  /* In theory this should not trigger the warning either but GCC
     doesn't treat the local static constant the same way as the
     global and issues a false positive.
  const char fmt[] = "%i";
  vsnprintf (d, n, fmt, va);
  */
}

void bar (char *d, unsigned n, const char *f, va_list va)
{
  (void)&n;

  /* The following call suggests that the enclosing function might
     be a candiate for the format attribute because it doesn't use
     a string literal as the format.  */
  vsnprintf (d, n, f, va);   /* { dg-warning "function .bar. might be a candidate for .gnu_printf. format attribute" } */
}
