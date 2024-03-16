/* Test for strftime formats.  Formats using C23 features.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic -Wformat" } */

#include "format.h"

void
foo (char *s, size_t m, const struct tm *tp)
{
  strftime (s, m, "%Ob", tp);
  strftime (s, m, "%OB", tp);
  /* It's not clear that %h equivalence to %b means %Oh is equivalent
     to %Ob; here we expect %Oh to be diagnosed.  */
  strftime (s, m, "%Oh", tp); /* { dg-warning "flag|modifier" "bad %Oh" } */
}
