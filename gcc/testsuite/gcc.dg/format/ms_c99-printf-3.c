/* Test for printf formats.  Test that the C99 functions get their default
   attributes in strict C99 mode, but the gettext functions do not.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile { target { *-*-mingw* } } } */
/* { dg-options "-std=iso9899:1999 -pedantic -Wformat" } */

#define USE_SYSTEM_FORMATS
#include "format.h"

void
foo (int i, char *s, size_t n, long l, llong ll, double d,
     long double ld, va_list v0, va_list v1, va_list v2, va_list v3,
     va_list v4, va_list v5, va_list v6, va_list v7)
{
  fprintf (stdout, "%d", i);
  fprintf (stdout, "%ld", i); /* { dg-warning "format" "fprintf" } */
  printf ("%d", i);
  printf ("%ld", i); /* { dg-warning "format" "printf" } */
  /* These are accepted since MSVCR80, MSVCRT from Vista, UCRT,
   * and mingw-w64 8.0 with C99/C++11.  */
  printf ("%lld", i); /* { dg-warning "format" "printf" } */
  printf ("%lld", l); /* { dg-warning "format" "printf" } */
  printf ("%lld", ll);
  printf ("%llu", i); /* { dg-warning "format" "printf" } */
  printf ("%llu", l); /* { dg-warning "format" "printf" } */
  printf ("%llu", ll);
  printf ("%llx", i); /* { dg-warning "format" "printf" } */
  printf ("%llx", l); /* { dg-warning "format" "printf" } */
  printf ("%llx", ll);
  /* As MSABI uses an 8-byte `long double`, `%Lg` matches GCC's
   * `double` instead of `long double` which takes 10 bytes.  */
  printf ("%Lg", d);
  printf ("%Lg", ld); /* { dg-warning "format" "printf" } */
  printf ("%Le", d);
  printf ("%Le", ld); /* { dg-warning "format" "printf" } */
  printf ("%Lf", d);
  printf ("%Lf", ld); /* { dg-warning "format" "printf" } */
  /* The "unlocked" functions shouldn't warn in c99 mode.  */
  fprintf_unlocked (stdout, "%ld", i);
  printf_unlocked ("%ld", i);
  sprintf (s, "%d", i);
  sprintf (s, "%ld", i); /* { dg-warning "format" "sprintf" } */
  snprintf (s, n, "%d", i);
  snprintf (s, n, "%ld", i); /* { dg-warning "format" "snprintf" } */
  vfprintf (stdout, "%d", v0);
  vfprintf (stdout, "%Y", v1); /* { dg-warning "format" "vfprintf" } */
  vprintf ("%d", v0);
  vprintf ("%Y", v1); /* { dg-warning "format" "vprintf" } */
  vsprintf (s, "%d", v0);
  vsprintf (s, "%Y", v1); /* { dg-warning "format" "vsprintf" } */
  vsnprintf (s, n, "%d", v0);
  vsnprintf (s, n, "%Y", v1); /* { dg-warning "format" "vsnprintf" } */
  printf (gettext ("%d"), i);
  printf (gettext ("%ld"), (long) i);
  printf (dgettext ("", "%d"), i);
  printf (dgettext ("", "%ld"), (long) i);
  printf (dcgettext ("", "%d", 0), i);
  printf (dcgettext ("", "%ld", 0), (long) i);
}
