/* Test for printf formats.  Test that the C90 functions get their default
   attributes in strict C90 mode, but the C99 and gettext functions
   do not.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile { target { *-*-mingw* } } } */
/* { dg-options "-std=iso9899:1990 -pedantic -Wformat" } */

#define USE_SYSTEM_FORMATS
#include "format.h"

void
foo (int i, char *s, size_t n, va_list v0, va_list v1, va_list v2, va_list v3,
     va_list v4, va_list v5, va_list v6, va_list v7, va_list v8)
{
  fprintf (stdout, "%d", i);
  fprintf (stdout, "%ld", i); /* { dg-warning "format" "fprintf" } */
  printf ("%d", i);
  printf ("%ld", i); /* { dg-warning "format" "printf" } */
  /* The "unlocked" functions shouldn't warn in c90 mode.  */
  fprintf_unlocked (stdout, "%ld", i);
  printf_unlocked ("%ld", i);
  sprintf (s, "%d", i);
  sprintf (s, "%ld", i); /* { dg-warning "format" "sprintf" } */
  vfprintf (stdout, "%d", v0);
  vfprintf (stdout, "%Y", v1); /* { dg-warning "format" "vfprintf" } */
  vprintf ("%d", v2);
  vprintf ("%Y", v3); /* { dg-warning "format" "vprintf" } */
  /* The following used to give a bogus warning.  */
  vprintf ("%*.*d", v8);   /* { dg-bogus "format" "vprintf" } */
  vsprintf (s, "%d", v4);
  vsprintf (s, "%Y", v5); /* { dg-warning "format" "Y is invalid" } */
  snprintf (s, n, "%d", i);
  snprintf (s, n, "%ld", i);
  vsnprintf (s, n, "%d", v6);
  vsnprintf (s, n, "%Y", v7);
  printf (gettext ("%d"), i);
  printf (gettext ("%ld"), i);
  printf (dgettext ("", "%d"), i);
  printf (dgettext ("", "%ld"), i);
  printf (dcgettext ("", "%d", 0), i);
  printf (dcgettext ("", "%ld", 0), i);
}
