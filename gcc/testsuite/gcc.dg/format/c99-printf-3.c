/* Test for printf formats.  Test that the C99 functions get their default
   attributes in strict C99 mode, but the gettext functions do not.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic -Wformat" } */

#include "format.h"

void
foo (int i, char *s, size_t n, va_list v0, va_list v1, va_list v2, va_list v3,
     va_list v4, va_list v5, va_list v6, va_list v7)
{
  fprintf (stdout, "%d", i);
  fprintf (stdout, "%ld", i); /* { dg-warning "format" "fprintf" } */
  printf ("%d", i);
  printf ("%ld", i); /* { dg-warning "format" "printf" } */
  /* The "unlocked" functions shouldn't warn in c99 mode.  */
  fprintf_unlocked (stdout, "%ld", i); /* { dg-bogus "format" "fprintf_unlocked" } */
  printf_unlocked ("%ld", i); /* { dg-bogus "format" "printf_unlocked" } */
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
  printf (gettext ("%ld"), i);
  printf (dgettext ("", "%d"), i);
  printf (dgettext ("", "%ld"), i);
  printf (dcgettext ("", "%d", 0), i);
  printf (dcgettext ("", "%ld", 0), i);
}
