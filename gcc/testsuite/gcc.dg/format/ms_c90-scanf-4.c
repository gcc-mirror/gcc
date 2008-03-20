/* Test for scanf formats.  Test that the C90 functions get their default
   attributes in strict C90 mode, but the C99 and gettext functions
   do not.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile { target { *-*-mingw* } } } */
/* { dg-options "-std=iso9899:1990 -pedantic -Wformat" } */

#define USE_SYSTEM_FORMATS
#include "format.h"

void
foo (int *ip, char *s, va_list v0, va_list v1, va_list v2, va_list v3,
     va_list v4, va_list v5)
{
  fscanf (stdin, "%d", ip);
  fscanf (stdin, "%ld", ip); /* { dg-warning "format" "fscanf" } */
  scanf ("%d", ip);
  scanf ("%ld", ip); /* { dg-warning "format" "scanf" } */
  sscanf (s, "%d", ip);
  sscanf (s, "%ld", ip); /* { dg-warning "format" "sscanf" } */
  vfscanf (stdin, "%d", v0);
  vscanf ("%d", v2);
  vsscanf (s, "%d", v4);
  scanf (gettext ("%d"), ip);
  scanf (gettext ("%ld"), ip);
  scanf (dgettext ("", "%d"), ip);
  scanf (dgettext ("", "%ld"), ip);
  scanf (dcgettext ("", "%d", 0), ip);
  scanf (dcgettext ("", "%ld", 0), ip);
}
