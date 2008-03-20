/* Test for some aspects of null format string handling.  */
/* Origin: Jason Thorpe <thorpej@wasabisystems.com> */
/* { dg-do compile { target { *-*-mingw* } } } */
/* { dg-options "-std=gnu99 -Wformat" } */

#define USE_SYSTEM_FORMATS
#include "format.h"

extern void my_printf (const char *, ...) __attribute__((format(ms_printf,1,2)));
extern const char *my_format (const char *, const char *)
  __attribute__((format_arg(2)));

void
foo (int i1)
{
  /* Warning about a null format string has been decoupled from the actual
     format check.  However, we still expect to be warned about any excess
     arguments after a null format string.  */
  my_printf (NULL);
  my_printf (NULL, i1); /* { dg-warning "too many" "null format with arguments" } */

  my_printf (my_format ("", NULL));
  my_printf (my_format ("", NULL), i1); /* { dg-warning "too many" "null format_arg with arguments" } */

  /* While my_printf allows a null argument, dgettext does not, so we expect
     a null argument warning here.  */
  my_printf (dgettext ("", NULL)); /* { dg-warning "null" "null format with dgettext" } */
}
