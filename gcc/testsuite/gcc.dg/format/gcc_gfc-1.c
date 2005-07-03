/* Test for gcc_gfc formats.  */
/* Origin: Kaveh Ghazi <ghazi@caip.rutgers.edu> */
/* { dg-do compile } */
/* { dg-options "-Wformat" } */

#include "format.h"

/* Magic identifier must be set before the attribute is used.  */
typedef struct locus locus;

extern int gfc_warn (const char *, ...) __attribute__ ((__format__ (__gcc_gfc__, 1, 2))) __attribute__ ((__nonnull__));

void
foo (int i, char *s, long int l, llong ll, locus *loc)
{
  /* Acceptable C90 specifiers, flags and modifiers.  */
  gfc_warn ("%%");
  gfc_warn ("%d%i%c%s%%", i, i, i, s);

  /* Extensions provided in gfc_warn.  */
  gfc_warn ("%C");
  gfc_warn ("%L", loc);

  /* Various tests of bad argument types.  */
  gfc_warn ("%d", l); /* { dg-warning "format" "bad argument types" } */
  gfc_warn ("%d", ll); /* { dg-warning "format" "bad argument types" } */
  gfc_warn ("%s", &i); /* { dg-warning "format" "bad argument types" } */
  gfc_warn ("%L", &i); /* { dg-warning "format" "bad argument types" } */
  gfc_warn ("%C", i); /* { dg-warning "format" "too many arguments" } */
}
