/* Test for multiple format_arg attributes.  Test for both branches
   getting checked.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wformat" } */

#include "format.h"

extern char *ngettext (const char *, const char *, unsigned long int)
     __attribute__((__format_arg__(1))) __attribute__((__format_arg__(2)));

void
foo (long l, int nfoo)
{
  printf (ngettext ("%d foo", "%d foos", nfoo), nfoo);
  printf (ngettext ("%d foo", "%d foos", l), l); /* { dg-warning "int format" "wrong type in conditional expr" } */
  printf (ngettext ("%d foo", "%ld foos", l), l); /* { dg-warning "int format" "wrong type in conditional expr" } */
  printf (ngettext ("%ld foo", "%d foos", l), l); /* { dg-warning "int format" "wrong type in conditional expr" } */
  /* Should allow one case to have extra arguments.  */
  printf (ngettext ("1 foo", "%d foos", nfoo), nfoo);
  printf (ngettext ("1 foo", "many foos", nfoo), nfoo); /* { dg-warning "too many" "too many args in all branches" } */
  printf (ngettext ("", "%d foos", nfoo), nfoo);
  printf (ngettext ("1 foo", (nfoo > 0) ? "%d foos" : "no foos", nfoo), nfoo);
  printf (ngettext ("%d foo", (nfoo > 0) ? "%d foos" : "no foos", nfoo), nfoo);
  printf (ngettext ("%ld foo", (nfoo > 0) ? "%d foos" : "no foos", nfoo), nfoo); /* { dg-warning "long int format" "wrong type" } */
  printf (ngettext ("%d foo", (nfoo > 0) ? "%ld foos" : "no foos", nfoo), nfoo); /* { dg-warning "long int format" "wrong type" } */
  printf (ngettext ("%d foo", (nfoo > 0) ? "%d foos" : "%ld foos", nfoo), nfoo); /* { dg-warning "long int format" "wrong type" } */
}
