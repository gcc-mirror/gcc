/* Test for multiple format attributes.  Test for printf and scanf attributes
   together.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wformat" } */

#include "format.h"

/* If we specify multiple attributes for a single function, they should
   all apply.  This should apply whether they are on the same declaration
   or on different declarations.  */

extern void my_vprintf_scanf (const char *, va_list, const char *, ...)
     __attribute__((__format__(__printf__, 1, 0)))
     __attribute__((__format__(__scanf__, 3, 4)));

extern void my_vprintf_scanf2 (const char *, va_list, const char *, ...)
     __attribute__((__format__(__scanf__, 3, 4)))
     __attribute__((__format__(__printf__, 1, 0)));

extern void my_vprintf_scanf3 (const char *, va_list, const char *, ...)
     __attribute__((__format__(__printf__, 1, 0)));
extern void my_vprintf_scanf3 (const char *, va_list, const char *, ...)
     __attribute__((__format__(__scanf__, 3, 4)));

extern void my_vprintf_scanf4 (const char *, va_list, const char *, ...)
     __attribute__((__format__(__scanf__, 3, 4)));
extern void my_vprintf_scanf4 (const char *, va_list, const char *, ...)
     __attribute__((__format__(__printf__, 1, 0)));

void
foo (va_list ap, int *ip, long *lp)
{
  my_vprintf_scanf ("%d", ap, "%d", ip);
  my_vprintf_scanf ("%d", ap, "%ld", lp);
  my_vprintf_scanf ("%", ap, "%d", ip); /* { dg-warning "format" "printf" } */
  my_vprintf_scanf ("%d", ap, "%ld", ip); /* { dg-warning "format" "scanf" } */
  my_vprintf_scanf2 ("%d", ap, "%d", ip);
  my_vprintf_scanf2 ("%d", ap, "%ld", lp);
  my_vprintf_scanf2 ("%", ap, "%d", ip); /* { dg-warning "format" "printf" } */
  my_vprintf_scanf2 ("%d", ap, "%ld", ip); /* { dg-warning "format" "scanf" } */
  my_vprintf_scanf3 ("%d", ap, "%d", ip);
  my_vprintf_scanf3 ("%d", ap, "%ld", lp);
  my_vprintf_scanf3 ("%", ap, "%d", ip); /* { dg-warning "format" "printf" } */
  my_vprintf_scanf3 ("%d", ap, "%ld", ip); /* { dg-warning "format" "scanf" } */
  my_vprintf_scanf4 ("%d", ap, "%d", ip);
  my_vprintf_scanf4 ("%d", ap, "%ld", lp);
  my_vprintf_scanf4 ("%", ap, "%d", ip); /* { dg-warning "format" "printf" } */
  my_vprintf_scanf4 ("%d", ap, "%ld", ip); /* { dg-warning "format" "scanf" } */
}
