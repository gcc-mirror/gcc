/* Test for multiple format attributes.  Test for printf and scanf attributes
   together, in different places on the declarations.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wformat" } */

#define DONT_GNU_PROTOTYPE
#include "format.h"

/* If we specify multiple attributes for a single function, they should
   all apply, wherever they are placed on the declarations.  */

extern __attribute__((__format__(gnu_attr___printf__, 1, 0))) void
     my_vprintf_scanf (const char *, va_list, const char *, ...)
     __attribute__((__format__(gnu_attr___scanf__, 3, 4)));

extern void (__attribute__((__format__(gnu_attr___printf__, 1, 0))) my_vprintf_scanf2)
     (const char *, va_list, const char *, ...)
     __attribute__((__format__(gnu_attr___scanf__, 3, 4)));

extern __attribute__((__format__(gnu_attr___scanf__, 3, 4))) void
     (__attribute__((__format__(gnu_attr___printf__, 1, 0))) my_vprintf_scanf3)
     (const char *, va_list, const char *, ...);

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
}
