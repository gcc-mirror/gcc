/* Test for format attributes: test use of __attribute__
   in prefix attributes.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wformat" } */

#define DONT_GNU_PROTOTYPE
#include "format.h"

extern __attribute__((format(gnu_attr_printf, 1, 2))) void tformatprintf0 (const char *, ...);
extern void __attribute__((format(gnu_attr_printf, 1, 2))) tformatprintf1 (const char *, ...);
extern void foo (void), __attribute__((format(gnu_attr_printf, 1, 2))) tformatprintf2 (const char *, ...);
extern __attribute__((noreturn)) void bar (void), __attribute__((format(gnu_attr_printf, 1, 2))) tformatprintf3 (const char *, ...);

void
baz (int i, int *ip, double d)
{
  tformatprintf0 ("%d", i);
  tformatprintf0 ("%"); /* { dg-warning "20:format" "attribute format printf case 0" } */
  tformatprintf1 ("%d", i);
  tformatprintf1 ("%"); /* { dg-warning "format" "attribute format printf case 1" } */
  tformatprintf2 ("%d", i);
  tformatprintf2 ("%"); /* { dg-warning "format" "attribute format printf case 2" } */
  tformatprintf3 ("%d", i);
  tformatprintf3 ("%"); /* { dg-warning "format" "attribute format printf case 3" } */
}
