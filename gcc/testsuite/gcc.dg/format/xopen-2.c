/* Test for X/Open format extensions, as found in the
   Single Unix Specification.  Test for bug reported by
   Pierre-Canalsat PETIT <pierrecanalsat.petit.canalsat@canal-plus.com>
   in PR c/6547.  The test for absence of a parameter for a * width was done
   too early in the case of operand numbers and vprintf formats.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wformat" } */

#include "format.h"

void vbar (va_list, const char *) __attribute__((__format__(__printf__, 2, 0)));

void
foo (int i, int j, va_list va)
{
  printf("%2$*1$c", i, j);
  printf("%2$*1$c %2$*1$c", i, j); /* { dg-bogus "too few" "bogus too few dollar" } */
  vbar(va, "%*s"); /* { dg-bogus "too few" "bogus too few vprintf" } */
}
