/* See e.g. https://en.cppreference.com/w/c/io/fprintf
   and https://www.man7.org/linux/man-pages/man3/sprintf.3.html */

extern int
sprintf(char* dst, const char* fmt, ...)
  __attribute__((__nothrow__));

#include "../../gcc.dg/analyzer/analyzer-decls.h"

void test_text_ok (void)
{
  char buf[16];
  sprintf (buf, "hello world");
}

void test_text_oob (void)
{
  char buf[3];
  sprintf (buf, "hello world"); /* { dg-warning "out-of-bounds" "PR analyzer/107017" { xfail *-*-* } } */
}

void test_percent_s_ok (void)
{
  char buf[16];
  sprintf (buf, "%s", "foo");
}

void test_percent_s_oob (void)
{
  char buf[3];
  sprintf (buf, "%s", "foo"); /* { dg-warning "out-of-bounds" "PR analyzer/107017" { xfail *-*-* } } */
}

void test_percent_i_ok (void)
{
  char buf[16];
  sprintf (buf, "%i", "42");
}

void test_percent_i_oob (void)
{
  char buf[4];
  sprintf (buf, "%i", "1066"); /* { dg-warning "out-of-bounds" "PR analyzer/107017" { xfail *-*-* } } */
}
