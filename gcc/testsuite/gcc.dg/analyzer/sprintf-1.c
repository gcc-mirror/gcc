/* See e.g. https://en.cppreference.com/w/c/io/fprintf
   and https://www.man7.org/linux/man-pages/man3/sprintf.3.html */

/* C only: C++ fpermissive already emits errors. */
#include "analyzer-decls.h"

extern int
sprintf(char* dst, const char* fmt, ...)
  __attribute__((__nothrow__));


#define NULL ((void *)0)

int
test_uninit_fmt_buf (char *dst)
{
  const char fmt[10];
  return sprintf (dst, fmt); /* { dg-warning "use of uninitialized value 'fmt\\\[0\\\]'" } */
  /* { dg-message "while looking for null terminator for argument 2 \\('&fmt'\\) of 'sprintf'..." "event" { target *-*-* } .-1 } */
}

int
test_fmt_not_terminated (char *dst)
{
  const char fmt[3] = "foo";
  return sprintf (dst, fmt); /* { dg-warning "stack-based buffer over-read" } */
  /* { dg-message "while looking for null terminator for argument 2 \\('&fmt'\\) of 'sprintf'..." "event" { target *-*-* } .-1 } */
}
