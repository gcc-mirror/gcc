/* See e.g. https://en.cppreference.com/w/c/string/byte/strlen */

#include "analyzer-decls.h"

typedef __SIZE_TYPE__ size_t;

static size_t __attribute__((noinline))
call_strlen_1 (const char *p)
{
  return __builtin_strlen (p);
}

void test_string (void)
{
  __analyzer_eval (call_strlen_1 ("abc") == 3); /* { dg-warning "TRUE" } */
}

static size_t __attribute__((noinline))
call_strlen_2 (const char *p)
{
  return __builtin_strlen (p); /* { dg-warning "stack-based buffer over-read" } */
}

void test_unterminated (void)
{
  const char buf[3] = "abc";
  __analyzer_eval (call_strlen_2 (buf) == 3); /* { dg-warning "UNKNOWN" } */
}

void test_uninitialized (void)
{
  char buf[16];
  __builtin_strlen (buf); /* { dg-warning "use of uninitialized value 'buf\\\[0\\\]'" } */
}

void test_partially_initialized (void)
{
  char buf[16];
  buf[0] = 'a';
  __builtin_strlen (buf); /* { dg-warning "use of uninitialized value 'buf\\\[1\\\]'" } */
}

extern size_t strlen (const char *str);

size_t
test_passthrough (const char *str)
{
  return strlen (str);
}

/* TODO
   - complain if NULL str
   - should it be tainted if str's bytes are tainted?
*/
