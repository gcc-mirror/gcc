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

static size_t __attribute__((noinline))
call_strlen_3 (const char *p)
{
  return __builtin_strlen (p);
}

void test_array_initialization_from_shorter_literal (void)
{
  const char buf[10] = "abc";
  __analyzer_eval (call_strlen_3 (buf) == 3); /* { dg-warning "TRUE" } */
  __analyzer_eval (call_strlen_3 (buf + 5) == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[5] == 0); /* { dg-warning "TRUE" } */
}

static size_t __attribute__((noinline))
call_strlen_4 (const char *p)
{
  return __builtin_strlen (p); /* { dg-warning "stack-based buffer over-read" } */
}

char test_array_initialization_from_longer_literal (void)
{
  const char buf[3] = "abcdefg"; /* { dg-warning "initializer-string for array of 'char' is too long" } */
  __analyzer_eval (call_strlen_4 (buf) == 3); /* { dg-warning "UNKNOWN" } */
  return buf[5]; /* { dg-warning "stack-based buffer over-read" } */
}

static size_t __attribute__((noinline))
call_strlen_5 (const char *p)
{
  return __builtin_strlen (p);
}

static size_t __attribute__((noinline))
call_strlen_5a (const char *p)
{
  return __builtin_strlen (p); /* { dg-warning "stack-based buffer over-read" } */
}

char test_array_initialization_implicit_length (void)
{
  const char buf[] = "abc";
  __analyzer_eval (call_strlen_5 (buf) == 3); /* { dg-warning "TRUE" } */
  __analyzer_eval (call_strlen_5 (buf + 2) == 1); /* { dg-warning "TRUE" } */
  __analyzer_eval (call_strlen_5 (buf + 3) == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[0] == 'a'); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[3] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (call_strlen_5a (buf + 4) == 0); /* { dg-warning "UNKNOWN" } */
  return buf[4]; /* { dg-warning "stack-based buffer over-read" } */
}

