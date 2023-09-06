#include "analyzer-decls.h"

#define NULL ((void *)0)
typedef __SIZE_TYPE__ size_t;

void test_terminated (void)
{
  __analyzer_eval (__analyzer_get_strlen ("abc") == 3); /* { dg-warning "TRUE" } */
}

void test_unterminated (void)
{
  char buf[3] = "abc";
  __analyzer_get_strlen (buf); /* { dg-warning "stack-based buffer over-read" } */
  /* { dg-message "out-of-bounds read at byte 3 but 'buf' ends at byte 3" "bad read event" { target *-*-* } .-1 } */
  /* { dg-message "while looking for null terminator for argument 1 \\('&buf'\\) of '__analyzer_get_strlen'..." "null terminator event" { target *-*-* } .-2 } */
}

void test_embedded_nuls (void)
{
  /*             0123 456 78.  */
  char buf[9] = "abc\0pq\0xy"; /* unterminated.  */
  __analyzer_eval (__analyzer_get_strlen (buf) == 3); /* { dg-warning "TRUE" } */
  __analyzer_eval (__analyzer_get_strlen (buf + 1) == 2); /* { dg-warning "TRUE" } */
  __analyzer_eval (__analyzer_get_strlen (buf + 2) == 1); /* { dg-warning "TRUE" } */
  __analyzer_eval (__analyzer_get_strlen (buf + 3) == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (__analyzer_get_strlen (buf + 4) == 2); /* { dg-warning "TRUE" } */
  __analyzer_eval (__analyzer_get_strlen (buf + 5) == 1); /* { dg-warning "TRUE" } */
  __analyzer_eval (__analyzer_get_strlen (buf + 6) == 0); /* { dg-warning "TRUE" } */
  __analyzer_get_strlen (buf + 7); /* { dg-warning "stack-based buffer over-read" } */
  /* { dg-message "while looking for null terminator for argument 1 \\('<unknown>'\\) of '__analyzer_get_strlen'..." "event" { target *-*-* } .-1 } */
  // TODO: fix the "<unknown>" here?
}

void test_before_start_of_buffer (void)
{
  const char *buf = "abc";
  __analyzer_get_strlen (buf - 1); /* { dg-warning "buffer under-read" } */
  /* { dg-message "while looking for null terminator for argument 1 \\('<unknown>'\\) of '__analyzer_get_strlen'..." "event" { target *-*-* } .-1 } */
  // TODO: fix the "<unknown>" here?
}

void test_after_end_of_buffer (void)
{
  const char *buf = "abc";
  __analyzer_get_strlen (buf + 4); /* { dg-warning "buffer over-read" } */
  /* { dg-message "while looking for null terminator for argument 1 \\('<unknown>'\\) of '__analyzer_get_strlen'..." "event" { target *-*-* } .-1 } */  
  // TODO: fix the "<unknown>" here?
}

void test_fully_initialized_but_unterminated (void)
{
  char buf[3];
  buf[0] = 'a';
  buf[1] = 'b';
  buf[2] = 'c';
  __analyzer_get_strlen (buf); /* { dg-warning "stack-based buffer over-read" } */
  /* { dg-message "while looking for null terminator for argument 1 \\('&buf'\\) of '__analyzer_get_strlen'..." "event" { target *-*-* } .-1 } */
}

void test_uninitialized (void)
{
  char buf[16];
  __analyzer_get_strlen (buf); /* { dg-warning "use of uninitialized value 'buf\\\[0\\\]'" } */
  /* { dg-message "while looking for null terminator for argument 1 \\('&buf'\\) of '__analyzer_get_strlen'..." "event" { target *-*-* } .-1 } */
}

void test_partially_initialized (void)
{
  char buf[16];
  buf[0] = 'a';
  __analyzer_get_strlen (buf); /* { dg-warning "use of uninitialized value 'buf\\\[1\\\]'" } */
  /* { dg-message "while looking for null terminator for argument 1 \\('&buf'\\) of '__analyzer_get_strlen'..." "event" { target *-*-* } .-1 } */
}

char *test_dynamic_1 (void)
{
  const char *kvstr = "NAME=value";
  size_t len = __builtin_strlen (kvstr);
  char *ptr = __builtin_malloc (len + 1);
  if (!ptr)
    return NULL;
  __builtin_memcpy (ptr, kvstr, len);
  ptr[len] = '\0';
  __analyzer_eval (__analyzer_get_strlen (ptr) == 10); /* { dg-warning "UNKNOWN" } */
  // TODO: should be TRUE
  return ptr;
}

char *test_dynamic_2 (void)
{
  const char *kvstr = "NAME=value";
  size_t len = __builtin_strlen (kvstr);
  char *ptr = __builtin_malloc (len + 1);
  if (!ptr)
    return NULL;
  __builtin_memcpy (ptr, kvstr, len);
  /* Missing termination.  */
  __analyzer_get_strlen (ptr); /* { dg-warning "use of uninitialized value '&buf'" "" { xfail *-*-* } } */
  // TODO (xfail)
  return ptr;
}

char *test_dynamic_3 (const char *src)
{
  size_t len = __builtin_strlen (src);
  char *ptr = __builtin_malloc (len + 1);
  if (!ptr)
    return NULL;
  __builtin_memcpy (ptr, src, len);
  ptr[len] = '\0';
  __analyzer_eval (__analyzer_get_strlen (ptr) == len); /* { dg-warning "UNKNOWN" } */
  // TODO: should get TRUE for this
  return ptr;
}

char *test_dynamic_4 (const char *src)
{
  size_t len = __builtin_strlen (src);
  char *ptr = __builtin_malloc (len + 1);
  if (!ptr)
    return NULL;
  __builtin_memcpy (ptr, src, len);
  /* Missing termination.  */
  __analyzer_get_strlen (ptr); /* { dg-warning "use of uninitialized value 'buf\\\[len\\\]'" "" { xfail *-*-* } } */
  // TODO (xfail)
  return ptr;
}

void test_symbolic_ptr (const char *ptr)
{
  __analyzer_describe (0, __analyzer_get_strlen (ptr)); /* { dg-warning "CONJURED" } */
}

void test_symbolic_offset (size_t idx)
{
  __analyzer_describe (0, __analyzer_get_strlen ("abc" + idx)); /* { dg-warning "CONJURED" } */
}

void test_casts (void)
{
  int i = 42;
  const char *p = (const char *)&i;
  __analyzer_eval (__analyzer_get_strlen (p) == 0); /* { dg-warning "UNKNOWN" } */  
  __analyzer_eval (__analyzer_get_strlen (p + 1) == 0); /* { dg-warning "UNKNOWN" } */  
}

void test_filled_nonzero (void)
{
  char buf[10];
  __builtin_memset (buf, 'a', 10);
  __analyzer_get_strlen (buf); /* { dg-warning "stack-based buffer over-read" "" { xfail *-*-* } } */
}

void test_filled_zero (void)
{
  char buf[10];
  __builtin_memset (buf, 0, 10);
  __analyzer_eval (__analyzer_get_strlen (buf) == 0); /* { dg-warning "TRUE" "correct" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "status quo" { xfail *-*-* } .-1 } */
  __analyzer_eval (__analyzer_get_strlen (buf + 1) == 0); /* { dg-warning "TRUE" "correct" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "status quo" { xfail *-*-* } .-1 } */
}

void test_filled_symbolic (int c)
{
  char buf[10];
  __builtin_memset (buf, c, 10);
  __analyzer_eval (__analyzer_get_strlen (buf) == 0); /* { dg-warning "UNKNOWN" } */  
}
