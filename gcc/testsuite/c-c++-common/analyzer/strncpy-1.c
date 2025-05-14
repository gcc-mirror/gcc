/* See e.g. https://en.cppreference.com/w/c/string/byte/strncpy  */

/* { dg-additional-options "-Wno-stringop-overflow" } */
/* { dg-additional-options "-fpermissive" { target c++ } } */

#include "../../gcc.dg/analyzer/analyzer-decls.h"

typedef __SIZE_TYPE__ size_t;

extern char *strncpy (char *dst, const char *src, size_t count);

char *
test_passthrough (char *dst, const char *src, size_t count)
{
  char *result = strncpy (dst, src, count);
  __analyzer_eval (result == dst); /* { dg-warning "TRUE" } */
  return result;
}

char *
test_null_dst (const char *src, size_t count)
{
  return strncpy (NULL, src, 42); /* { dg-warning "use of NULL where non-null expected" } */
}

char *
test_null_src (char *dst, size_t count)
{
  return strncpy (dst, NULL, 42); /* { dg-warning "use of NULL where non-null expected" } */
}

void
test_zero_fill (char *dst)
{
  strncpy (dst, "", 5);
  __analyzer_eval (dst[0] == '\0'); /* { dg-warning "TRUE" "correct" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "status quo" { xfail *-*-* } .-1 } */
  __analyzer_eval (dst[1] == '\0'); /* { dg-warning "TRUE" "correct" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "status quo" { xfail *-*-* } .-1 } */
  __analyzer_eval (dst[2] == '\0'); /* { dg-warning "TRUE" "correct" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "status quo" { xfail *-*-* } .-1 } */
  __analyzer_eval (dst[3] == '\0'); /* { dg-warning "TRUE" "correct" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "status quo" { xfail *-*-* } .-1 } */
  __analyzer_eval (dst[4] == '\0'); /* { dg-warning "TRUE" "correct" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "status quo" { xfail *-*-* } .-1 } */
  __analyzer_eval (__analyzer_get_strlen (dst) == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (__analyzer_get_strlen (dst + 1) == 0); /* { dg-warning "TRUE" } */
}

char *test_unterminated_concrete_a (char *dst)
{
  char buf[3] = "abc"; /* { dg-warning "initializer-string for '\[^\n\]*' is too long" "" { target c++ } } */
  /* Should be OK to copy nothing.  */
  return strncpy (dst, buf, 0); /* { dg-bogus "" } */
}

char *test_unterminated_concrete_b (char *dst)
{
  char buf[3] = "abc"; /* { dg-warning "initializer-string for '\[^\n\]*' is too long" "" { target c++ } } */
  /* Should be OK as the count limits the accesses to valid
     locations within src buf.  */
  return strncpy (dst, buf, 3); /* { dg-bogus "" } */
}

char *test_unterminated_concrete_c (char *dst)
{
  char buf[3] = "abc"; /* { dg-warning "initializer-string for '\[^\n\]*' is too long" "" { target c++ } } */
  /* Should warn: the count is one too high to limit the accesses
     to within src buf.  */
  return strncpy (dst, buf, 4); /* { dg-warning "stack-based buffer over-read" } */
}

char *test_terminated_concrete_d (char *dst)
{
  char buf[6];
  __builtin_memset (buf, 'a', 3);
  __builtin_memset (buf + 3, 'b', 3);

  /* Shouldn't warn.  */
  return strncpy (dst, buf, 6); /* { dg-bogus "" } */
}

char *test_unterminated_concrete_e (char *dst)
{
  char buf[6];
  __builtin_memset (buf, 'a', 3);
  __builtin_memset (buf + 3, 'b', 3);

  /* Should warn.  */
  return strncpy (dst, buf, 7); /* { dg-warning "stack-based buffer over-read" } */
}

char *test_unterminated_symbolic (char *dst, size_t count)
{
  char buf[3] = "abc"; /* { dg-warning "initializer-string for '\[^\n\]*' is too long" "" { target c++ } } */
  return strncpy (dst, buf, count);
}

char *test_terminated_symbolic (char *dst, size_t count)
{
  const char *src = "abc";
  return strncpy (dst, src, count); /* { dg-bogus "" } */
}

char *test_uninitialized_concrete_a (char *dst)
{
  char buf[16];
  return strncpy (dst, buf, 0); /* { dg-bogus "" } */
}

char *test_uninitialized_concrete_b (char *dst)
{
  char buf[16];
  return strncpy (dst, buf, 1); /* { dg-warning "use of uninitialized value" } */
}

char *test_initialized_concrete_c (char *dst)
{
  char buf[16];
  buf[0] = 'a';
  return strncpy (dst, buf, 1);  /* { dg-bogus "" } */
}

char *test_uninitialized_symbolic (char *dst, size_t count)
{
  char buf[16];
  return strncpy (dst, buf, count); /* { dg-warning "use of uninitialized value" } */
}

void test_truncation_1 (const char *src)
{
  char buf[16];
  strncpy (buf, src, 16);
  /* buf might not be terminated (when strlen(src) > 16).  */
  __analyzer_get_strlen (buf); /* { dg-warning "stack-based buffer over-read" "" { xfail *-*-* } } */
}

void test_truncation_2 (size_t count)
{
  char buf[16];
  strncpy (buf, "abc", count);
  /* buf might not be terminated (when count <= 3).  */
  __analyzer_get_strlen (buf); /* { dg-warning "stack-based buffer over-read" "" { xfail *-*-* } } */
}

void test_too_big_concrete (void)
{
  char buf[10];
  strncpy (buf, "abc", 128); /* { dg-warning "stack-based buffer overflow" } */
}

void test_too_big_symbolic (const char *src)
{
  char buf[10];
  strncpy (buf, src, 128); /* { dg-warning "stack-based buffer overflow" } */
}
