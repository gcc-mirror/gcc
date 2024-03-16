/* { dg-additional-options "-fpermissive" { target c++ } } */

#include "../../gcc.dg/analyzer/analyzer-decls.h"

typedef __SIZE_TYPE__ size_t;

extern char *strtok (char *str, const char *delim)
  __attribute__((nonnull (2)));

char *
test_passthrough (char *str, const char *delim)
{
  return strtok (str, delim);
}

char *
test_null_str (const char *delim)
{
  return strtok (NULL, delim);
}

char *
test_null_delim (char *str)
{
  return strtok (str, NULL); /* { dg-warning "use of NULL where non-null expected" } */
  /* This is from the attribute.  */
}

char *
test_write_to_literal (void)
{
  const char *str = "hello world";
  return strtok ((char *)str, " "); /* { dg-warning "write to string literal" } */
}

char *
test_unterminated_str (const char *delim)
{
  char str[3] = "abc"; /* { dg-warning "initializer-string for '\[^\n\]*' is too long" "" { target c++ } } */
  return strtok (str, delim); /* { dg-warning "stack-based buffer over-read" } */
}

char *
test_unterminated_delimstr (char *str)
{
  char delim[3] = "abc"; /* { dg-warning "initializer-string for '\[^\n\]*' is too long" "" { target c++ } } */
  return strtok (str, delim); /* { dg-warning "stack-based buffer over-read" } */
  /* { dg-message "while looking for null terminator for argument 2" "" { target *-*-* } .-1 } */
}

size_t
test_use_after_free_via_null_2 (char *p)
{
  strtok (p, " ");
  __builtin_free (p);

  char *q = strtok (NULL, " "); /* TODO: should complain about this.  */
  if (q)
    return __builtin_strlen (q); /* TODO: should complain about this.  */
  else
    return 0;
}
