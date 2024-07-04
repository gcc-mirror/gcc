#include "../../gcc.dg/analyzer/analyzer-decls.h"

/* { dg-additional-options "-fpermissive" { target c++ } } */

/* Example with multiple params with attribute null_terminated_string_arg.  */

char *example_fn (const char *p, const char *q)
  __attribute__((null_terminated_string_arg (1)))
  __attribute__((null_terminated_string_arg (2)));
// but can be NULL

char *
test_passthrough (const char *a, const char *b)
{
  return example_fn (a, b);
}

char *
test_NULL_str (void)
{
  return example_fn (NULL, NULL); /* { dg-bogus "use of NULL where non-null expected" } */
}

char *
test_unterminated_str_1 (void)
{
  char str[3] = "abc"; /* { dg-warning "initializer-string for '\[^\n\]*' is too long" "" { target c++ } } */
  return example_fn (str, NULL); /* { dg-warning "stack-based buffer over-read" } */
  /* { dg-message "while looking for null terminator for argument 1" "note" { target *-*-* } .-1 } */
}

char *
test_unterminated_str_2 (void)
{
  char str[3] = "abc"; /* { dg-warning "initializer-string for '\[^\n\]*' is too long" "" { target c++ } } */
  return example_fn (NULL, str); /* { dg-warning "stack-based buffer over-read" } */
  /* { dg-message "while looking for null terminator for argument 2" "note" { target *-*-* } .-1 } */
}

char *
test_uninitialized_str_1 (void)
{
  char str[16];
  return example_fn (str, NULL); /* { dg-warning "use of uninitialized value 'str\\\[0\\\]'" } */
}

char *
test_uninitialized_str_2 (void)
{
  char str[16];
  return example_fn (NULL, str); /* { dg-warning "use of uninitialized value 'str\\\[0\\\]'" } */
}
