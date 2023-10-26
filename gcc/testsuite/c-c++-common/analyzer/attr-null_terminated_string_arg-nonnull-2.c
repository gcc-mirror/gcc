/* { dg-additional-options "-fpermissive" { target c++ } } */

#include "../../gcc.dg/analyzer/analyzer-decls.h"

extern char *example_fn (const char *p) /* { dg-message "argument 1 of '\[^\n\r\]*' must be a pointer to a null-terminated string" } */
  __attribute__((null_terminated_string_arg (1), nonnull (1)));

char *
test_passthrough (const char* str)
{
  return example_fn (str);
}

char *
test_NULL_str (void)
{
  return example_fn (NULL); /* { dg-warning "use of NULL where non-null expected" } */
}

char *
test_unterminated_str (void)
{
  char str[3] = "abc"; /* { dg-warning "initializer-string for '\[^\n\]*' is too long" "" { target c++ } } */
  return example_fn (str); /* { dg-warning "stack-based buffer over-read" } */
  /* { dg-message "while looking for null terminator for argument 1" "note" { target *-*-* } .-1 } */
}

char *
test_uninitialized_str (void)
{
  char str[16];
  return example_fn (str); /* { dg-warning "use of uninitialized value 'str\\\[0\\\]'" } */
}
