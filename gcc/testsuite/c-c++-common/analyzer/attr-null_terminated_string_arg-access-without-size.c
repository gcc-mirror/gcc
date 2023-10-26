/* { dg-additional-options "-Wno-stringop-overread" } */
/* { dg-additional-options "-fpermissive" { target c++ } } */

#include "../../gcc.dg/analyzer/analyzer-decls.h"

char *example_fn (const char *p, __SIZE_TYPE__ n) /* { dg-message "argument 1 of '\[^\n\r\]*' must be a pointer to a null-terminated string" } */
  __attribute__((null_terminated_string_arg (1)))
  __attribute__((access (read_only, 1))); // but doesn't identify an argument for a size limit

char *
test_passthrough (const char* str, __SIZE_TYPE__ n)
{
  return example_fn (str, n);
}

char *
test_NULL_str_a (void)
{
  return example_fn (NULL, 0); /* { dg-bogus "use of NULL where non-null expected" } */
}

char *
test_NULL_str_b (void)
{
  return example_fn (NULL, 4); /* { dg-bogus "use of NULL where non-null expected" } */
}

char *
test_unterminated_str (void)
{
  char str[3] = "abc"; /* { dg-warning "initializer-string for '\[^\n\]*' is too long" "" { target c++ } } */
  return example_fn (str, 4); /* { dg-warning "stack-based buffer over-read" } */
}

char *
test_uninitialized_str_a (void)
{
  char str[16];
  return example_fn (str, 1); /* { dg-warning "use of uninitialized value 'str\\\[0\\\]'" } */
}

char *
test_uninitialized_str_b (void)
{
  char str[16];
  return example_fn (str, 16); /* { dg-warning "use of uninitialized value 'str\\\[0\\\]'" } */
}

char *
test_uninitialized_str_c (void)
{
  char str[16];
  return example_fn (str, 17); /* { dg-warning "use of uninitialized value 'str\\\[0\\\]'" } */
}
