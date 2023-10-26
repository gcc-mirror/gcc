/* { dg-additional-options "-Wno-stringop-overflow" } */
/* { dg-additional-options "-fpermissive" { target c++ } } */

#include "../../gcc.dg/analyzer/analyzer-decls.h"

char *example_fn (char *p, __SIZE_TYPE__ n)
  __attribute__((null_terminated_string_arg (1)))
  __attribute__((access (read_write, 1, 2)));

char *
test_unterminated_str (void)
{
  char str[3] = "abc"; /* { dg-warning "initializer-string for '\[^\n\]*' is too long" "" { target c++ } } */
  return example_fn (str, 4); /* { dg-warning "stack-based buffer over-read" } */
}
