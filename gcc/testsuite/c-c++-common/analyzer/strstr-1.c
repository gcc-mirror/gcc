/* See e.g. https://en.cppreference.com/w/c/string/byte/strstr  */

/* { dg-additional-options "-fpermissive" { target c++ } } */

#include "../../gcc.dg/analyzer/analyzer-decls.h"

extern char *strstr (const char* str, const char* substr);

char *
test_passthrough (const char* str, const char* substr)
{
  return strstr (str, substr);
}

char *
test_NULL_str (const char *substr)
{
  return strstr (NULL, substr); /* { dg-warning "use of NULL where non-null expected" } */
}

char *
test_unterminated_str (const char *substr)
{
  char str[3] = "abc"; /* { dg-warning "initializer-string for '\[^\n\]*' is too long" "" { target c++ } } */
  return strstr (str, substr); /* { dg-warning "stack-based buffer over-read" } */
  /* { dg-message "while looking for null terminator for argument 1" "note" { target *-*-* } .-1 } */
}

char *
test_uninitialized_str (const char *substr)
{
  char str[16];
  return strstr (str, substr); /* { dg-warning "use of uninitialized value 'str\\\[0\\\]'" } */
}

char *
test_NULL_substr (const char *str)
{
  return strstr (str, NULL); /* { dg-warning "use of NULL where non-null expected" } */
}

char *
test_unterminated_substr (const char *str)
{
  char substr[3] = "abc"; /* { dg-warning "initializer-string for '\[^\n\]*' is too long" "" { target c++ } } */
  return strstr (str, substr); /* { dg-warning "stack-based buffer over-read" } */
  /* { dg-message "while looking for null terminator for argument 2" "note" { target *-*-* } .-1 } */
}

char *test_uninitialized_substr (const char *str)
{
  char substr[16];
  return strstr (str, substr); /* { dg-warning "use of uninitialized value 'substr\\\[0\\\]'" } */
}
