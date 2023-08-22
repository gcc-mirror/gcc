#include <string.h>
#include "analyzer-decls.h"

char *
test_1 (char *dst, char *src)
{
  char *result = strcpy (dst, src);
  __analyzer_eval (result == dst); /* { dg-warning "TRUE" } */
  return result;
}

char *
test_1a (char *dst, char *src)
{
  char *result = __strcpy_chk (dst, src, -1);
  __analyzer_eval (result == dst); /* { dg-warning "TRUE" } */
  return result;
}

char *test_unterminated (char *dst)
{
  char buf[3] = "abc";
  return strcpy (dst, buf); /* { dg-warning "stack-based buffer over-read" } */
  /* { dg-message "while looking for null terminator for argument 2 \\('&buf'\\) of 'strcpy'..." "event" { target *-*-* } .-1 } */
}

char *test_uninitialized (char *dst)
{
  char buf[16];
  return strcpy (dst, buf); /* { dg-warning "use of uninitialized value 'buf\\\[0\\\]'" } */
  /* { dg-message "while looking for null terminator for argument 2 \\('&buf'\\) of 'strcpy'..." "event" { target *-*-* } .-1 } */
}
