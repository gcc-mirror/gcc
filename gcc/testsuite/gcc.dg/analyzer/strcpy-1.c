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

extern void external_fn (void *ptr);

char *test_external_fn (void)
{
  char src[10];
  char dst[10];
  external_fn (src);
  strcpy (dst, src);
  __analyzer_eval (strlen (dst) == strlen (src)); /* { dg-warning "UNKNOWN" } */
  // TODO: ideally would be TRUE  
}

void test_sprintf_strcpy (const char *a, const char *b)
{
  char buf_1[10];
  char buf_2[10];
  __builtin_sprintf (buf_1, "%s/%s", a, b);
  strcpy (buf_2, buf_1);
  __analyzer_eval (strlen (buf_1) == strlen (buf_2)); /* { dg-warning "UNKNOWN" } */
  // TODO: ideally would be TRUE  
}
