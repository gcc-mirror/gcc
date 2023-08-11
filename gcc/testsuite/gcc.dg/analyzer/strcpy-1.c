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
  return strcpy (dst, buf); /* { dg-warning "passing pointer to unterminated string '&buf' as argument 2 of 'strcpy'" } */
}
