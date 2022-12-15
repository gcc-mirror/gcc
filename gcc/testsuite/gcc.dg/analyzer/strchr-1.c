#include <string.h>
#include "analyzer-decls.h"

const char* test_literal (int x)
{
  char *p = __builtin_strchr ("123", x); /* { dg-message "when '__builtin_strchr' returns non-NULL" } */
  if (p)
    {
      __analyzer_eval (*p == x); /* { dg-message "UNKNOWN" } */
      /* TODO: this ought to be TRUE, but it's unclear that it's
	 worth stashing this constraint.  */
      *p = 'A'; /* { dg-warning "write to string literal" } */
    }
  return p;
}

void test_2 (const char *s, int c)
{
  char *p = __builtin_strchr (s, c); /* { dg-message "when '__builtin_strchr' returns NULL"} */
  *p = 'A'; /* { dg-warning "dereference of NULL 'p'" "null deref" } */
}

void test_3 (const char *s, int c)
{
  char *p = strchr (s, c); /* { dg-message "when 'strchr' returns NULL"} */
  *p = 'A'; /* { dg-warning "dereference of NULL 'p'" "null deref" } */
}
