#include "../../gcc.dg/analyzer/analyzer-decls.h"

char buf[16];

int main (void)
{
  /* We should be able to assume that "buf" is all zeroes here.  */

  __analyzer_eval (__analyzer_get_strlen (buf) == 0); /* { dg-warning "TRUE" } */

  buf[0] = 'a';
  __analyzer_eval (__analyzer_get_strlen (buf) == 1);  /* { dg-warning "TRUE" } */

  return 0;
}
