/* { dg-additional-options "-fanalyzer-call-summaries" } */

#include <errno.h>
#include "analyzer-decls.h"

void sets_errno (int x)
{
  errno = x;
}

void test_sets_errno (int y)
{
  sets_errno (y);
  sets_errno (y);

  __analyzer_eval (errno == y); /* { dg-warning "TRUE" } */  
}
