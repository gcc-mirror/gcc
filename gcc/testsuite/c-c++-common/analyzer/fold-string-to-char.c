#include "../../gcc.dg/analyzer/analyzer-decls.h"

void test_1 (void)
{
  char str[] = "Hello";
  char *ptr = str;
  __analyzer_eval (ptr[0] == 'H'); /* { dg-warning "TRUE" } */
}
