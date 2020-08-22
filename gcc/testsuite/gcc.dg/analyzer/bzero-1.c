#include "analyzer-decls.h"

extern void bzero(void *s, __SIZE_TYPE__ n);

void test_1 (void)
{
  char tmp[1024];
  bzero (tmp, 1024);
  __analyzer_eval (tmp[0] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (tmp[1023] == 0); /* { dg-warning "TRUE" } */
}
