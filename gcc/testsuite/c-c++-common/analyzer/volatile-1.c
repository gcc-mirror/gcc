#include "../../gcc.dg/analyzer/analyzer-decls.h"

volatile int g;

void test_global (void)
{
  int v1 = g;
  int v2 = g;
  __analyzer_eval (v1 == v2); /* { dg-warning "UNKNOWN" } */
}

void test_local (void)
{
  volatile int x = 0;
  int v1 = x;
  int v2 = x;
  __analyzer_eval (v1 == v2); /* { dg-warning "UNKNOWN" } */
}
