#include "analyzer-decls.h"

_Atomic int i;

void test_atomic_int_1(int x)
{
  i = x;
  __analyzer_eval(i == x); /* { dg-warning "TRUE" } */
  i++;
  __analyzer_eval(i == x + 1); /* { dg-warning "TRUE" } */  
}
