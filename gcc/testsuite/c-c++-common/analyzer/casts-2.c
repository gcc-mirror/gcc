#include "analyzer-decls.h"

void test_1 (int i)
{
  char c1 = i;
  char c2 = i;
  __analyzer_eval (c1 == i); /* { dg-warning "UNKNOWN" } */  
  __analyzer_eval (c1 == c2); /* { dg-warning "TRUE" } */  
}

void test_2 (char c)
{
  int i = c;
  __analyzer_eval (i == c); /* { dg-warning "TRUE" } */  
}
