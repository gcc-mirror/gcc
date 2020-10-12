#include "analyzer-decls.h"

int a;
void test (int *p, int x)
{
  int y;

  a = 17;
  x = 42;
  y = 13;

  __analyzer_eval (a == 17); /* { dg-warning "TRUE" } */
  __analyzer_eval (x == 42); /* { dg-warning "TRUE" } */
  __analyzer_eval (y == 13); /* { dg-warning "TRUE" } */

  __analyzer_eval (p == &a); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (p == &x); /* { dg-warning "FALSE" } */
  __analyzer_eval (p == &y); /* { dg-warning "FALSE" } */
  
  *p = 73;

  __analyzer_eval (a == 17); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (x == 42); /* { dg-warning "TRUE" } */
  __analyzer_eval (y == 13); /* { dg-warning "TRUE" } */
}
