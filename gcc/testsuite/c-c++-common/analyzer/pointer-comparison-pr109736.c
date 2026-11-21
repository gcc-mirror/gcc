#include "analyzer-decls.h"

void c()
{
  int d[2] = {42,43};
  int *e = d;

  __analyzer_eval(e == d + 1); /* { dg-warning "FALSE" } */
  __analyzer_eval(e + 1 == d + 1); /* { dg-warning "TRUE" } */
}
