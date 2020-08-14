#include "analyzer-decls.h"

struct base
{
  int i;
};

struct sub
{
  struct base b;
  int j;
};

void test (void)
{
  struct sub s;
  s.b.i = 3;
  s.j = 4;
  __analyzer_eval (s.b.i == 3); /* { dg-warning "TRUE" } */
  __analyzer_eval (s.j == 4); /* { dg-warning "TRUE" } */

  struct base *bp = (struct base *)&s;

  __analyzer_eval (bp->i == 3); /* { dg-warning "TRUE" } */
}
