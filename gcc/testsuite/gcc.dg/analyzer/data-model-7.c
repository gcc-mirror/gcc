/* { dg-additional-options "-fno-analyzer-state-merge" } */
#include "analyzer-decls.h"

int test_40 (int flag)
{
  int i;
  if (flag)
    i = 43;
  else
    i = 17;

  /* Without state-merging, we retain the relationship between 'flag' and 'i'.  */
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */

  if (flag)
    __analyzer_eval (i == 43); /* { dg-warning "TRUE" } */
  else
    __analyzer_eval (i == 17); /* { dg-warning "TRUE" } */
}
