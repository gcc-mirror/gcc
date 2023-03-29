/* { dg-additional-options "-fanalyzer-call-summaries --param analyzer-min-snodes-for-call-summary=0 -fno-analyzer-state-merge" } */

/* There need to be at least two calls to a function for the
   call-summarization code to be used.
   TODO: add some kind of test that summarization *was* used.  */

#include "analyzer-decls.h"

/* With state merging disabled, we get two summaries here.  */

int two_outcomes (int flag, int x, int y)
{
  if (flag)
    return x;
  else
    return y;
}

void test_two_outcomes (int outer_flag, int a, int b)
{
  int r;
  __analyzer_eval (two_outcomes (1, a, b) == a); /* { dg-warning "TRUE" } */
  __analyzer_eval (two_outcomes (0, a, b) == b); /* { dg-warning "TRUE" } */
  r = two_outcomes (outer_flag, a, b);
  if (outer_flag)
    __analyzer_eval (r == a); /* { dg-warning "TRUE" } */
  else
    __analyzer_eval (r == b); /* { dg-warning "TRUE" } */  
}
