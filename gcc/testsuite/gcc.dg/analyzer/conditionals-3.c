/* { dg-additional-options "-fno-analyzer-state-merge" } */

#include "analyzer-decls.h"

static void only_called_when_flag_a_true (int i)
{
  __analyzer_eval (i == 42); /* { dg-warning "TRUE" } */
}

static void only_called_when_flag_b_true (int i)
{
  __analyzer_eval (i == 17); /* { dg-warning "TRUE" } */
}

int test_1 (int flag_a, int flag_b)
{
  int i = 17;

  __analyzer_eval (flag_a); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (flag_b); /* { dg-warning "UNKNOWN" } */

  if (flag_a)
    {
      __analyzer_eval (flag_a); /* { dg-warning "TRUE" } */
      __analyzer_eval (flag_b); /* { dg-warning "UNKNOWN" } */
      i = 42;
    }

  __analyzer_eval (flag_b); /* { dg-warning "UNKNOWN" } */

  if (flag_a)
    {
      __analyzer_eval (flag_a); /* { dg-warning "TRUE" } */
      __analyzer_eval (flag_b); /* { dg-warning "UNKNOWN" } */
      __analyzer_eval (i == 42); /* { dg-warning "TRUE" } */
      __analyzer_eval (i == 17); /* { dg-warning "FALSE" } */
      only_called_when_flag_a_true (i);
    }  
  else
    {
      __analyzer_eval (flag_a); /* { dg-warning "FALSE" } */
      __analyzer_eval (flag_b); /* { dg-warning "UNKNOWN" } */
      __analyzer_eval (i == 42); /* { dg-warning "FALSE" } */
      __analyzer_eval (i == 17); /* { dg-warning "TRUE" } */
      only_called_when_flag_b_true (i);
    }
}
