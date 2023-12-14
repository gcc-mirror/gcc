#include "analyzer-decls.h"

void test (int p, int q, int r)
{
  if (p == 42)
    {
      __analyzer_eval (p == 42);  /* { dg-warning "TRUE" } */
      __analyzer_eval (p != 42);  /* { dg-warning "FALSE" } */
      if (q == 42)
	{
	  __analyzer_eval (p == q);  /* { dg-warning "TRUE" } */
	}
      else
	{
	  __analyzer_eval (p != q);  /* { dg-warning "TRUE" } */
	}
    }
  else
    {
      __analyzer_eval (p == 42);  /* { dg-warning "FALSE" } */
      __analyzer_eval (p != 42);  /* { dg-warning "TRUE" } */
      if (q == 42)
	{
	  __analyzer_eval (p == q);  /* { dg-warning "FALSE" } */
	}
      else
	{
	  __analyzer_eval (p == q);  /* { dg-warning "UNKNOWN" } */
	}
    }
}
