#include "analyzer-decls.h"

void foo(int width) {
  int i = 0;
  int base;
  if (width > 0){
    __analyzer_eval(i == 0); /* { dg-warning "TRUE" } */
    __analyzer_eval(width > 0); /* { dg-warning "TRUE" } */
    __analyzer_eval(width - i > 0); /* { dg-warning "TRUE" } */
    __analyzer_eval(i - width <= 0); /* { dg-warning "TRUE" } */
    if (i - width <= 0) {
      base = 512;
    }
    else {
      __analyzer_dump_path (); /* { dg-bogus "path" } */
    }
    base+=1; /* { dg-bogus "uninit" } */
  }
}

void test_ge_zero (int x)
{
  if (x >= 0)
    {
      __analyzer_eval(x >= 0); /* { dg-warning "TRUE" } */
      __analyzer_eval(x > 0); /* { dg-warning "UNKNOWN" } */
      __analyzer_eval(x <= 0); /* { dg-warning "UNKNOWN" } */
      __analyzer_eval(x < 0); /* { dg-warning "FALSE" } */
      __analyzer_eval(-x <= 0); /* { dg-warning "TRUE" } */
      __analyzer_eval(-x < 0); /* { dg-warning "UNKNOWN" } */
      __analyzer_eval(-x >= 0); /* { dg-warning "UNKNOWN" } */
      __analyzer_eval(-x > 0); /* { dg-warning "FALSE" } */
    }
}

void test_gt_zero (int x)
{
  if (x > 0)
    {
      __analyzer_eval(x >= 0); /* { dg-warning "TRUE" } */
      __analyzer_eval(x > 0); /* { dg-warning "TRUE" } */
      __analyzer_eval(x <= 0); /* { dg-warning "FALSE" } */
      __analyzer_eval(x < 0); /* { dg-warning "FALSE" } */
      __analyzer_eval(-x <= 0); /* { dg-warning "TRUE" } */
      __analyzer_eval(-x < 0); /* { dg-warning "TRUE" } */
      __analyzer_eval(-x >= 0); /* { dg-warning "FALSE" } */
      __analyzer_eval(-x > 0); /* { dg-warning "FALSE" } */
    }
}
