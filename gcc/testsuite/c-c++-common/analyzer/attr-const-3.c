/* Verify that we handle unknown values passed to  __attribute__ ((const))
   (by imposing a complexity limit).  */

/* { dg-additional-options "--param analyzer-max-svalue-depth=4 -Wno-analyzer-symbol-too-complex" } */

#include "analyzer-decls.h"

extern int const_fn_1 (int) __attribute__ ((const));

void test_const_fn_1 (int x, int y)
{
  int x_1 = const_fn_1 (x);
  int x_2 = const_fn_1 (x);
  int y_1 = const_fn_1 (y);
  int y_2 = const_fn_1 (y);
  __analyzer_eval (x_1 == x_2); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (y_1 == y_2); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (x_1 == y_1); /* { dg-warning "UNKNOWN" } */
}

void test_2 (int x)
{
  int once = const_fn_1 (x);
  int again = const_fn_1 (once);
  __analyzer_eval (once == again); /* { dg-warning "UNKNOWN" } */  
}
