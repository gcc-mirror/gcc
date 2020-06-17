#include <stdlib.h>
#include "analyzer-decls.h"

typedef void (*fn_ptr_t) (void *);

void
calls_free (void *victim)
{
  free (victim);
}

void
no_op (void *ptr)
{
}

void test_1 (void *ptr)
{
  fn_ptr_t fn_ptr = calls_free;
  __analyzer_eval (fn_ptr == calls_free); /* { dg-warning "TRUE" } */
  __analyzer_eval (fn_ptr != NULL); /* { dg-warning "TRUE" } */
  __analyzer_eval (fn_ptr == NULL); /* { dg-warning "FALSE" } */
  __analyzer_eval (fn_ptr == no_op); /* { dg-warning "FALSE" } */

  fn_ptr (ptr);
  fn_ptr (ptr);
}
// TODO: issue a double-'free' warning at 2nd call to fn_ptr.

/* As above, but with an extra indirection to try to thwart
   the optimizer.  */

void test_2 (void *ptr, fn_ptr_t *fn_ptr)
{
  *fn_ptr = calls_free;
  __analyzer_eval (*fn_ptr == calls_free); /* { dg-warning "TRUE" } */
  __analyzer_eval (*fn_ptr != NULL); /* { dg-warning "TRUE" } */
  __analyzer_eval (*fn_ptr == NULL); /* { dg-warning "FALSE" } */
  __analyzer_eval (*fn_ptr == no_op); /* { dg-warning "FALSE" } */

  (*fn_ptr) (ptr);
  (*fn_ptr) (ptr);
}
// TODO: issue a double-'free' warning at 2nd call to fn_ptr.
