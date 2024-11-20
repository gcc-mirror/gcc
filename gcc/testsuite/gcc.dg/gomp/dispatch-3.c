/* { dg-do compile } */

/* Check that the assignment from pointer to integer does not trigger an ICE but 
   is rejected as invalid code.  */

long long *f();
int *variant_fn();

#pragma omp declare variant(variant_fn) match(construct={dispatch})
int *bar();

void sub()
{
  #pragma omp dispatch
   *f() = bar(); /* { dg-error "assignment to 'long long int' from 'int \\*' makes integer from pointer without a cast" } */
}
