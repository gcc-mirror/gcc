/* { dg-do compile } */

/* Check that the parser does not issue an error when a variant returns a 
   reference. */

int& variant_fn();

#pragma omp declare variant(variant_fn) match(construct={target})
int& bar();

void sub(int a)
{
  #pragma omp dispatch
    bar();
  #pragma omp dispatch
    a = bar();
}
