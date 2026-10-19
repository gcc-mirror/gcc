/* { dg-do compile } */

void
foo (void)
{
#pragma omp parallel if (__has_include ("<stdlib.h>"))	/* { dg-error "used outside of preprocessing directive" } */
  ;
}
