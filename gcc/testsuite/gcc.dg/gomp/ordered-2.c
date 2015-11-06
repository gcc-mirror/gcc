/* { dg-do compile } */

void f1(void)
{
  #pragma omp ordered asdf	/* { dg-error "expected" } */
  #pragma omp ordered		/* { dg-error "region may not be closely nested inside of" } */
}				/* { dg-error "expected expression" } */
