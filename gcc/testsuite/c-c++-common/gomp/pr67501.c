/* PR c/67501 */
/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

void
foo (void)
{
  int i, j;
  #pragma omp for simd copyprivate(j	/* { dg-error "before end of line" } */
  for (i = 0; i < 16; ++i)		/* { dg-error "is not valid for" "" { target *-*-* } .-1 } */
    ;
}
