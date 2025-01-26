/* PR c/118639 */
/* { dg-do compile } */

void
foo (void)
{
  int a = 0, b = 0, c = 0;
#ifndef __cplusplus
  #pragma omp allocate (a, )			/* { dg-error "expected" "" { target c } } */
#endif
  #pragma omp flush (b, )			/* { dg-error "expected" } */
  #pragma omp parallel firstprivate (c, )	/* { dg-error "expected" } */
  ;
}
