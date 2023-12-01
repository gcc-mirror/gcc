/* PR c/35738 */
/* { dg-do compile } */
/* { dg-options "-fpermissive -fopenmp" } */

void foo (void);

void
bar (void *p)
{
  int i = 0;
  char q[10];
#pragma omp atomic
  i += q;		/* { dg-warning "makes integer from pointer without a cast" } */
#pragma omp atomic
  i += foo;		/* { dg-warning "makes integer from pointer without a cast" } */
#pragma omp atomic
  i += p;		/* { dg-warning "makes integer from pointer without a cast" } */
}
