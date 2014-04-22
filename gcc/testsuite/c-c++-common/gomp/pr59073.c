/* PR c/59073 */
/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

void
foo ()
{
  int i; 
#pragma omp distribute parallel for
  for (i = 0; i < 10; i)	/* { dg-error "invalid increment expression" } */
    ;
}
