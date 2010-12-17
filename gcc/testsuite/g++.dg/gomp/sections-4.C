/* PR c++/24613 */
/* { dg-do compile } */

#pragma omp section	/* { dg-error "may only be used in" } */

int i;

void
foo (void)
{
  #pragma omp section	/* { dg-error "may only be used in" } */
    i++;
}
