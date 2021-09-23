/* { dg-do compile } */
/* { dg-options "-Wall -std=c17 -fopenmp -pedantic-errors" } */

void f1(void)
{
  #pragma omp barrier a		/* { dg-error "expected end of line" } */
}

/* OpenMP 2.5, section 2.7.3:

   Note that because the barrier construct does not have a C language
   statement as part of its syntax, there are some restrictions on its
   placement within a program. The barrier directive may only be placed
   in the program at a position where ignoring or deleting the directive
   would result in a program with correct syntax.  */

void f2(void)
{
  label:			/* { dg-warning "defined but not used" } */
    #pragma omp barrier		/* { dg-error "may only be used in compound statements" } */
}

void f3(_Bool p)
{
  if (p)
    #pragma omp barrier		/* { dg-error "compound statements" } */
}
