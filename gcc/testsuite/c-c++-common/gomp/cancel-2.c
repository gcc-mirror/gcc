/* { dg-do compile } */

void
foo (void)
{
  #pragma omp parallel
  {
    #pragma omp cancel parallel if (1) if (1)			/* { dg-error "too many 'if' clauses without modifier" } */
  }
}
