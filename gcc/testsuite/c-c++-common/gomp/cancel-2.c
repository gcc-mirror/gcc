/* { dg-do compile } */

void
foo (void)
{
  #pragma omp parallel
  {
    #pragma omp cancel parallel if (1) if (1)			/* { dg-error "too many 'if' clauses without modifier" } */
    #pragma omp cancel parallel if (cancel: 1) if (cancel: 1)	/* { dg-error "too many 'if' clauses with 'cancel' modifier" } */
    #pragma omp cancel parallel if (cancel: 1) if (1)		/* { dg-error "if any 'if' clause has modifier, then all 'if' clauses have to use modifier" } */
    #pragma omp cancel parallel if (cancel: 1) if (parallel: 1)	/* { dg-error "expected 'cancel' 'if' clause modifier" } */
    #pragma omp cancel parallel if (1) if (cancel: 1)		/* { dg-error "if any 'if' clause has modifier, then all 'if' clauses have to use modifier" } */
    #pragma omp cancel parallel if (parallel: 1) if (cancel: 1)	/* { dg-error "expected 'cancel' 'if' clause modifier" } */
  }
}
