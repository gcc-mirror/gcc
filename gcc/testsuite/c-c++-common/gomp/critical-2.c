int i;

void
foo (int j)
{
  #pragma omp critical (foo) hint (j + 1)	/* { dg-error "constant integer expression" } */
  i = i + 1;
  #pragma omp critical (foo),hint(j)		/* { dg-error "constant integer expression" } */
  i = i + 1;
}
