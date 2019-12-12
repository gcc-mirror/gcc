void
foo (void)
{
  int x = 0, i;
  #pragma omp task default(none)	/* { dg-error "enclosing 'task'" } */
  {
    x++;	/* { dg-error "'x' not specified in enclosing 'task'" } */
  }
  #pragma omp taskloop default(none)	/* { dg-error "enclosing 'taskloop'" } */
  for (i = 0; i < 64; i++)
    {
      x++;	/* { dg-error "'x' not specified in enclosing 'taskloop'" } */
    }
  #pragma omp teams default(none)	/* { dg-error "enclosing 'teams'" } */
  {
    x++;	/* { dg-error "'x' not specified in enclosing 'teams'" } */
  }
  #pragma omp parallel default(none)	/* { dg-error "enclosing 'parallel'" } */
  {
    x++;	/* { dg-error "'x' not specified in enclosing 'parallel'" } */
  }
}
