void
foo (void)
{
  int i;
  #pragma omp task default(copyprivate)	/* { dg-error "expected 'none', 'shared', 'private' or 'firstprivate' before 'copyprivate'" } */
  ;
  #pragma omp taskloop default(copyprivate)	/* { dg-error "expected 'none', 'shared', 'private' or 'firstprivate' before 'copyprivate'" } */
  for (i = 0; i < 64; i++)
    ;
  #pragma omp teams default(copyprivate)	/* { dg-error "expected 'none', 'shared', 'private' or 'firstprivate' before 'copyprivate'" } */
  ;
  #pragma omp parallel default(copyprivate)	/* { dg-error "expected 'none', 'shared', 'private' or 'firstprivate' before 'copyprivate'" } */
  ;
}
