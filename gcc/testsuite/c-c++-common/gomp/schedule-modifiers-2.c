void
foo (void)
{
  int i = 0;
  #pragma omp for simd schedule(simd, static)	/* { dg-error "expected ':'" } */
  for (i = 0; i < 16; i++)
    ;
  #pragma omp for simd schedule(monotonic, dynamic)	/* { dg-error "expected ':'" } */
  for (i = 0; i < 16; i++)
    ;
  #pragma omp for simd schedule(nonmonotonic, guided, 1)	/* { dg-error "expected ':'" } */
  for (i = 0; i < 16; i++)
    ;
}
