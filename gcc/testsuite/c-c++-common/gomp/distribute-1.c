int s1, s2, s3, s4, s5, s6, s7, s8;
#pragma omp declare target (s1, s2, s3, s4, s5, s6, s7, s8)

void
f1 (void)
{
  int i;
  #pragma omp distribute
  for (i = 0; i < 64; i++)
    ;
  #pragma omp distribute private (i)
  for (i = 0; i < 64; i++)
    ;
  #pragma omp distribute
  for (int j = 0; j < 64; j++)
    ;
  #pragma omp distribute lastprivate (s1)
  for (s1 = 0; s1 < 64; s1 += 2)
    ;
  #pragma omp distribute lastprivate (s2)
  for (i = 0; i < 64; i++)
    s2 = 2 * i;
  #pragma omp distribute simd
  for (i = 0; i < 64; i++)
    ;
  #pragma omp distribute simd lastprivate (s3, s4) collapse(2)
  for (s3 = 0; s3 < 64; s3++)
    for (s4 = 0; s4 < 3; s4++)
      ;
  #pragma omp distribute parallel for
  for (i = 0; i < 64; i++)
    ;
  #pragma omp distribute parallel for private (i)
  for (i = 0; i < 64; i++)
    ;
  #pragma omp distribute parallel for lastprivate (s5)
  for (s5 = 0; s5 < 64; s5++)
    ;
  #pragma omp distribute firstprivate (s7) private (s8)
  for (i = 0; i < 64; i++)
    s8 = s7++;
}

void
f2 (void)
{
  int i;
  #pragma omp distribute lastprivate (i)	/* { dg-error "lastprivate variable .i. is private in outer context" } */
  for (i = 0; i < 64; i++)
    ;
  #pragma omp distribute firstprivate (s6) lastprivate (s6) /* { dg-error "same variable used in .firstprivate. and .lastprivate. clauses on .distribute. construct" } */
  for (i = 0; i < 64; i++)
    s6 += i;
}

#pragma omp declare target to(f1, f2)
