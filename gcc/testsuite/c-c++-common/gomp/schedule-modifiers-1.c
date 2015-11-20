/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

void
foo (void)
{
  int i;
  #pragma omp for simd schedule (simd, simd: static, 5)
  for (i = 0; i < 64; i++)
    ;
  #pragma omp for simd schedule (monotonic, simd: static)
  for (i = 0; i < 64; i++)
    ;
  #pragma omp for simd schedule (simd , monotonic : static, 6)
  for (i = 0; i < 64; i++)
    ;
  #pragma omp for schedule (monotonic, monotonic : static, 7)
  for (i = 0; i < 64; i++)
    ;
  #pragma omp for schedule (nonmonotonic, nonmonotonic : dynamic)
  for (i = 0; i < 64; i++)
    ;
  #pragma omp for simd schedule (nonmonotonic , simd : dynamic, 3)
  for (i = 0; i < 64; i++)
    ;
  #pragma omp for simd schedule (nonmonotonic,simd:guided,4)
  for (i = 0; i < 64; i++)
    ;
  #pragma omp for schedule (monotonic: static, 2)
  for (i = 0; i < 64; i++)
    ;
  #pragma omp for schedule (monotonic : static)
  for (i = 0; i < 64; i++)
    ;
  #pragma omp for schedule (monotonic : dynamic)
  for (i = 0; i < 64; i++)
    ;
  #pragma omp for schedule (monotonic : dynamic, 3)
  for (i = 0; i < 64; i++)
    ;
  #pragma omp for schedule (monotonic : guided)
  for (i = 0; i < 64; i++)
    ;
  #pragma omp for schedule (monotonic : guided, 7)
  for (i = 0; i < 64; i++)
    ;
  #pragma omp for schedule (monotonic : runtime)
  for (i = 0; i < 64; i++)
    ;
  #pragma omp for schedule (monotonic : auto)
  for (i = 0; i < 64; i++)
    ;
  #pragma omp for schedule (nonmonotonic : dynamic)
  for (i = 0; i < 64; i++)
    ;
  #pragma omp for schedule (nonmonotonic : dynamic, 3)
  for (i = 0; i < 64; i++)
    ;
  #pragma omp for schedule (nonmonotonic : guided)
  for (i = 0; i < 64; i++)
    ;
  #pragma omp for schedule (nonmonotonic : guided, 7)
  for (i = 0; i < 64; i++)
    ;
}

void
bar (void)
{
  int i;
  #pragma omp for schedule (nonmonotonic: static, 2)	/* { dg-error ".nonmonotonic. modifier specified for .static. schedule kind" } */
  for (i = 0; i < 64; i++)
    ;
  #pragma omp for schedule (nonmonotonic : static)	/* { dg-error ".nonmonotonic. modifier specified for .static. schedule kind" } */
  for (i = 0; i < 64; i++)
    ;
  #pragma omp for schedule (nonmonotonic : runtime)	/* { dg-error ".nonmonotonic. modifier specified for .runtime. schedule kind" } */
  for (i = 0; i < 64; i++)
    ;
  #pragma omp for schedule (nonmonotonic : auto)	/* { dg-error ".nonmonotonic. modifier specified for .auto. schedule kind" } */
  for (i = 0; i < 64; i++)
    ;
  #pragma omp for schedule (nonmonotonic, dynamic) ordered	/* { dg-error ".nonmonotonic. schedule modifier specified together with .ordered. clause" } */
  for (i = 0; i < 64; i++)
    #pragma omp ordered
      ;
  #pragma omp for ordered schedule(nonmonotonic, dynamic, 5)	/* { dg-error ".nonmonotonic. schedule modifier specified together with .ordered. clause" } */
  for (i = 0; i < 64; i++)
    #pragma omp ordered
      ;
  #pragma omp for schedule (nonmonotonic, guided) ordered(1)	/* { dg-error ".nonmonotonic. schedule modifier specified together with .ordered. clause" } */
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered depend(sink: i - 1)
      #pragma omp ordered depend(source)
    }
  #pragma omp for ordered(1) schedule(nonmonotonic, guided, 2)	/* { dg-error ".nonmonotonic. schedule modifier specified together with .ordered. clause" } */
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered depend(source)
      #pragma omp ordered depend(sink: i - 1)
    }
  #pragma omp for schedule (nonmonotonic , monotonic : dynamic)	/* { dg-error "both .monotonic. and .nonmonotonic. modifiers specified" } */
  for (i = 0; i < 64; i++)
    ;
  #pragma omp for schedule (monotonic,nonmonotonic:dynamic)	/* { dg-error "both .monotonic. and .nonmonotonic. modifiers specified" } */
  for (i = 0; i < 64; i++)
    ;
}
