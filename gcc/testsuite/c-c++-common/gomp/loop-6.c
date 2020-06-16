int bar (int);
int baz (int *);

void
f1 (int x)
{
  int i = 0, j = 0, k = 0;
  long long l = 0;
  #pragma omp for collapse(2)
  for (i = 0; i < 16; i = i + 2)
    for (j = i * i; j < 16; j += 2)	/* { dg-error "initializer expression refers to iteration variable" } */
      ;
  #pragma omp for collapse(2)
  for (i = 0; i < 16; i = i + 2)
    for (j = i + 3; j < (i + 1) * 2; j += 2)	/* { dg-error "condition expression refers to iteration variable" } */
      ;
  #pragma omp for collapse(2)
  for (i = 0; i < 16; i = i + 2)
    for (j = (i + 1) * 2; j < i * 8; j += 2)	/* { dg-error "initializer expression refers to iteration variable" } */
      ;
  #pragma omp for collapse(3)
  for (i = 0; i < 16; i = i + 2)
    for (j = 0; j < 16; j++)
      for (k = i + j; k < 32; k++)	/* { dg-error "initializer expression refers to iteration variable" } */
	;
  #pragma omp for collapse(2)
  for (l = 0; l < 16; l++)
    for (j = 0; j < l; j++)		/* { dg-error "outer iteration variable 'l' used in condition expression has type other than 'int'" } */
      ;
  #pragma omp for collapse(2)		/* { dg-error "outer iteration variable 'l' used in initializer expression has type other than 'int'" "" { target c } } */
  for (l = 0; l < 16; l++)		/* { dg-error "outer iteration variable 'l' used in initializer expression has type other than 'int'" "" { target c++ } } */
    for (j = 7LL * l; j < 32; j++)
      ;
  #pragma omp for collapse(2)
  for (i = 0; i < 16; i = i + 2)
    for (j = i + 3; j < i * 2 + 2; j += 2)
      ;
  #pragma omp for collapse(2)
  for (i = 0; i < 16; i = i + 2)
    for (j = i * 2 + 2; j < i * 6 + 2; j += 2)
      ;
  #pragma omp for collapse(3)
  for (i = 0; i < 16; i = i + 2)
    for (j = 0; j < 16; j++)
      for (k = 14 + 7 * i; k < 32 * j; k++)	/* { dg-error "two different outer iteration variables 'i' and 'j' used in a single loop" } */
	;
  #pragma omp for schedule(static, 2) collapse(2)	/* { dg-error "'schedule' clause may not appear on non-rectangular 'for'" } */
  for (i = 0; i < 16; i++)
    for (j = 1; j < i; j++)
      ;
  #pragma omp for schedule(static) collapse(2)		/* { dg-error "'schedule' clause may not appear on non-rectangular 'for'" } */
  for (i = 0; i < 16; i++)
    for (j = 1; j < i; j++)
      ;
  #pragma omp for schedule(dynamic, 5) collapse(2)	/* { dg-error "'schedule' clause may not appear on non-rectangular 'for'" } */
  for (i = 0; i < 16; i++)
    for (j = 1; j < i; j++)
      ;
  #pragma omp for ordered collapse(2)			/* { dg-error "'ordered' clause may not appear on non-rectangular 'for'" } */
  for (i = 0; i < 16; i++)
    for (j = 1; j < i; j++)
      ;
  #pragma omp for ordered collapse(2)			/* { dg-error "'ordered' clause may not appear on non-rectangular 'for'" } */
  for (i = 0; i < 16; i++)
    for (j = 1; j < i; j++)
      ;
  #pragma omp for ordered (3) collapse (2)		/* { dg-error "'ordered' clause may not appear on non-rectangular 'for'" } */
  for (i = 0; i < 64; i++)
    for (j = 0; j < i; j++)
      for (k = 0; k < 64; k++)
        {
          #pragma omp ordered depend (sink: i - 1, j - 2, k - 3)
          #pragma omp ordered depend (source)
        }
  #pragma omp for ordered (3) collapse (2)		/* { dg-error "'ordered' clause may not appear on non-rectangular 'for'" } */
  for (i = 0; i < 64; i++)
    for (j = 0; j < 64; j++)
      for (k = i; k < 64; k++)
        {
          #pragma omp ordered depend (sink: i - 1, j - 2, k - 3)
          #pragma omp ordered depend (source)
        }
  #pragma omp for simd schedule(simd: static) collapse(2)	/* { dg-error "'schedule' clause may not appear on non-rectangular 'for'" } */
  for (i = 0; i < 16; i++)
    for (j = 1; j < i; j++)
      ;
}

void
f2 (void)
{
  int i = 0, j = 0;
  #pragma omp distribute dist_schedule(static, 4) collapse(2)	/* { dg-error "'dist_schedule' clause may not appear on non-rectangular 'distribute'" } */
  for (i = 0; i < 64; i++)
    for (j = i; j < 64; j++)
      ;
  #pragma omp distribute collapse(2) dist_schedule(static)	/* { dg-error "'dist_schedule' clause may not appear on non-rectangular 'distribute'" } */
  for (i = 0; i < 64; i++)
    for (j = i; j < 64; j++)
      ;
  #pragma omp distribute parallel for simd schedule(simd: static) collapse(2)	/* { dg-error "'schedule' clause may not appear on non-rectangular 'for'" } */
  for (i = 0; i < 16; i++)
    for (j = 1; j < i; j++)
      ;
  #pragma omp distribute parallel for simd collapse(2) dist_schedule(static)	/* { dg-error "'dist_schedule' clause may not appear on non-rectangular 'distribute'" } */
  for (i = 0; i < 64; i++)
    for (j = i; j < 64; j++)
      ;
  #pragma omp distribute simd collapse(2) dist_schedule(static)	/* { dg-error "'dist_schedule' clause may not appear on non-rectangular 'distribute'" } */
  for (i = 0; i < 64; i++)
    for (j = i; j < 64; j++)
      ;
}
