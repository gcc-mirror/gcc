/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

void
foo (void)
{
  int i;
  #pragma omp for ordered			/* { dg-error "'ordered' clause used with generated loops" } */
  #pragma omp tile sizes (2)
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered
      ;
    }
  #pragma omp for ordered			/* { dg-error "'ordered' clause used with generated loops" } */
  #pragma omp tile sizes (2)
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered threads
      ;
    }
  #pragma omp for simd ordered			/* { dg-error "'ordered' clause used with generated loops" "" { target c } } */
  #pragma omp tile sizes (2)			/* { dg-error "'ordered' clause used with generated loops" "" { target c++ } } */
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered threads, simd
      ;
    }
  #pragma omp for simd ordered(1)		/* { dg-error "'ordered' clause used with generated loops" "" { target c } } */
  #pragma omp tile sizes (2)			/* { dg-error "'ordered' clause used with generated loops" "" { target c++ } } */
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered depend(sink: i - 1)
      #pragma omp ordered depend(source)
    }
  #pragma omp for simd ordered(1)		/* { dg-error "'ordered' clause used with generated loops" "" { target c } } */
  #pragma omp tile sizes (2)			/* { dg-error "'ordered' clause used with generated loops" "" { target c++ } } */
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered doacross(sink: i - 1)
      #pragma omp ordered doacross(source:omp_cur_iteration)
    }
  #pragma omp parallel for ordered		/* { dg-error "'ordered' clause used with generated loops" "" { target c } } */
  #pragma omp tile sizes (2)			/* { dg-error "'ordered' clause used with generated loops" "" { target c++ } } */
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered depend(sink: i - 1)
      #pragma omp ordered depend(source)
    }
  #pragma omp parallel for ordered		/* { dg-error "'ordered' clause used with generated loops" "" { target c } } */
  #pragma omp tile sizes (2)			/* { dg-error "'ordered' clause used with generated loops" "" { target c++ } } */
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered doacross(sink: i - 1)
      #pragma omp ordered doacross(source:)
    }
  #pragma omp for ordered(1)			/* { dg-error "'ordered' clause used with generated loops" } */
  #pragma omp tile sizes (2)
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered depend(sink: i - 1)
      #pragma omp ordered depend(source)
    }
  #pragma omp for ordered(1)			/* { dg-error "'ordered' clause used with generated loops" } */
  #pragma omp tile sizes (2)
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered doacross(sink: i - 1)
      #pragma omp ordered doacross(source:omp_cur_iteration)
    }
}

void
bar (void)
{
  int i;
  #pragma omp for ordered			/* { dg-error "'ordered' clause used with generated loops" } */
  #pragma omp unroll partial (2)
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered
      ;
    }
  #pragma omp for ordered			/* { dg-error "'ordered' clause used with generated loops" } */
  #pragma omp unroll partial (2)
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered threads
      ;
    }
  #pragma omp for simd ordered			/* { dg-error "'ordered' clause used with generated loops" "" { target c } } */
  #pragma omp unroll partial (2)		/* { dg-error "'ordered' clause used with generated loops" "" { target c++ } } */
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered threads, simd
      ;
    }
  #pragma omp for simd ordered(1)		/* { dg-error "'ordered' clause used with generated loops" "" { target c } } */
  #pragma omp unroll partial (2)		/* { dg-error "'ordered' clause used with generated loops" "" { target c++ } } */
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered depend(sink: i - 1)
      #pragma omp ordered depend(source)
    }
  #pragma omp for simd ordered(1)		/* { dg-error "'ordered' clause used with generated loops" "" { target c } } */
  #pragma omp unroll partial (2)		/* { dg-error "'ordered' clause used with generated loops" "" { target c++ } } */
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered doacross(sink: i - 1)
      #pragma omp ordered doacross(source:omp_cur_iteration)
    }
  #pragma omp parallel for ordered		/* { dg-error "'ordered' clause used with generated loops" "" { target c } } */
  #pragma omp unroll partial (2)		/* { dg-error "'ordered' clause used with generated loops" "" { target c++ } } */
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered depend(sink: i - 1)
      #pragma omp ordered depend(source)
    }
  #pragma omp parallel for ordered		/* { dg-error "'ordered' clause used with generated loops" "" { target c } } */
  #pragma omp unroll partial (2)		/* { dg-error "'ordered' clause used with generated loops" "" { target c++ } } */
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered doacross(sink: i - 1)
      #pragma omp ordered doacross(source:)
    }
  #pragma omp for ordered(1)			/* { dg-error "'ordered' clause used with generated loops" } */
  #pragma omp unroll partial (2)
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered depend(sink: i - 1)
      #pragma omp ordered depend(source)
    }
  #pragma omp for ordered(1)			/* { dg-error "'ordered' clause used with generated loops" } */
  #pragma omp unroll partial (2)
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered doacross(sink: i - 1)
      #pragma omp ordered doacross(source:omp_cur_iteration)
    }
}
