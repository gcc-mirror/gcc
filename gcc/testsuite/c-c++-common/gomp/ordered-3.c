/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

void
foo (void)
{
  int i;
  #pragma omp for ordered
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered
      ;
    }
  #pragma omp for ordered
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered threads
      ;
    }
  #pragma omp for ordered
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered threads threads	/* { dg-error "too many .threads. clauses" } */
      ;
    }
  #pragma omp simd
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered simd
      ;
    }
  #pragma omp simd
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered simd simd		/* { dg-error "too many .simd. clauses" } */
      ;
    }
  #pragma omp for simd ordered
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered threads, simd
      ;
    }
  #pragma omp for simd ordered
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered threads, simd, threads, simd	/* { dg-error "too many .threads. clauses" } */
      ;	/* { dg-error "too many .simd. clauses" "" { target *-*-* } .-1 } */
    }
  #pragma omp for simd ordered(1)
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered depend(sink: i - 1)	/* { dg-error "clause must be closely nested inside a loop with .ordered. clause" } */
      #pragma omp ordered depend(source)	/* { dg-error "clause must be closely nested inside a loop with .ordered. clause" } */
    }
  #pragma omp for simd ordered(1)
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered doacross(sink: i - 1)	/* { dg-error "clause must be closely nested inside a loop with .ordered. clause" } */
      #pragma omp ordered doacross(source:omp_cur_iteration)	/* { dg-error "clause must be closely nested inside a loop with .ordered. clause" } */
    }
  #pragma omp parallel for simd ordered(1)
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered depend(sink: i - 1)	/* { dg-error "clause must be closely nested inside a loop with .ordered. clause" } */
      #pragma omp ordered depend(source)	/* { dg-error "clause must be closely nested inside a loop with .ordered. clause" } */
    }
  #pragma omp parallel for simd ordered(1)
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered doacross(sink: i - 1)	/* { dg-error "clause must be closely nested inside a loop with .ordered. clause" } */
      #pragma omp ordered doacross(source:)	/* { dg-error "clause must be closely nested inside a loop with .ordered. clause" } */
    }
  #pragma omp parallel for ordered
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered depend(sink: i - 1)
      #pragma omp ordered depend(source)
    }
  #pragma omp parallel for ordered
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered doacross(sink: i - 1)
      #pragma omp ordered doacross(source:)
    }
  #pragma omp parallel for
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered depend(sink: i - 1)	/* { dg-error "clause must be closely nested inside a loop with .ordered. clause" } */
      #pragma omp ordered depend(source)	/* { dg-error "clause must be closely nested inside a loop with .ordered. clause" } */
    }
  #pragma omp parallel for
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered doacross(sink: i - 1)	/* { dg-error "clause must be closely nested inside a loop with .ordered. clause" } */
      #pragma omp ordered doacross(source:)	/* { dg-error "clause must be closely nested inside a loop with .ordered. clause" } */
    }
}

void
bar (int x)
{
  switch (x)
    {
    case 0:
      #pragma omp ordered
      ;
      break;
    case 1:
      #pragma omp ordered threads
      ;
      break;
    case 2:
      #pragma omp ordered threads, threads	/* { dg-error "too many .threads. clauses" } */
      ;
      break;
    }
}

void
baz (void)
{
  #pragma omp ordered simd
  ;
  #pragma omp ordered simd, simd		/* { dg-error "too many .simd. clauses" } */
  ;
}
