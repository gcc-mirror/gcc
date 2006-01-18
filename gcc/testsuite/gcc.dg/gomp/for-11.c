/* { dg-do compile } */
/* { dg-options "-std=c99 -fopenmp" } */

extern void baz (int);

void foo (int j, int k)
{
  int i;

  /* Valid loops.  */
  #pragma omp for
  for (i = 0; i < 10; i++)
    baz (i);

  #pragma omp for
  for (i = j; i <= 10; i+=4)
    baz (i);

  #pragma omp for
  for (i = j; i > 0; i = i - 1)
    baz (j);

  #pragma omp for
  for (i = j; i >= k; i--)
    baz (i);

  #pragma omp for
  for (int l = j; l < 10; l++)
    baz (l);

  /* Malformed parallel loops.  */
  #pragma omp for
  i = 0;		/* { dg-error "for statement expected" } */
  for ( ; i < 10; )
    {
      baz (i);
      i++;
    }

  #pragma omp for
  for (i = 0; ; i--)	/* { dg-error "missing controlling predicate" } */
    {
      if (i >= 10)
	break;		/* { dg-error "break" } */
      baz (i);
    }

  #pragma omp for
  for (i = 0;
       i < 10 && j > 4; /* { dg-error "invalid controlling predicate" } */
       i-=3)
    baz (i);

  #pragma omp for
  for (i = 0;
       i < 10;
       i-=3, j+=2)	/* { dg-error "invalid increment expression" } */
    baz (i);

  int m = 0;
  #pragma omp for
  for (; m < 10; m++)	/* { dg-error "expected" } */
    baz (m);

  m = 0;
  #pragma omp for
  for (int n = 0; m < 10; m++)	/* { dg-error "invalid controlling predicate|invalid increment expression" } */
    baz (m);

  #pragma omp for
  for (m = 0; m < 10; i++)	/* { dg-error "invalid increment expression" } */
    baz (m);
}
