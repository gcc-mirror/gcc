void
foo (int n)
{
  int i;
  #pragma omp for ordered
  for (i = 0; i < 8; i += n)
    {
      #pragma omp ordered doacross(source:)
      #pragma omp ordered doacross(sink: i - 2)
    }
}

void
bar (int n)
{
  int i, j;
  #pragma omp for collapse(2) ordered(2)
  for (i = 0; i < 8; i += n)
    for (j = 0; j < 8; j += n)
      {
	#pragma omp ordered doacross(source:omp_cur_iteration)
	#pragma omp ordered doacross(sink: i - 2, j + 2)
      }
}

void
baz (void)
{
  int i, j;
  #pragma omp for ordered(1)
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered			/* { dg-error "'ordered' construct without 'doacross' or 'depend' clauses must not have the same binding region as 'ordered' construct with those clauses" } */
      ;
      #pragma omp ordered doacross(source:)
      #pragma omp ordered doacross(sink: i - 1)
    }
  #pragma omp for ordered
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered doacross(source: omp_cur_iteration )
      #pragma omp ordered doacross(sink: i - 1)
      #pragma omp ordered threads		/* { dg-error "'ordered' construct without 'doacross' or 'depend' clauses must not have the same binding region as 'ordered' construct with those clauses" } */
      ;
    }
  #pragma omp for ordered(2)
  for (i = 0; i < 8; i++)
    for (j = 0; j < 8; j++)
      {
	#pragma omp ordered			/* { dg-error "'ordered' construct without 'doacross' or 'depend' clauses binds to loop where 'collapse' argument 1 is different from 'ordered' argument 2" } */
	;
      }
  #pragma omp for ordered(2) collapse(1)
  for (i = 0; i < 8; i++)
    for (j = 0; j < 8; j++)
      {
	#pragma omp ordered threads		/* { dg-error "'ordered' construct without 'doacross' or 'depend' clauses binds to loop where 'collapse' argument 1 is different from 'ordered' argument 2" } */
	;
      }
}

void
qux (void)
{
  int i, j = 0;
  #pragma omp for ordered linear(j)
  for (i = 0; i < 64; i++)
    {
      ++j;
      #pragma omp ordered
      ;
    }
  #pragma omp for ordered linear(j)		/* { dg-error "'linear' clause may not be specified together with 'ordered' clause if stand-alone 'ordered' construct is nested in it" } */
  for (i = 0; i < 64; i++)
    {
      ++j;
      #pragma omp ordered doacross(source:)
      #pragma omp ordered doacross(sink:i-1)
    }
  #pragma omp for ordered(1) linear(j)
  for (i = 0; i < 64; i++)
    {
      ++j;
      #pragma omp ordered
      ;
    }
  #pragma omp for ordered(1) linear(j)		/* { dg-error "'linear' clause may not be specified together with 'ordered' clause if stand-alone 'ordered' construct is nested in it" } */
  for (i = 0; i < 64; i++)
    {
      ++j;
      #pragma omp ordered doacross(source:)
      #pragma omp ordered doacross(sink:i-1)
    }
}
