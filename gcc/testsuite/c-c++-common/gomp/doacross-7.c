void
foo (int l)
{
  int i, j, k;
  #pragma omp parallel
  {
    #pragma omp for schedule(static) ordered (3)
    for (i = 2; i < 256 / 16 - 1; i++)
      for (j = 0; j < 8; j += 2)
	for (k = 1; k <= 3; k++)
	  {
	    #pragma omp ordered doacross(sink: omp_cur_iteration - 1)
	    #pragma omp ordered doacross(source:)
	  }
    #pragma omp for schedule(static) ordered (3) collapse(2)
    for (i = 2; i < 256 / 16 - 1; i++)
      for (j = 0; j < 8; j += 2)
	for (k = 1; k <= 3; k++)
	  {
	    #pragma omp ordered doacross(sink: omp_cur_iteration - 1)
	    #pragma omp ordered doacross(source:)
	  }
    #pragma omp for schedule(static) ordered (3) collapse(3)
    for (i = 2; i < 256 / 16 - 1; i++)
      for (j = 0; j < 8; j += 2)
	for (k = 1; k <= 3; k++)
	  {
	    #pragma omp ordered doacross(sink: omp_cur_iteration - 1)
	    #pragma omp ordered doacross(source: omp_cur_iteration)
	  }
    #pragma omp for schedule(static) ordered (1) nowait
    for (i = 2; i < 256 / 16 - 1; i += l)
      {
	#pragma omp ordered doacross(sink: omp_cur_iteration - 1)
	#pragma omp ordered doacross(source:)
      }
  }
}

void
bar (int l, int m, int n, int o)
{
  int i, j, k;
  #pragma omp for schedule(static) ordered (3)
  for (i = 2; i < 256 / 16 - 1; i++)
    for (j = 0; j < m; j += n)
      for (k = o; k <= 3; k++)
	{
	  foo (l);
	  #pragma omp ordered doacross(sink: omp_cur_iteration - 1)
	  #pragma omp ordered doacross(source:omp_cur_iteration)
	}
  #pragma omp for schedule(static) ordered (3) collapse(2)
  for (i = 2; i < 256 / 16 - m; i += n)
    for (j = 0; j < 8; j += o)
      for (k = 1; k <= 3; k++)
	{
	  foo (l);
	  #pragma omp ordered doacross(sink: omp_cur_iteration - 1)
	  #pragma omp ordered doacross(source : omp_cur_iteration)
	}
  #pragma omp for schedule(static) ordered (3) collapse(3)
  for (i = m; i < 256 / 16 - 1; i++)
    for (j = 0; j < n; j += 2)
      for (k = 1; k <= o; k++)
	{
	  foo (l);
	  #pragma omp ordered doacross(sink: omp_cur_iteration - 1)
	  #pragma omp ordered doacross(source :)
	}
  #pragma omp for schedule(static) ordered
  for (i = m; i < n / 16 - 1; i += l)
    {
      foo (l);
      #pragma omp ordered doacross(sink: omp_cur_iteration - 1)
      #pragma omp ordered doacross(source: omp_cur_iteration)
    }
}
