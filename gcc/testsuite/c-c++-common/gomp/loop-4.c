int r, l;

void
f1 (int *a)
{
  int i;
  #pragma omp master
  {
    #pragma omp loop bind		/* { dg-error "expected" } */
    for (i = 0; i < 64; ++i)
      a[i] = i;
    #pragma omp loop bind )		/* { dg-error "expected" } */
    for (i = 0; i < 64; ++i)
      a[i] = i;
    #pragma omp loop bind (		/* { dg-error "expected" } */
    for (i = 0; i < 64; ++i)
      a[i] = i;
    #pragma omp loop bind ()		/* { dg-error "expected" } */
    for (i = 0; i < 64; ++i)
      a[i] = i;
    #pragma omp loop bind ( foobar )	/* { dg-error "expected" } */
    for (i = 0; i < 64; ++i)
      a[i] = i;
    #pragma omp loop bind (default)	/* { dg-error "expected" } */
    for (i = 0; i < 64; ++i)
      a[i] = i;
    #pragma omp loop bind (parallel	/* { dg-error "expected" } */
    for (i = 0; i < 64; ++i)
      a[i] = i;
  }
}

void
f2 (int *a)
{
  int i;
  #pragma omp loop bind(parallel) reduction(task, +: r)	/* { dg-error "invalid 'task' reduction modifier on construct other than 'parallel', 'for' or 'sections'" } */
  for (i = 0; i < 64; ++i)
    a[i] = i;
  #pragma omp loop bind(thread) reduction(inscan, +: r)	/* { dg-error "'inscan' 'reduction' clause on 'loop' construct" } */
  for (i = 0; i < 64; ++i)
    a[i] = i;
  #pragma omp loop bind(parallel) lastprivate (l)	/* { dg-error "'lastprivate' clause on a 'loop' construct refers to a variable 'l' which is not the loop iterator" } */
  for (i = 0; i < 64; ++i)
    l = i;
}
