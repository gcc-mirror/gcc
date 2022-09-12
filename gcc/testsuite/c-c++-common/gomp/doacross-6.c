void
foo (int n)
{
  int i;
  #pragma omp for ordered
  for (i = 0; i < 8; i += n)
    {
      #pragma omp ordered doacross(source)		/* { dg-error "expected ':' before '\\\)' token" } */
    }
  #pragma omp for ordered
  for (i = 0; i < 8; i += n)
    {
      #pragma omp ordered doacross(source:omp_current_iteration)	/* { dg-error "expected '\\\)' before 'omp_current_iteration'" } */
    }
  #pragma omp for ordered
  for (i = 0; i < 8; i += n)
    {
      #pragma omp ordered doacross(source:i - 2)	/* { dg-error "expected '\\\)' before 'i'" } */
    }
  #pragma omp for ordered
  for (i = 0; i < 8; i += n)
    {
      #pragma omp ordered doacross(sink)		/* { dg-error "expected ':' before '\\\)' token" } */
    }
  #pragma omp for ordered
  for (i = 0; i < 8; i += n)
    {
      #pragma omp ordered doacross(source)		/* { dg-error "expected ':' before '\\\)' token" } */
      #pragma omp ordered doacross(sink:i-1)
    }
  #pragma omp for ordered
  for (i = 0; i < 8; i += n)
    {
      #pragma omp ordered doacross(source:)
      #pragma omp ordered doacross(sink)		/* { dg-error "expected ':' before '\\\)' token" } */
    }
}

void
bar (int n)
{
  int i;
  #pragma omp for ordered
  for (i = 0; i < 8; i += n)
    {
      #pragma omp ordered doacross(sink:omp_current_iteration - 1)	/* { dg-error "'omp_current_iteration' undeclared \\\(first use in this function\\\)" "" { target c } } */
    }									/* { dg-error "'omp_current_iteration' has not been declared" "" { target c++ } .-1 } */
  #pragma omp for ordered
  for (i = 0; i < 8; i += n)
    {
      #pragma omp ordered doacross(sink:omp_cur_iteration)	/* { dg-error "'omp_cur_iteration' undeclared \\\(first use in this function\\\)" "" { target c } } */
    }								/* { dg-error "'omp_cur_iteration' has not been declared" "" { target c++ } .-1 } */
}

void
baz (int n)
{
  int i;
  #pragma omp for ordered
  for (i = 0; i < 8; i += n)
    {
      #pragma omp ordered doacross(sink:omp_cur_iteration + 1)	/* { dg-error "'omp_cur_iteration' undeclared \\\(first use in this function\\\)" "" { target c } } */
    }								/* { dg-error "'omp_cur_iteration' has not been declared" "" { target c++ } .-1 } */
}

void
qux (int n)
{
  int i;
  #pragma omp for ordered
  for (i = 0; i < 8; i += n)
    {
      #pragma omp ordered doacross(sink:omp_cur_iteration - (2 - 1))	/* { dg-error "'omp_cur_iteration' undeclared \\\(first use in this function\\\)" "" { target c } } */
    }								/* { dg-error "expected integer before '\\\(' token" "" { target *-*-* } .-1 } */
}								/* { dg-error "'omp_cur_iteration' has not been declared" "" { target c++ } .-2 } */
								/* { dg-error "expected '\\\)' before '\\\(' token" "" { target c++ } .-3 } */
void
corge (int n)
{
  int i;
  #pragma omp for ordered
  for (i = 0; i < 8; i += n)
    {
      #pragma omp ordered doacross(sink:omp_cur_iteration - 1)
    }
  #pragma omp for ordered
  for (i = 0; i < 8; i += n)
    {
      #pragma omp ordered doacross(sink:omp_cur_iteration - 1LL)
    }
  #pragma omp for ordered
  for (i = 0; i < 8; i += n)
    {
      #pragma omp ordered doacross(sink:omp_cur_iteration - 0x00001)
    }
  #pragma omp for ordered
  for (i = 0; i < 8; i += n)
    {
      #pragma omp ordered doacross(sink:omp_cur_iteration - 001)
    }
  #pragma omp for ordered
  for (i = 0; i < 8; i += n)
    {
      #pragma omp ordered doacross(sink:omp_cur_iteration - 1ULL)
    }
}
