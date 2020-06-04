/* PR middle-end/70550 */
/* { dg-do compile } */
/* { dg-additional-options "-Wuninitialized" } */

#ifdef __SIZEOF_INT128__
typedef __int128 T;
#else
typedef long long T;
#endif

void bar (T);
#pragma omp declare target (bar)

void
foo (void)
{
  {
    int i;
    #pragma omp target defaultmap(tofrom:scalar)	/* { dg-bogus "is used uninitialized" } */
    {
      i = 26;
      bar (i);
    }
  }
  {
    T j;
    #pragma omp target defaultmap(tofrom:scalar)	/* { dg-bogus "is used uninitialized" } */
    {
      j = 37;
      bar (j);
    }
  }
  {
    int i;
    #pragma omp target					/* { dg-bogus "is used uninitialized" } */
    {
      i = 26;
      bar (i);
    }
  }
  {
    T j;
    #pragma omp target					/* { dg-bogus "is used uninitialized" } */
    {
      j = 37;
      bar (j);
    }
  }
  {
    int i;
    #pragma omp target firstprivate (i)			/* { dg-warning "is used uninitialized" } */
    {
      i = 26;
      bar (i);
    }
  }
  {
    T j;
    #pragma omp target firstprivate (j)			/* { dg-warning "is used uninitialized" } */
    {
      j = 37;
      bar (j);
    }
  }
  {
    int i;
    #pragma omp target private (i)			/* { dg-bogus "is used uninitialized" } */
    {
      i = 26;
      bar (i);
    }
  }
  {
    T j;
    #pragma omp target private (j)			/* { dg-bogus "is used uninitialized" } */
    {
      j = 37;
      bar (j);
    }
  }
}
