/* { dg-do compile } */
/* { dg-options "-O3 -fopenmp" } */

int *a, *b, c;
void *jmpbuf[10];

void foo()
{
  int i, j;

#pragma simd
  for (i=0; i < 1000; ++i)
    {
      if (c == 5)
	return;	 /* { dg-error "return statments are not allowed" } */
      if (c == 6)
	__builtin_setjmp (jmpbuf); /* { dg-error "calls to setjmp are not allowed" } */
      a[i] = b[i];
    }

#pragma simd
  for (i=0; i < 1000; ++i)
    {
      if (c==5)
	break; /* { dg-error "break statement within" } */
    }

#pragma simd
  for (i=0; i < 1000; ++i)
    {
#pragma omp for /* { dg-error "OpenMP statements are not allowed" } */
      for (j=0; j < 1000; ++j)
	a[i] = b[i];
    }
}
