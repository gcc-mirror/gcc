/* { dg-do compile } */
/* { dg-options "-fcilkplus -fopenmp" } */

int *a, *b, c;
void *jmpbuf[10];

void foo()
{
  int j;

#pragma simd
  for (int i=0; i < 1000; ++i)
    {
      if (c == 6)
	__builtin_setjmp (jmpbuf); /* { dg-error "setjmp" } */
      a[i] = b[i];
    }

#pragma simd
  for (int i=0; i < 1000; ++i)
    {
      if (c==5)
	break; /* { dg-error "break statement " } */
    }

#pragma simd
  for (int i=0; i < 1000; ++i)
    {
#pragma omp for /* { dg-error "OpenMP constructs may not" } */
      for (j=0; j < 1000; ++j)
	a[i] = b[i];
    }
}
