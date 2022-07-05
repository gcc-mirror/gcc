// { dg-do compile }
// { dg-options "-fopenmp" }

int i;

void
foo (int &x, int &y)
{
  #pragma omp simd linear (x: step (y + 1), ref)		// { dg-error "modifier other than 'val' specified in 'linear' clause" }
  for (i = 0; i < 10; i++)
    x += y + 1;
  #pragma omp simd linear (x: uval, step (y + 1))		// { dg-error "modifier other than 'val' specified in 'linear' clause" }
  for (i = 0; i < 10; i++)
    x += y + 1;
  #pragma omp parallel for linear (x: ref, step (y + 1))	// { dg-error "modifier other than 'val' specified in 'linear' clause" }
  for (i = 0; i < 10; i++)
    x += y + 1;
  #pragma omp parallel for linear (x: step (y + 1), uval)	// { dg-error "modifier other than 'val' specified in 'linear' clause" }
  for (i = 0; i < 10; i++)
    x += y + 1;
  #pragma omp parallel for simd linear (x: step (y + 1), ref)	// { dg-error "modifier other than 'val' specified in 'linear' clause" }
  for (i = 0; i < 10; i++)
    x += y + 1;
  #pragma omp parallel for simd linear (x: uval, step (y + 1))	// { dg-error "modifier other than 'val' specified in 'linear' clause" }
  for (i = 0; i < 10; i++)
    x += y + 1;
}
