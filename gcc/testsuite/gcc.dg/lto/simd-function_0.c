/* { dg-lto-do link } */
/* { dg-require-effective-target vect_simd_clones } */
/* { dg-require-effective-target avx2 } */
/* { dg-lto-options { { -fopenmp-simd -O3 -ffast-math -mavx2 -flto -flto-partition=max } } } */

#define SIZE 4096
float x[SIZE];


#pragma omp declare simd
float
__attribute__ ((noinline))
my_mul (float x, float y) {
  return x * y;
}

__attribute__ ((noinline))
int foo ()
{
  int i = 0;
#pragma omp simd safelen (16)
  for (i = 0; i < SIZE; i++)
    x[i] = my_mul ((float)i, 9932.3323);
  return (int)x[0];
}

int main ()
{
  int i = 0;
  for (i = 0; i < SIZE; i++)
    x[i] = my_mul ((float) i, 9932.3323);
  foo ();
  return (int)x[0];
}

