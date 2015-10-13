/* { dg-do run } */
/* { dg-options "-O2 -fopenmp-simd" } */
/* { dg-additional-options "-msse2" { target sse2_runtime } } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

#define N 1024
extern
#ifdef __cplusplus
"C"
#endif
void abort (void);

int last;

void
bar (unsigned char *a, int i, int safelen)
{
  int j, k;
  if (i != last++)
    abort ();
  for (j = i - safelen - 32; j < i; j++)
    if (j >= 0 && a[j] != 2)
      break;
  if (j <= i - safelen || a[j] != 1)
    abort ();
  for (k = j; k < i + safelen + 32; k++)
    if (k >= N || a[k] != 1)
      break;
  if (k <= i || k > j + safelen)
    abort ();
  if (k < N && a[k] != 0)
    abort ();
  for (; k < i + safelen + 32; k++)
    if (k < N && a[k] != 0)
      abort ();
}

static inline void
foo (unsigned char *a, int i)
{
  #pragma omp ordered simd
  bar (a, i, 64);
}

int
main ()
{
  unsigned char a[N], b[N];
  int i;
  #pragma omp simd
  for (i = 0; i < N; i++)
    a[i] = 0;
  #pragma omp simd safelen (64)
  for (i = 0; i < N; i++)
    {
      a[i]++;
      foo (a, i);
      a[i]++;
    }
  #pragma omp simd
  for (i = 0; i < N; i++)
    {
      a[i] = 0;
      b[i] = 0;
    }
  last = 0;
  #pragma omp simd safelen (32)
  for (i = 0; i < N; i++)
    {
      a[i]++;
      #pragma omp ordered simd
      bar (a, i, 32);
      a[i]++;
    }
  for (i = 0; i < N; i++)
    if (a[i] != 2)
      abort ();
  #pragma omp simd safelen (32)
  for (i = 1; i < N; i++)
    {
      #pragma omp ordered simd
      b[i] = b[i - 1] + 1;
      a[i]++;
      #pragma omp ordered simd
      a[i] += a[i - 1];
    }
  for (i = 0; i < N; i++)
    if (a[i] != (unsigned char) (2 + 3 * i) || b[i] != (unsigned char) i)
      abort ();
  return 0;
}
