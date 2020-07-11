/* PR middle-end/95315 */
/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp --param ggc-min-heapsize=0" } */

typedef float __v4sf __attribute__((vector_size (16)));
typedef int __v4si __attribute__((vector_size (16)));
typedef float __v8sf __attribute__((vector_size (32)));
typedef int __v8si __attribute__((vector_size (32)));
__v4si f1 (__v4sf, __v4sf, float *);
__v8si f2 (__v8sf, __v8sf, float *);
__v4si f3 (__v4si, int, __v4si);

#pragma omp declare variant (f1) match (construct={parallel,for,simd(simdlen(4),notinbranch,uniform(z),aligned(z:4 * sizeof (*z)))})
#pragma omp declare variant (f2) match (construct={for,simd(uniform(z),simdlen(8),notinbranch)})
int f4 (float x, float y, float *z);

#pragma omp declare variant (f3) match (construct={simd(simdlen(4),inbranch,linear(y:1))})
int f5 (int x, int y);

static inline __attribute__((always_inline)) int
ret_false (void)
{
  return 0;
}

void
test (int *x, float *y, float *z, float *w)
{
  #pragma omp parallel
  #pragma omp for simd aligned (w:4 * sizeof (float))
  for (int i = 0; i < 1024; i++)
    if (ret_false ())
      x[i] = f4 (y[i], z[i], w);
  #pragma omp parallel for simd aligned (w:4 * sizeof (float)) simdlen(4)
  for (int i = 1024; i < 2048; i++)
    if (ret_false ())
      x[i] = f4 (y[i], z[i], w);
  #pragma omp simd aligned (w:4 * sizeof (float))
  for (int i = 2048; i < 4096; i++)
    if (ret_false ())
      x[i] = f4 (y[i], z[i], w);
  #pragma omp simd
  for (int i = 4096; i < 8192; i++)
    if (x[i] > 10 && ret_false ())
      x[i] = f5 (x[i], i);
}
