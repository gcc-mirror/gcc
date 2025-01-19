/* { dg-do compile } */
/* { dg-options "-O3 -fopenmp-simd" } */

typedef int a;
typedef struct {
  a b __attribute__((__vector_size__(8)));
} c;

typedef a d __attribute__((__vector_size__(8)));
c e, f, g;
d h, j;
void k() {
  c l;
  l.b[1] = 0;
  c m = l;
  __builtin_memcpy(&h, &m, sizeof(h));
  j = h;
  {
    c l;
    l.b[1] = 0;
    m = l;
    __builtin_memcpy(&h, &m, sizeof(h));
    d m = j;
    __builtin_memcpy(&g, &m, sizeof(g));
    e = g;
    m = h;
    __builtin_memcpy(&g, &m, sizeof(g));
#pragma omp simd
    for (long i = 0; i < f.b[0]; i++)
      f.b[i] = e.b[i] > g.b[i];
  }
}
