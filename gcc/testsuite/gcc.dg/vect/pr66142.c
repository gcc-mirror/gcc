/* PR middle-end/66142 */
/* { dg-do compile } */
/* { dg-additional-options "-ffast-math -fopenmp-simd" } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

struct A { float x, y; };
struct B { struct A t, w; };

static inline float
bar (const struct B *x)
{
  struct A r;
  float b, c, d;
  r.x = x->t.x;
  r.y = x->t.y;
  b = r.x * x->w.x + r.y * x->w.y;
  c = b + r.x * r.x + r.y * r.y;
  if (c > 0.0f)
    return c + 3.0f;
  return 0.0f;
}

void
foo (float *a, float *b, float *c)
{
  int i;
  float z = 0.0f;
  float u = *a;
#pragma omp simd
  for (i = 0; i < 32; i++)
    {
      float x = b[i];
      float y = c[i];
      struct B r;
      r.t.x = 1.0f;
      r.t.y = u;
      r.w.x = x;
      r.w.y = y;
      z += bar (&r);
    }
  *a = z;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 1 "vect" { target vect_condition } } } */
