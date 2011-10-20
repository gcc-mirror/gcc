/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-require-effective-target vect_shift } */
/* { dg-require-effective-target vect_floatint_cvt } */

static inline float
i2f(int x)
{
  union { float f; int i; } tmp;
  tmp.i=x;
  return tmp.f;
}
static inline float
vect_ldexpf(float x, int n)
{
  n = (n+0x7f)<<23;
  return x * i2f(n);
}

float __attribute__ ((aligned(16))) a[1024];
float __attribute__ ((aligned(16))) b[1024];
float __attribute__ ((aligned(16))) c[1024];

void
tV()
{
  int i;
  for (i=0; i!=1024; ++i)
    {
      float z = a[i];
      int n = b[i];
      c[i] = vect_ldexpf(z,n);
    }
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
