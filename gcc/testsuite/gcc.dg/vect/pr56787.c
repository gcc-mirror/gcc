/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */

inline void
bar (const float s[5], float z[3][5])
{
  float a = s[0], b = s[1], c = s[2], d = s[3], e = s[4];
  float f = 1.0f / a;
  float u = f * b, v = f * c, w = f * d;
  float p = 0.4f * (e - 0.5f * (b * u + c * v + d * w));
  z[0][3] = b * w;
  z[1][3] = c * w;
  z[2][3] = d * w + p;
}

void
foo (unsigned long n, const float *__restrict u0,
     const float *__restrict u1, const float *__restrict u2,
     const float *__restrict u3, const float *__restrict u4,
     const float *__restrict s0, const float *__restrict s1,
     const float *__restrict s2, float *__restrict t3,
     float *__restrict t4)
{
  unsigned long i;
  for (i = 0; i < n; i++)
    {
      float u[5], f[3][5];
      u[0] = u0[i]; u[1] = u1[i]; u[2] = u2[i]; u[3] = u3[i]; u[4] = u4[i];
      bar (u, f);
      t3[i] = s0[i] * f[0][3] + s1[i] * f[1][3] + s2[i] * f[2][3];
    }
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
