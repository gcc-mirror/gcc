/* { dg-do compile } */
/* { dg-additional-options "-O3 -fcx-limited-range -fno-signed-zeros" } */
/* { dg-require-effective-target vect_complex_add_double } */
/* { dg-add-options arm_v8_3a_complex_neon } */

extern void abort (void);

#define N 16

#define DEF(NAME, TYPE, EXPR)                                  \
  __attribute__((noipa)) void                                  \
  NAME (_Complex TYPE *__restrict d, _Complex TYPE *__restrict c,\
       _Complex TYPE *__restrict a, _Complex TYPE *__restrict b,\
       int n)                                                  \
  {                                                            \
    for (int i = 0; i < n; i++)                                        \
      d[i] = EXPR;                                             \
  }                                                            \
                                                               \
  __attribute__((noipa, optimize ("no-tree-vectorize"))) void  \
  NAME##_ref (_Complex TYPE *__restrict d, _Complex TYPE *__restrict c,\
             _Complex TYPE *__restrict a, _Complex TYPE *__restrict b,\
             int n)                                            \
  {                                                            \
    for (int i = 0; i < n; i++)                                        \
      d[i] = EXPR;                                             \
  }

DEF (fms_f, float, c[i] - a[i] * b[i])
DEF (fms_d, double, c[i] - a[i] * b[i])
DEF (fmsconj_f, float, c[i] - a[i] * ~b[i])
DEF (mul_f, float, a[i] * b[i])

#define CHECK(NAME, TYPE)                                      \
  do {                                                         \
    _Complex TYPE a[N], b[N], c[N], d[N], ref[N];              \
    for (int i = 0; i < N; i++)                                        \
      {                                                                \
       __real__ a[i] = i + 1;                                  \
       __imag__ a[i] = 2 * i + 3;                              \
       __real__ b[i] = 3 * i - 1;                              \
       __imag__ b[i] = i + 5;                                  \
       __real__ c[i] = 100 + i;                                \
       __imag__ c[i] = 200 - i;                                \
      }                                                                \
    NAME (d, c, a, b, N);                                      \
    NAME##_ref (ref, c, a, b, N);                              \
    _Pragma("novect")					       \
    for (int i = 0; i < N; i++)                                        \
      if (__real__ d[i] != __real__ ref[i]                     \
         || __imag__ d[i] != __imag__ ref[i])                  \
       abort ();                                               \
  } while (0)

int
main (void)
{
  CHECK (fms_f, float);
  CHECK (fms_d, double);
  CHECK (fmsconj_f, float);
  CHECK (mul_f, float);
  return 0;
}

/* { dg-final { scan-tree-dump "add new stmt: \[^\n\r]*COMPLEX_FMS \\(" "vect" } } */
/* { dg-final { scan-tree-dump "add new stmt: \[^\n\r]*COMPLEX_FMS_CONJ" "vect" } } */
/* { dg-final { scan-tree-dump "add new stmt: \[^\n\r]*COMPLEX_MUL \\(" "vect" } } */
