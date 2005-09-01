/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */

typedef float __v4sf __attribute__ ((__vector_size__ (16)));
typedef float __m128 __attribute__ ((__vector_size__ (16)));
typedef long long __v2di __attribute__ ((__vector_size__ (16)));

static __inline __m128
_mm_cmpeq_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpeqps ((__v4sf)__A, (__v4sf)__B);
}

static __inline __m128
_mm_setr_ps (float __Z, float __Y, float __X, float __W)
{
  return __extension__ (__m128)(__v4sf){__Z, __Y, __X, __W };
}

static __inline __m128
_mm_and_si128 (__m128 __A, __m128 __B)
{
  return (__m128)__builtin_ia32_pand128 ((__v2di)__A, (__v2di)__B);
}

static __inline __m128
_mm_or_si128 (__m128 __A, __m128 __B)
{
  return (__m128)__builtin_ia32_por128 ((__v2di)__A, (__v2di)__B);
}

typedef union
{
  __m128 xmmi;
  int si[4];
}
__attribute__ ((aligned (16))) um128;

um128 u;

static inline int
sse_max_abs_indexf (float *v, int step, int n)
{
  __m128 m1, mm;
  __m128 mim, mi, msk;
  um128 u, ui;
  int n4, step2, step3;
  mm = __builtin_ia32_andps ((__m128) (__v4sf)
			     { 0.0, v[step], v[step2], v[step3] }
			     , u.xmmi);
  if (n4)
    {
      int i;
      for (i = 0; i < n4; ++i);
      msk = (__m128) _mm_cmpeq_ps (m1, mm);
      mim = _mm_or_si128 (_mm_and_si128 (msk, mi), mim);
    }
  ui.xmmi = (__m128) mim;
  return ui.si[n];
}

static void
sse_swap_rowf (float *r1, float *r2, int n)
{
  int n4 = (n / 4) * 4;
  float *r14end = r1 + n4;
  while (r1 < r14end)
    {
      *r1 = *r2;
      r1++;
    }
}

void
ludcompf (float *m, int nw, int *prow, int n)
{
  int i, s = 0;
  float *pm;
  for (i = 0, pm = m; i < n - 1; ++i, pm += nw)
    {
      int vi = sse_max_abs_indexf (pm + i, nw, n - i);
      float *pt;
      int j;
      if (vi != 0)
	{
	  sse_swap_rowf (pm, pm + vi * nw, nw);
	  swap_index (prow, i, i + vi);
	}
      for (j = i + 1, pt = pm + nw; j < n; ++j, pt += nw)
	sse_add_rowf (pt + i + 1, pm + i + 1, -1.0, n - i - 1);
    }
}
