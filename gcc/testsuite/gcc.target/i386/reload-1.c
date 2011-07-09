/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-O3 -msse2 -fdump-rtl-csa" } */
/* { dg-skip-if "no stdint" { vxworks_kernel } } */

#include <emmintrin.h>
#include <stdint.h>

typedef __SIZE_TYPE__ size_t;
typedef float vFloat __attribute__ ((__vector_size__ (16)));
typedef double vDouble __attribute__ ((__vector_size__ (16)));
typedef struct buf
{
  void *data;
  unsigned long h;
  unsigned long  w;
  size_t bytes;
} buf;

typedef struct job
{
  struct Job *next;
  void * info;
  long (*func)(struct Job *job);
  long error;
} job;

typedef struct fj
{
    job hd;
    buf src;
    buf dest;
    float g;
    unsigned int flags;
} fj;

static const double r[256], t[256];

long bar (const buf *src, const buf *dest, float g, unsigned int flags)
{
  float *d0 = (float*) src->data;
  float *d1 = (float*) dest->data;
  uintptr_t w = dest->w;
  uintptr_t idx;
  vFloat p0;
  static const vFloat m0;
  static const vDouble p[3], m, b;
  float *sr = d0;
  float *dr = d1;
  for( idx = 0; idx + 8 <= w; idx += 8 )
  {
    vFloat f0 = _mm_loadu_ps (sr);
    vFloat f1 = _mm_loadu_ps (sr + 4);
    sr += 8;
    vFloat fa0 = _mm_andnot_ps (m0, f0);
    vFloat fa1 = _mm_andnot_ps (m0, f1);
    vDouble v0 = _mm_cvtps_pd (fa0);
    vDouble v1 = _mm_cvtps_pd (_mm_movehl_ps (fa0, fa0));
    vDouble v2 = _mm_cvtps_pd (fa1);
    vDouble v3 = _mm_cvtps_pd (_mm_movehl_ps (fa1, fa1));
    vDouble  vi0, vi1, vi2, vi3;
    __m128i b0, b1, b2, b3;
    b0 = _mm_packs_epi32 (_mm_packs_epi32 (b0, b1), _mm_packs_epi32 (b2, b3));
    b1 = _mm_srli_epi64 (b0, 32);
    unsigned int i0 = _mm_cvtsi128_si32 (b0); 
    unsigned int i2 = _mm_cvtsi128_si32 (b1);
    v0 -= _mm_loadh_pd (_mm_load_sd (r + (i0 & 0xff)), r + (i0 >> 16));
    v1 -= _mm_loadh_pd (_mm_load_sd (r + (i2 & 0xff)), r + (i2 >> 16));
    b0 = _mm_unpackhi_epi64 (b0, b0);
    b1 = _mm_unpackhi_epi64 (b1, b1);
    unsigned int i4 = _mm_cvtsi128_si32 (b0);
    unsigned int i6 = _mm_cvtsi128_si32 (b1);
    v2 -= _mm_loadh_pd (_mm_load_sd (r + (i4 & 0xff)), r + (i4 >> 16));
    v3 -= _mm_loadh_pd (_mm_load_sd (r + (i6 & 0xff)), r + (i6 >> 16));
    v0 = p[0] + (p[1] + p[2] * v0) * v0;
    v1 = p[0] + (p[1] + p[2] * v1) * v1;
    v2 = p[0] + (p[1] + p[2] * v2) * v2;
    v3 = p[0] + (p[1] + p[2] * v3) * v3;
    vi0 = (vDouble) _mm_slli_epi64 ((__m128i)((vi0 + b) + m), 52);
    vi1 = (vDouble) _mm_slli_epi64 ((__m128i)((vi1 + b) + m), 52);
    vi2 = (vDouble) _mm_slli_epi64 ((__m128i)((vi2 + b) + m), 52);
    vi3 = (vDouble) _mm_slli_epi64 ((__m128i)((vi3 + b) + m), 52);
    vi0 *= _mm_loadh_pd (_mm_load_sd (t + (i0 & 0xff)), t + (i0 >> 16));
    vi1 *= _mm_loadh_pd (_mm_load_sd (t + (i2 & 0xff)), t + (i2 >> 16));
    vi2 *= _mm_loadh_pd (_mm_load_sd (t + (i4 & 0xff)), t + (i4 >> 16));
    vi3 *= _mm_loadh_pd (_mm_load_sd (t + (i6 & 0xff)), t + (i6 >> 16));
    v0 *= vi0;
    v1 *= vi1;
    v2 *= vi2;
    v3 *= vi3;
    vFloat r0 = _mm_movelh_ps (_mm_cvtpd_ps( v0 ), _mm_cvtpd_ps (v1));
    vFloat r1 = _mm_movelh_ps (_mm_cvtpd_ps( v2 ), _mm_cvtpd_ps (v3));
    vFloat z0 = _mm_cmpeq_ps (f0, _mm_setzero_ps());
    vFloat z1 = _mm_cmpeq_ps (f1, _mm_setzero_ps());
    r0 = _mm_andnot_ps (z0, r0);
    r1 = _mm_andnot_ps (z1, r1);
    z0 = _mm_and_ps (z0, p0);
    z1 = _mm_and_ps (z1, p0);
    r0 = _mm_or_ps (r0, z0);
    r1 = _mm_or_ps (r1, z1);
    _mm_storeu_ps (dr, r0);
    _mm_storeu_ps (dr + 4, r1);
    dr += 8;
  }
  return 0;
}

long foo (job *j )
{
  fj *jd = (fj*) j;
  return bar (&jd->src, &jd->dest, jd->g, jd->flags);
}

/* { dg-final { scan-rtl-dump-not "deleted 1 dead insns" "csa" } } */
/* { dg-final { cleanup-rtl-dump "csa" } } */
