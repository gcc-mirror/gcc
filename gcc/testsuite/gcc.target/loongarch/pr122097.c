/* { dg-do compile } */
/* { dg-options "-O3 -mabi=lp64d -mlsx" } */
/* { dg-final { scan-assembler "vbitseti\.d\t\\\$vr\[0-9\]+,\\\$vr\[0-9\]+,63" } } */

typedef long unsigned int size_t;
typedef unsigned char simde__mmask8;
typedef long simde__m128i __attribute__ ((__aligned__ ((16))))
__attribute__ ((__vector_size__ (16))) __attribute__ ((__may_alias__));
typedef union
{

  __attribute__ ((__aligned__ ((16)))) long i64
      __attribute__ ((__vector_size__ (16))) __attribute__ ((__may_alias__));
} simde__m128i_private;
typedef double simde_float64;
typedef simde_float64 simde__m128d __attribute__ ((__aligned__ ((16))))
__attribute__ ((__vector_size__ (16))) __attribute__ ((__may_alias__));
typedef long int int_fast32_t;
typedef union
{

  __attribute__ ((__aligned__ ((16)))) int_fast32_t i32f
      __attribute__ ((__vector_size__ (16))) __attribute__ ((__may_alias__));
  __attribute__ ((__aligned__ ((16)))) long i64
      __attribute__ ((__vector_size__ (16))) __attribute__ ((__may_alias__));
  __attribute__ ((__aligned__ ((16)))) simde_float64 f64
      __attribute__ ((__vector_size__ (16))) __attribute__ ((__may_alias__));
} simde__m128d_private;
__attribute__ ((__always_inline__)) inline static simde__m128d
simde__m128d_from_private (simde__m128d_private v)
{
  simde__m128d r;
  __builtin_memcpy (&r, &v, sizeof (r));
  return r;
}

__attribute__ ((__always_inline__)) inline static simde__m128d
simde_mm_set_pd (simde_float64 e1, simde_float64 e0)
{

  simde__m128d_private r_;
  r_.f64[0] = e0;
  r_.f64[1] = e1;

  return simde__m128d_from_private (r_);
}
__attribute__ ((__always_inline__)) inline static simde__m128i
simde_mm_castpd_si128 (simde__m128d a)
{
  simde__m128i r;
  __builtin_memcpy (&r, &a, sizeof (a));
  return r;
}

__attribute__ ((__always_inline__)) inline static simde__m128i
simde__m128i_from_private (simde__m128i_private v)
{
  simde__m128i r;
  __builtin_memcpy (&r, &v, sizeof (r));
  return r;
}

__attribute__ ((__always_inline__)) inline static simde__m128i_private
simde__m128i_to_private (simde__m128i v)
{
  simde__m128i_private r;
  __builtin_memcpy (&r, &v, sizeof (r));
  return r;
}
__attribute__ ((__always_inline__)) inline static simde__m128d
simde_mm_castsi128_pd (simde__m128i a)
{
  simde__m128d r;
  __builtin_memcpy (&r, &a, sizeof (a));
  return r;
}

__attribute__ ((__always_inline__)) inline static simde__m128i
simde_mm_mask_mov_epi64 (simde__m128i src, simde__mmask8 k, simde__m128i a)
{

  simde__m128i_private src_ = simde__m128i_to_private (src),
                       a_ = simde__m128i_to_private (a), r_;

  for (size_t i = 0; i < (sizeof (r_.i64) / sizeof (r_.i64[0])); i++)
    {
      r_.i64[i] = ((k >> i) & 1) ? a_.i64[i] : src_.i64[i];
    }

  return simde__m128i_from_private (r_);
}

__attribute__ ((__always_inline__)) inline static simde__m128d
simde_mm_mask_mov_pd (simde__m128d src, simde__mmask8 k, simde__m128d a)
{
  return simde_mm_castsi128_pd (simde_mm_mask_mov_epi64 (
      simde_mm_castpd_si128 (src), k, simde_mm_castpd_si128 (a)));
}

static double
simde_test_f64_precision_to_slop (int precision)
{
  return __builtin_expect (!!(precision == 0x7fffffff), 0)
             ? 0.0
             : __builtin_pow (10.0, -((double)(precision)));
}
__attribute__ ((__always_inline__)) inline static void
simde_mm_storeu_pd (simde_float64 *mem_addr, simde__m128d a)
{

  __builtin_memcpy (mem_addr, &a, sizeof (a));
}
int simde_test_equal_f64 (simde_float64 a, simde_float64 b,
                          simde_float64 slop);
void simde_test_debug_printf_ (const char *format, ...);
static int
simde_assert_equal_vf64_ (size_t vec_len, simde_float64 const a[(vec_len)],
                          simde_float64 const b[(vec_len)], simde_float64 slop,
                          const char *filename, int line, const char *astr,
                          const char *bstr)
{
  for (size_t i = 0; i < vec_len; i++)
    {
      if (__builtin_expect (!!(!simde_test_equal_f64 (a[i], b[i], slop)), 0))
        {
          simde_test_debug_printf_ (
              "%s:%d: assertion failed: %s[%zu] ~= %s[%zu] (%f ~= %f)\n",
              filename, line, astr, i, bstr, i, ((double)(a[i])),
              ((double)(b[i])));
          return 1;
        }
    }
  return 0;
}
static int
simde_test_x86_assert_equal_f64x2_ (simde__m128d a, simde__m128d b,
                                    simde_float64 slop, const char *filename,
                                    int line, const char *astr,
                                    const char *bstr)
{
  simde_float64 a_[sizeof (a) / sizeof (simde_float64)],
      b_[sizeof (a) / sizeof (simde_float64)];
  simde_mm_storeu_pd (a_, a);
  simde_mm_storeu_pd (b_, b);
  return simde_assert_equal_vf64_ (sizeof (a_) / sizeof (a_[0]), a_, b_, slop,
                                   filename, line, astr, bstr);
}
__attribute__ ((__always_inline__)) inline static simde__m128d_private
simde__m128d_to_private (simde__m128d v)
{
  simde__m128d_private r;
  __builtin_memcpy (&r, &v, sizeof (r));
  return r;
}
__attribute__ ((__always_inline__)) inline static simde__m128d
simde_mm_min_pd (simde__m128d a, simde__m128d b)
{

  simde__m128d_private r_, a_ = simde__m128d_to_private (a),
                           b_ = simde__m128d_to_private (b);

  for (size_t i = 0; i < (sizeof (r_.f64) / sizeof (r_.f64[0])); i++)
    {
      r_.f64[i] = (a_.f64[i] < b_.f64[i]) ? a_.f64[i] : b_.f64[i];
    }

  return simde__m128d_from_private (r_);
}

__attribute__ ((__always_inline__)) inline static simde__m128d
simde_mm_max_pd (simde__m128d a, simde__m128d b)
{

  simde__m128d_private r_, a_ = simde__m128d_to_private (a),
                           b_ = simde__m128d_to_private (b);

  for (size_t i = 0; i < (sizeof (r_.f64) / sizeof (r_.f64[0])); i++)
    {
      r_.f64[i] = (a_.f64[i] > b_.f64[i]) ? a_.f64[i] : b_.f64[i];
    }

  return simde__m128d_from_private (r_);
}

__attribute__ ((__always_inline__)) inline static simde__m128d
simde_x_mm_abs_pd (simde__m128d a)
{

  simde__m128d_private r_, a_ = simde__m128d_to_private (a);
  for (size_t i = 0; i < (sizeof (r_.f64) / sizeof (r_.f64[0])); i++)
    {
      r_.f64[i] = __builtin_fabs (a_.f64[i]);
    }

  return simde__m128d_from_private (r_);
}
__attribute__ ((__always_inline__)) inline static simde__m128d
simde_mm_cmple_pd (simde__m128d a, simde__m128d b)
{

  simde__m128d_private r_, a_ = simde__m128d_to_private (a),
                           b_ = simde__m128d_to_private (b);

  r_.i64 = ((__typeof__ (r_.i64))((a_.f64 <= b_.f64)));
  return simde__m128d_from_private (r_);
}

__attribute__ ((__always_inline__)) inline static simde__m128d
simde_x_mm_select_pd (simde__m128d a, simde__m128d b, simde__m128d mask)
{
  simde__m128d_private r_, a_ = simde__m128d_to_private (a),
                           b_ = simde__m128d_to_private (b),
                           mask_ = simde__m128d_to_private (mask);

  r_.i64 = a_.i64 ^ ((a_.i64 ^ b_.i64) & mask_.i64);
  return simde__m128d_from_private (r_);
}
simde__m128d simde_mm_cmpge_pd (simde__m128d a, simde__m128d b);

simde__m128d
simde_x_mm_copysign_pd (simde__m128d dest, simde__m128d src)
{
  simde__m128d_private r_, dest_ = simde__m128d_to_private (dest),
                           src_ = simde__m128d_to_private (src);
  for (size_t i = 0; i < (sizeof (r_.f64) / sizeof (r_.f64[0])); i++)
    {
      r_.f64[i] = __builtin_copysign (dest_.f64[i], src_.f64[i]);
    }

  return simde__m128d_from_private (r_);
}
simde__m128d simde_mm_or_pd (simde__m128d a, simde__m128d b);

simde__m128d simde_mm_set1_pd (simde_float64 a);

__attribute__ ((__always_inline__)) inline static simde__m128d
simde_mm_range_pd (simde__m128d a, simde__m128d b, int imm8)
{
  simde__m128d r;

  r = simde_x_mm_select_pd (
      b, a, simde_mm_cmple_pd (simde_x_mm_abs_pd (a), simde_x_mm_abs_pd (b)));

  r = simde_x_mm_copysign_pd (r, a);

  return r;
}
int
test_simde_mm_mask_range_pd (void)
{

  simde__m128d src, a, b, e, r;

  src = simde_mm_set_pd (-2.92, -85.39);
  a = simde_mm_set_pd (-47.59, -122.31);
  b = simde_mm_set_pd (877.42, 69.15);
  e = simde_mm_set_pd (-47.59, -69.15);
  r = simde_mm_mask_mov_pd (src, 143, simde_mm_range_pd (a, b, 2));
  do
    {
      if (simde_test_x86_assert_equal_f64x2_ (
              r, e, simde_test_f64_precision_to_slop (1),
              "../test/x86/avx512/range.c", 1454, "r", "e"))
        {
          return 1;
        }
    }
  while (0);

  return 0;
}
