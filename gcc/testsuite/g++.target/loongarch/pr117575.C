/* { dg-do compile } */
/* { dg-options "-O3 -mlasx" } */

typedef long unsigned int size_t;
typedef unsigned char simde__mmask8;
typedef double simde_float64;
typedef simde_float64 simde__m512d __attribute__ ((__aligned__ ((64))))
__attribute__ ((__vector_size__ (64))) __attribute__ ((__may_alias__));
typedef simde_float64 simde__m256d __attribute__ ((__aligned__ ((32))))
__attribute__ ((__vector_size__ (32))) __attribute__ ((__may_alias__));
simde__m512d simde_mm512_set_pd (simde_float64 e7, simde_float64 e6,
                                 simde_float64 e5, simde_float64 e4,
                                 simde_float64 e3, simde_float64 e2,
                                 simde_float64 e1, simde_float64 e0);
simde__m256d simde_mm256_maskz_mov_pd (simde__mmask8 k, simde__m256d a);
int simde_test_x86_assert_equal_f64x4_ (simde__m256d a, simde__m256d b);

typedef union
{

  __attribute__ ((__aligned__ ((32)))) simde_float64 f64
      __attribute__ ((__vector_size__ (32))) __attribute__ ((__may_alias__));
} simde__m256d_private;
__attribute__ ((__always_inline__)) inline static simde__m256d
simde__m256d_from_private (simde__m256d_private v)
{
  simde__m256d r;
  __builtin_memcpy (&r, &v, sizeof (r));
  return r;
}
simde__m256d
simde_mm256_set_pd (simde_float64 e3, simde_float64 e2, simde_float64 e1,
                    simde_float64 e0)
{

  simde__m256d_private r_;

  r_.f64[0] = e0;
  r_.f64[1] = e1;
  r_.f64[2] = e2;
  r_.f64[3] = e3;

  return simde__m256d_from_private (r_);
}

simde__m256d simde_mm512_extractf64x4_pd (simde__m512d a, int imm8);
int
test_simde_mm512_maskz_extractf64x4_pd (void)
{
  const struct
  {
    simde__mmask8 k;
    simde__m512d a;
    simde__m256d r0;
    simde__m256d r1;
  } test_vec[2] = {
    { 21,
      simde_mm512_set_pd (-139.11, -172.36, -268.86, 393.53, -71.72, 36.69,
                          98.47, -135.52),
      simde_mm256_set_pd (0.00, 36.69, 0.00, -135.52),
      simde_mm256_set_pd (0.00, -172.36, 0.00, 393.53) },
    { 150,
      simde_mm512_set_pd (-556.90, 522.06, 160.98, -932.28, 391.82, 600.12,
                          -569.99, -491.12),
      simde_mm256_set_pd (0.00, 600.12, -569.99, 0.00),
      simde_mm256_set_pd (0.00, 522.06, 160.98, 0.00) },
  };

  for (size_t i = 0; i < (sizeof (test_vec) / sizeof (test_vec[0])); i++)
    {
      simde__m256d r;
      r = simde_mm256_maskz_mov_pd (
          test_vec[i].k, simde_mm512_extractf64x4_pd (test_vec[i].a, 0));
      if (simde_test_x86_assert_equal_f64x4_ (r, test_vec[i].r0))
        {
          return 1;
        }
    }

  return 0;
}
