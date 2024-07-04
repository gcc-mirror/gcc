/* Check that calls to the vectorized math functions are actually emitted.  */

/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fno-math-errno -mstack-size=3000000 -fdump-tree-vect" } */
/* The 'scan-tree-dump' directives are specific to 64-lane vectors.
   { dg-additional-options --param=gcn-preferred-vectorization-factor=64 } */

#undef PRINT_RESULT
#define VERBOSE 0
#define EARLY_EXIT 1

#include <math.h>
#include <stdlib.h>

#ifdef PRINT_RESULT
  #include <stdio.h>
  #define PRINTF printf
#else
  static void null_printf (const char *f, ...) { }

  #define PRINTF null_printf
#endif

#define N 512
#define EPSILON_float 1e-5
#define EPSILON_double 1e-10

static int failed = 0;

int deviation_float (float x, float y)
{
  union {
    float f;
    unsigned u;
  } u, v;

  u.f = x;
  v.f = y;

  unsigned mask = 0x80000000U; 
  int i;

  for (i = 32; i > 0; i--)
    if ((u.u ^ v.u) & mask)
      break;
    else
      mask >>= 1;

  return i;
}

int deviation_double (double x, double y)
{
  union {
    double d;
    unsigned long long u;
  } u, v;

  u.d = x;
  v.d = y;

  unsigned long long mask = 0x8000000000000000ULL;
  int i;

  for (i = 64; i > 0; i--)
    if ((u.u ^ v.u) & mask)
      break;
    else
      mask >>= 1;

  return i;
}

#define TEST_FUN(TFLOAT, LOW, HIGH, FUN) \
__attribute__((optimize("no-tree-vectorize"))) \
__attribute__((optimize("no-unsafe-math-optimizations"))) \
void check_##FUN (TFLOAT res[N], TFLOAT a[N]) \
{ \
  int failed = 0; \
  for (int i = 0; i < N; i++) { \
    TFLOAT expected = FUN (a[i]); \
    TFLOAT diff = __builtin_fabs (expected - res[i]); \
    int deviation = deviation_##TFLOAT (expected, res[i]); \
    int fail = isnan (res[i]) != isnan (expected) \
               || isinf (res[i]) != isinf (expected) \
               || (diff > EPSILON_##TFLOAT && deviation > 10); \
    failed |= fail; \
    if (VERBOSE || fail) \
      PRINTF (#FUN "(%f) = %f, expected = %f, diff = %f, deviation = %d %s\n", \
              a[i], res[i], expected, diff, deviation, fail ? "(!)" : ""); \
    if (EARLY_EXIT && fail) \
      exit (1); \
  } \
} \
void test_##FUN (void) \
{ \
  TFLOAT res[N], a[N]; \
  for (int i = 0; i < N; i++) \
    a[i] = LOW + ((HIGH - LOW) / N) * i; \
  for (int i = 0; i < N; i++) \
    res[i] = FUN (a[i]); \
  check_##FUN (res, a); \
}\
test_##FUN ();

#define TEST_FUN2(TFLOAT, LOW1, HIGH1, LOW2, HIGH2, FUN) \
__attribute__((optimize("no-tree-vectorize"))) \
__attribute__((optimize("no-unsafe-math-optimizations"))) \
void check_##FUN (TFLOAT res[N], TFLOAT a[N], TFLOAT b[N]) \
{ \
  int failed = 0; \
  for (int i = 0; i < N; i++) { \
    TFLOAT expected = FUN (a[i], b[i]); \
    TFLOAT diff = __builtin_fabs (expected - res[i]); \
    int deviation = deviation_##TFLOAT (expected, res[i]); \
    int fail = isnan (res[i]) != isnan (expected) \
               || isinf (res[i]) != isinf (expected) \
               || (diff > EPSILON_##TFLOAT && deviation > 10); \
    failed |= fail; \
    if (VERBOSE || fail) \
      PRINTF (#FUN "(%f,%f) = %f, expected = %f, diff = %f, deviation = %d %s\n", \
              a[i], b[i], res[i], expected, diff, deviation, fail ? "(!)" : ""); \
    if (EARLY_EXIT && fail) \
      exit (1); \
  } \
} \
void test_##FUN (void) \
{ \
  TFLOAT res[N], a[N], b[N]; \
  for (int i = 0; i < N; i++) { \
    a[i] = LOW1 + ((HIGH1 - LOW1) / N) * i; \
    b[i] = LOW2 + ((HIGH2 - LOW2) / N) * i; \
  } \
  for (int i = 0; i < N; i++) \
    res[i] = FUN (a[i], b[i]); \
  check_##FUN (res, a, b); \
}\
test_##FUN ();

int main (void)
{
  TEST_FUN (float, -1.1, 1.1, acosf); /* { dg-final { scan-tree-dump "v64sf_acosf" "vect" } }*/
  TEST_FUN (float, -10, 10, acoshf); /* { dg-final { scan-tree-dump "v64sf_acoshf" "vect" } }*/
  TEST_FUN (float, -1.1, 1.1, asinf); /* { dg-final { scan-tree-dump "v64sf_asinf" "vect" } }*/
  TEST_FUN (float, -10, 10, asinhf); /* { dg-final { scan-tree-dump "v64sf_asinhf" "vect" } }*/
  TEST_FUN (float, -1.1, 1.1, atanf); /* { dg-final { scan-tree-dump "v64sf_atanf" "vect" } }*/
  TEST_FUN2 (float, -2.0, 2.0, 2.0, -2.0, atan2f); /* { dg-final { scan-tree-dump "v64sf_atan2f" "vect" } }*/
  TEST_FUN (float, -2.0, 2.0, atanhf); /* { dg-final { scan-tree-dump "v64sf_atanhf" "vect" } }*/
  TEST_FUN2 (float, -10.0, 10.0, 5.0, -15.0, copysignf); /* { dg-final { scan-tree-dump "v64sf_copysignf" "vect" } }*/
  TEST_FUN (float, -3.14159265359, 3.14159265359, cosf); /* { dg-final { scan-tree-dump "v64sf_cosf" "vect" } }*/
  TEST_FUN (float, -3.14159265359, 3.14159265359, coshf); /* { dg-final { scan-tree-dump "v64sf_coshf" "vect" } }*/
  TEST_FUN (float, -10.0, 10.0, erff);  /* { dg-final { scan-tree-dump "v64sf_erff" "vect" } }*/
  TEST_FUN (float, -10.0, 10.0, expf); /* { dg-final { scan-tree-dump "v64sf_expf" "vect" } }*/
  TEST_FUN (float, -10.0, 10.0, exp2f); /* { dg-final { scan-tree-dump "v64sf_exp2f" "vect" } }*/
  TEST_FUN2 (float, -10.0, 10.0, 100.0, -25.0, fmodf); /* { dg-final { scan-tree-dump "v64sf_fmodf" "vect" } }*/
  TEST_FUN (float, -10.0, 10.0, gammaf); /* { dg-final { scan-tree-dump "v64sf_gammaf" "vect" { xfail *-*-*} } }*/
  TEST_FUN2 (float, -10.0, 10.0, 15.0, -5.0,hypotf); /* { dg-final { scan-tree-dump "v64sf_hypotf" "vect" } }*/
  TEST_FUN (float, -10.0, 10.0, lgammaf); /* { dg-final { scan-tree-dump "v64sf_lgammaf" "vect" { xfail *-*-*} } }*/
  TEST_FUN (float, -1.0, 50.0, logf); /* { dg-final { scan-tree-dump "v64sf_logf" "vect" } }*/
  TEST_FUN (float, -1.0, 500.0, log10f); /* { dg-final { scan-tree-dump "v64sf_log10f" "vect" } }*/
  TEST_FUN (float, -1.0, 64.0, log2f); /* { dg-final { scan-tree-dump "v64sf_log2f" "vect" } }*/
  TEST_FUN2 (float, -100.0, 100.0, 100.0, -100.0, powf); /* { dg-final { scan-tree-dump "v64sf_powf" "vect" } }*/
  TEST_FUN2 (float, -50.0, 100.0, -2.0, 40.0, remainderf); /* { dg-final { scan-tree-dump "v64sf_remainderf" "vect" } }*/
  TEST_FUN (float, -50.0, 50.0, rintf);  /* { dg-final { scan-tree-dump "v64sf_rintf" "vect" } }*/
  TEST_FUN2 (float, -50.0, 50.0, -10.0, 32.0, __builtin_scalbf); /* { dg-final { scan-tree-dump "v64sf_scalbf" "vect" } }*/
  TEST_FUN (float, -10.0, 10.0, __builtin_significandf); /* { dg-final { scan-tree-dump "v64sf_significandf" "vect" } }*/
  TEST_FUN (float, -3.14159265359, 3.14159265359, sinf); /* { dg-final { scan-tree-dump "v64sf_sinf" "vect" } }*/
  TEST_FUN (float, -3.14159265359, 3.14159265359, sinhf); /* { dg-final { scan-tree-dump "v64sf_sinhf" "vect" } }*/
  TEST_FUN (float, -0.1, 10000.0, sqrtf); /* { dg-final { scan-tree-dump "v64sf_sqrtf" "vect" } }*/
  TEST_FUN (float, -5.0, 5.0, tanf); /* { dg-final { scan-tree-dump "v64sf_tanf" "vect" } }*/
  TEST_FUN (float, -3.14159265359, 3.14159265359, tanhf); /* { dg-final { scan-tree-dump "v64sf_tanhf" "vect" } }*/
  TEST_FUN (float, -10.0, 10.0, tgammaf); /* { dg-final { scan-tree-dump "v64sf_tgammaf" "vect" } }*/

  TEST_FUN (double, -1.1, 1.1, acos); /* { dg-final { scan-tree-dump "v64df_acos" "vect" } }*/
  TEST_FUN (double, -10, 10, acosh); /* { dg-final { scan-tree-dump "v64df_acosh" "vect" } }*/
  TEST_FUN (double, -1.1, 1.1, asin); /* { dg-final { scan-tree-dump "v64df_asin" "vect" } }*/
  TEST_FUN (double, -10, 10, asinh); /* { dg-final { scan-tree-dump "v64df_asinh" "vect" } }*/
  TEST_FUN (double, -1.1, 1.1, atan); /* { dg-final { scan-tree-dump "v64df_atan" "vect" } }*/
  TEST_FUN2 (double, -2.0, 2.0, 2.0, -2.0, atan2); /* { dg-final { scan-tree-dump "v64df_atan2" "vect" } }*/
  TEST_FUN (double, -2.0, 2.0, atanh); /* { dg-final { scan-tree-dump "v64df_atanh" "vect" } }*/
  TEST_FUN2 (double, -10.0, 10.0, 5.0, -15.0, copysign); /* { dg-final { scan-tree-dump "v64df_copysign" "vect" } }*/
  TEST_FUN (double, -3.14159265359, 3.14159265359, cos); /* { dg-final { scan-tree-dump "v64df_cos" "vect" } }*/
  TEST_FUN (double, -3.14159265359, 3.14159265359, cosh); /* { dg-final { scan-tree-dump "v64df_cosh" "vect" } }*/
  TEST_FUN (double, -10.0, 10.0, erf); /* { dg-final { scan-tree-dump "v64df_erf" "vect" } }*/
  TEST_FUN (double, -10.0, 10.0, exp); /* { dg-final { scan-tree-dump "v64df_exp" "vect" } }*/
  TEST_FUN (double, -10.0, 10.0, exp2); /* { dg-final { scan-tree-dump "v64df_exp2" "vect" } }*/
  TEST_FUN2 (double, -10.0, 10.0, 100.0, -25.0, fmod); /* { dg-final { scan-tree-dump "v64df_fmod" "vect" } }*/
  TEST_FUN (double, -10.0, 10.0, gamma); /* { dg-final { scan-tree-dump "v64df_gamma" "vect" { xfail *-*-*} } }*/
  TEST_FUN2 (double, -10.0, 10.0, 15.0, -5.0, hypot); /* { dg-final { scan-tree-dump "v64df_hypot" "vect" } }*/
  TEST_FUN (double, -10.0, 10.0, lgamma); /* { dg-final { scan-tree-dump "v64df_lgamma" "vect" { xfail *-*-*} } }*/
  TEST_FUN (double, -1.0, 50.0, log); /* { dg-final { scan-tree-dump "v64df_log" "vect" } }*/
  TEST_FUN (double, -1.0, 500.0, log10); /* { dg-final { scan-tree-dump "v64df_log10" "vect" } }*/
  TEST_FUN (double, -1.0, 64.0, log2); /* { dg-final { scan-tree-dump "v64df_log2" "vect" { xfail *-*-*} } }*/
  TEST_FUN2 (double, -100.0, 100.0, 100.0, -100.0, pow); /* { dg-final { scan-tree-dump "v64df_pow" "vect" } }*/
  TEST_FUN2 (double, -50.0, 100.0, -2.0, 40.0, remainder); /* { dg-final { scan-tree-dump "v64df_remainder" "vect" } }*/
  TEST_FUN (double, -50.0, 50.0, rint); /* { dg-final { scan-tree-dump "v64df_rint" "vect" } }*/
  TEST_FUN2 (double, -50.0, 50.0, -10.0, 32.0, __builtin_scalb); /* { dg-final { scan-tree-dump "v64df_scalb" "vect" } }*/
  TEST_FUN (double, -10.0, 10.0, __builtin_significand); /* { dg-final { scan-tree-dump "v64df_significand" "vect" } }*/
  TEST_FUN (double, -3.14159265359, 3.14159265359, sin); /* { dg-final { scan-tree-dump "v64df_sin" "vect" } }*/
  TEST_FUN (double, -3.14159265359, 3.14159265359, sinh); /* { dg-final { scan-tree-dump "v64df_sinh" "vect" } }*/
  TEST_FUN (double, -0.1, 10000.0, sqrt); /* { dg-final { scan-tree-dump "v64df_sqrt" "vect" } }*/
  TEST_FUN (double, -5.0, 5.0, tan); /* { dg-final { scan-tree-dump "v64df_tan" "vect" } }*/
  TEST_FUN (double, -3.14159265359, 3.14159265359, tanh); /* { dg-final { scan-tree-dump "v64df_tanh" "vect" } }*/
  TEST_FUN (double, -10.0, 10.0, tgamma); /* { dg-final { scan-tree-dump "v64df_tgamma" "vect" } }*/

  return failed;
}
