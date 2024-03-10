/* PR tree-optimization/107547 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-add-options float16 } */

int x;

void
foo (void)
{
#define TEST(...) \
  __builtin_acos##__VA_ARGS__ (x);	\
  __builtin_asin##__VA_ARGS__ (x);	\
  __builtin_acosh##__VA_ARGS__ (x);	\
  __builtin_atanh##__VA_ARGS__ (x);	\
  __builtin_cosh##__VA_ARGS__ (x);	\
  __builtin_sinh##__VA_ARGS__ (x);	\
  __builtin_log##__VA_ARGS__ (x);	\
  __builtin_log2##__VA_ARGS__ (x);	\
  __builtin_log10##__VA_ARGS__ (x);	\
  __builtin_log1p##__VA_ARGS__ (x);	\
  __builtin_exp##__VA_ARGS__ (x);	\
  __builtin_expm1##__VA_ARGS__ (x);	\
  __builtin_exp2##__VA_ARGS__ (x);	\
  __builtin_sqrt##__VA_ARGS__ (x)
  TEST (f);
  TEST ();
  TEST (l);
#ifdef __FLT16_MAX__
  TEST (f16);
#endif
#ifdef __FLT32_MAX__
  TEST (f32);
#endif
#ifdef __FLT64_MAX__
  TEST (f64);
#endif
#ifdef __FLT128_MAX__
  TEST (f128);
#endif
}
