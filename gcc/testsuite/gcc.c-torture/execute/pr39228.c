/* { dg-add-options ieee } */
/* { dg-skip-if "No Inf/NaN support" { spu-*-* } "*" "" } */

extern void abort (void);

static inline int __attribute__((always_inline)) testf (float b)
{
  float c = 1.01f * b;

  return __builtin_isinff (c);
}

static inline int __attribute__((always_inline)) test (double b)
{
  double c = 1.01 * b;

  return __builtin_isinf (c);
}

static inline int __attribute__((always_inline)) testl (long double b)
{
  long double c = 1.01L * b;

  return __builtin_isinfl (c);
}

int main()
{
  if (testf (__FLT_MAX__) < 1)
    abort ();

  if (test (__DBL_MAX__) < 1)
    abort ();

  if (testl (__LDBL_MAX__) < 1)
    abort ();

  return 0;
}
