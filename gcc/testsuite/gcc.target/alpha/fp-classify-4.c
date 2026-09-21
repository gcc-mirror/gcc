/* Without -mieee the isnan, isinf, isfinite and isnormal built-ins must
   classify subnormals, infinities and NaNs correctly, without trapping.  */

/* { dg-do run } */
/* { dg-options "-O2 -fno-inline" } */

enum { NAN_, INF_, NORMAL_, SUBNORMAL_, ZERO_ };

static void
t_f (float x, int class)
{
  if (__builtin_isnan (x) != (class == NAN_))
    __builtin_abort ();
  if (__builtin_isinf (x) != (class == INF_))
    __builtin_abort ();
  if (__builtin_isfinite (x) != (class != NAN_ && class != INF_))
    __builtin_abort ();
  if (__builtin_isnormal (x) != (class == NORMAL_))
    __builtin_abort ();
}

static void
t_d (double x, int class)
{
  if (__builtin_isnan (x) != (class == NAN_))
    __builtin_abort ();
  if (__builtin_isinf (x) != (class == INF_))
    __builtin_abort ();
  if (__builtin_isfinite (x) != (class != NAN_ && class != INF_))
    __builtin_abort ();
  if (__builtin_isnormal (x) != (class == NORMAL_))
    __builtin_abort ();
}

/* Keep the operands away from the constant folders.  */
volatile float vf[] = {
  0.0f, -0.0f, 1.0f, -1.0f, __FLT_MIN__, __FLT_MAX__, -__FLT_MAX__,
  __FLT_DENORM_MIN__, -__FLT_DENORM_MIN__, __FLT_MIN__ / 2,
  __builtin_inff (), -__builtin_inff (), __builtin_nanf (""),
  -__builtin_nanf ("")
};

volatile double vd[] = {
  0.0, -0.0, 1.0, -1.0, __DBL_MIN__, __DBL_MAX__, -__DBL_MAX__,
  __DBL_DENORM_MIN__, -__DBL_DENORM_MIN__, __DBL_MIN__ / 2,
  __builtin_inf (), -__builtin_inf (), __builtin_nan (""),
  -__builtin_nan ("")
};

const int classes[] = {
  ZERO_, ZERO_, NORMAL_, NORMAL_, NORMAL_, NORMAL_, NORMAL_,
  SUBNORMAL_, SUBNORMAL_, SUBNORMAL_,
  INF_, INF_, NAN_, NAN_
};

int
main ()
{
  for (unsigned i = 0; i < sizeof classes / sizeof classes[0]; i++)
    {
      t_f (vf[i], classes[i]);
      t_d (vd[i], classes[i]);
    }
  return 0;
}
