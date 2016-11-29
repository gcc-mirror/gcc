/* Test infinities convert to __float128 infinity.  Bug 77265.  */
/* { dg-do run } */
/* { dg-require-effective-target __float128 } */
/* { dg-require-effective-target base_quadfloat_support } */
/* { dg-add-options __float128 } */

extern void abort (void);
extern void exit (int);

volatile float finf = __builtin_inff ();
volatile double dinf = __builtin_inf ();
volatile long double ldinf = __builtin_infl ();
volatile float nfinf = -__builtin_inff ();
volatile double ndinf = -__builtin_inf ();
volatile long double nldinf = -__builtin_infl ();

int
main (void)
{
  volatile __float128 r;
  r = (__float128) finf;
  if (!__builtin_isinf (r) || __builtin_signbit (r) != 0)
    abort ();
  r = (__float128) dinf;
  if (!__builtin_isinf (r) || __builtin_signbit (r) != 0)
    abort ();
  r = (__float128) ldinf;
  if (!__builtin_isinf (r) || __builtin_signbit (r) != 0)
    abort ();
  r = (__float128) nfinf;
  if (!__builtin_isinf (r) || __builtin_signbit (r) == 0)
    abort ();
  r = (__float128) ndinf;
  if (!__builtin_isinf (r) || __builtin_signbit (r) == 0)
    abort ();
  r = (__float128) nldinf;
  if (!__builtin_isinf (r) || __builtin_signbit (r) == 0)
    abort ();
  exit (0);
}
