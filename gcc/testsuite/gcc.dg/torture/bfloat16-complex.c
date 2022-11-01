/* Test __bf16 complex arithmetic.  */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-add-options bfloat16 } */
/* { dg-require-effective-target bfloat16_runtime } */

extern void exit (int);
extern void abort (void);

volatile __bf16 a = 1.0bf16;
typedef _Complex float __cbf16 __attribute__((__mode__(__BC__)));
volatile __cbf16 b = __builtin_complex (2.0bf16, 3.0bf16);
volatile __cbf16 c = __builtin_complex (2.0bf16, 3.0bf16);
volatile __cbf16 d = __builtin_complex (2.0bf16, 3.0bf16);

__cbf16
fn (__cbf16 arg)
{
  return arg / 4;
}

int
main (void)
{
  volatile __cbf16 r;
  if (b != c)
    abort ();
  if (b != d)
    abort ();
  r = a + b;
  if (__real__ r != 3.0bf16 || __imag__ r != 3.0bf16)
    abort ();
  r += d;
  if (__real__ r != 5.0bf16 || __imag__ r != 6.0bf16)
    abort ();
  r -= a;
  if (__real__ r != 4.0bf16 || __imag__ r != 6.0bf16)
    abort ();
  r /= (a + a);
  if (__real__ r != 2.0bf16 || __imag__ r != 3.0bf16)
    abort ();
  r *= (a + a);
  if (__real__ r != 4.0bf16 || __imag__ r != 6.0bf16)
    abort ();
  r -= b;
  if (__real__ r != 2.0bf16 || __imag__ r != 3.0bf16)
    abort ();
  r *= r;
  if (__real__ r != -5.0bf16 || __imag__ r != 12.0bf16)
    abort ();
  /* Division may not be exact, so round result before comparing.  */
  r /= b;
  r += __builtin_complex (100.0bf16, 100.0bf16);
  r -= __builtin_complex (100.0bf16, 100.0bf16);
  if (r != b)
    abort ();
  r = fn (r);
  if (__real__ r != 0.5bf16 || __imag__ r != 0.75bf16)
    abort ();
  exit (0);
}
