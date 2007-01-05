/* PR c/30360 */
/* { dg-do run { target i?86-*-linux* x86_64-*-linux* ia64-*-linux* s390*-*-linux* } } */
/* { dg-options "-O2 -std=gnu99" } */

#define I	(__extension__ 1.0iF)
#define H(x)	asm ("" : "=m" (x) : "m" (x))
extern void abort (void);

int
main (void)
{
  _Complex double a = 1.0 + 1.0 * I, b = 0.0, c;
  H (a);
  H (b);
  c = a / b;
  if (!__builtin_isinf (__real__ c) && !__builtin_isinf (__imag__ c))
    abort ();
  a = 0.0;
  H (a);
  H (b);
  c = a / b;
  if (!__builtin_isnan (__real__ c) || !__builtin_isnan (__imag__ c))
    abort ();
  return 0;
}
