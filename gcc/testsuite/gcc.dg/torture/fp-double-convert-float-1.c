/* PR57245 */
/* { dg-do run } */
/* { dg-require-effective-target fenv } */
/* { dg-require-effective-target hard_float } */
/* { dg-additional-options "-frounding-math" } */

#include <fenv.h>
#include <stdlib.h>

int
main ()
{
#if __DBL_MANT_DIG__ == 53 && __FLT_MANT_DIG__ == 24
#ifdef FE_UPWARD
  fesetround (FE_UPWARD);
  float f = 1.3;
  if (f != 0x1.4ccccep+0f)
    __builtin_abort ();
#endif
#ifdef FE_TONEAREST
  fesetround (FE_TONEAREST);
  /* Use different actual values so the bogus CSE we perform does not
     break things.  */
  f = 1.33;
  if (f != 0x1.547ae2p+0f)
    abort ();
#endif
#ifdef FE_DOWNWARD
  fesetround (FE_DOWNWARD);
  f = 1.333;
  if (f != 0x1.553f7cp+0f)
    abort ();
#endif
#ifdef FE_TOWARDZERO
  fesetround (FE_TOWARDZERO);
  f = 1.3333;
  if (f != 0x1.555326p+0f)
    abort ();
#endif
#endif
  return 0;
}
