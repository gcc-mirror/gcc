/* PR tree-optimization/110755 */
/* { dg-do run } */
/* { dg-require-effective-target fenv } */
/* { dg-require-effective-target hard_float } */
/* { dg-options "-O2 -frounding-math" } */

#include <fenv.h>

__attribute__((noipa)) float
foo (float x)
{ 
  if (x > 0.0)
    { 
      x += 0x1p+23;
      x -= 0x1p+23;
      x = __builtin_fabsf (x);
    }
  return x;
}

int
main ()
{
#ifdef FE_DOWNWARD
  fesetround (FE_DOWNWARD);
  if (__builtin_signbit (foo (0.5)))
    __builtin_abort ();
#endif
}
