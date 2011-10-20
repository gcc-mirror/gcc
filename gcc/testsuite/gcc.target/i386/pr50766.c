/* PR target/50766 */
/* { dg-do assemble } */
/* { dg-options "-mbmi2" } */
/* { dg-require-effective-target bmi2 } */

#include <x86intrin.h>

unsigned z;

void
foo ()
{
  unsigned x = 0x23593464;
  unsigned y = 0xF9494302;
  z = _pext_u32(x, y);
}

