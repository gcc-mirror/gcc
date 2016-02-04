/* { dg-do compile } */
/* { dg-options "-mpku -O2" } */
/* { dg-final { scan-assembler "rdpkru\n" } } */

#include <x86intrin.h>

unsigned extern
rdpku_test (void)
{
  return _rdpkru_u32 ();
}
