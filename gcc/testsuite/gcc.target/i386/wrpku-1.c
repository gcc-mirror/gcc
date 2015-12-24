/* { dg-do compile } */
/* { dg-options "-mpku -O2" } */
/* { dg-final { scan-assembler "wrpkru\n" } } */

#include <x86intrin.h>

void extern
wrpku_test (unsigned int key)
{
  _wrpkru (key);
}
