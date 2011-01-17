/* { dg-do run } */
/* { dg-options "-O2 -funsafe-math-optimizations -fno-finite-math-only -DUNSAFE" } */
/* { dg-options "-mieee -O2 -funsafe-math-optimizations -fno-finite-math-only -DUNSAFE" { target alpha*-*-* } } */
/* { dg-skip-if "No Inf/NaN support" { spu-*-* } } */
/* { dg-skip-if "Bug in _Q_dtoq" { sparc*-sun-solaris2.8 } } */

#include "tg-tests.h"

int main(void)
{
  return main_tests ();
}
