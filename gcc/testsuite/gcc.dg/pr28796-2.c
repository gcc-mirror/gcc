/* { dg-do run } */
/* { dg-options "-O2 -funsafe-math-optimizations -fno-finite-math-only" } */
/* { dg-options "-mieee -O2 -funsafe-math-optimizations -fno-finite-math-only" { target alpha*-*-* } } */

#include "tg-tests.h"

int main(void)
{
  return main_tests ();
}
