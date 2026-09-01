/* { dg-do run } */
/* { dg-options "-O2 -funsafe-math-optimizations -fno-finite-math-only -DUNSAFE" } */
/* { dg-add-options ieee } */
/* { dg-skip-if "not fully ieee" { avr-*-* } } */

#include "tg-tests.h"

int main(void)
{
  return main_tests ();
}
