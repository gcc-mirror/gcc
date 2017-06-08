/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-add-options ieee } */
/* { dg-skip-if "No Inf/NaN support" { spu-*-* } } */

#include "tg-tests.h"

int main(void)
{
  return main_tests ();
}
