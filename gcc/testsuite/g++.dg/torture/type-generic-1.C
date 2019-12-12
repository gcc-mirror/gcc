/* Do the type-generic tests.  Unlike pr28796-2.c, we test these
   without any fast-math flags.  */

/* { dg-do run } */
/* { dg-add-options ieee } */
/* { dg-skip-if "No subnormal support" { csky-*-* } { "-mhard-float" } } */

#include "../../gcc.dg/tg-tests.h"

int main(void)
{
  return main_tests ();
}
