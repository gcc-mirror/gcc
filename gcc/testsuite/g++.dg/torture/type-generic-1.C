/* Do the type-generic tests.  Unlike pr28796-2.c, we test these
   without any fast-math flags.  */

/* { dg-do run } */
/* { dg-options "-mieee" { target sh*-*-* } } */
/* IBM 128-bit long double is not IEEE, don't use it for this test.  */
/* { dg-options "-mlong-double-64" { target powerpc*-*-linux* } } */
/* { dg-skip-if "No Inf/NaN support" { spu-*-* } } */

#include "../../gcc.dg/tg-tests.h"

int main(void)
{
  return main_tests ();
}
