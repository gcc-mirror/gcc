/* { dg-do compile } */
/* { dg-options "-mfp16-format=alternative -pedantic -std=gnu99" } */

#include <math.h>

/* NaNs are not representable in the alternative format; we should get a
   diagnostic.  */
__fp16 xx = NAN; /* { dg-warning "overflow" } */
