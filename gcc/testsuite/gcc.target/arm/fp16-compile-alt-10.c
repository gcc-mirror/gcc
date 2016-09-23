/* { dg-do compile } */
/* { dg-require-effective-target arm_fp16_alternative_ok } */
/* { dg-options "-mfp16-format=alternative -pedantic -std=gnu99" } */

#include <math.h>

/* NaNs are not representable in the alternative format; we should get a
   diagnostic.  */
__fp16 xx = NAN; /* { dg-warning "overflow" } */
