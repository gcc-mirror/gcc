/* { dg-do compile } */
/* { dg-require-effective-target arm_fp16_alternative_ok } */
/* { dg-options "-pedantic -std=gnu99" } */
/* { dg-add-options arm_fp16_alternative } */

#include <math.h>

/* NaNs are not representable in the alternative format; we should get a
   diagnostic.  */
__fp16 xx = NAN; /* { dg-warning "overflow" } */
