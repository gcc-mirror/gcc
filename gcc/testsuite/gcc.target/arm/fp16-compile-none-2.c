/* { dg-do compile } */
/* { dg-options "-mfp16-format=none" } */

/* mode(HF) attributes are not recognized unless you explicitly enable
   half-precision floating point by selecting -mfp16-format=ieee or
   -mfp16-format=alternative.  */
float xx __attribute__((mode(HF))) = 0.0;  /* { dg-error "HF" } */
