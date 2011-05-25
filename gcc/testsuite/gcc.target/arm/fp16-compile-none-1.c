/* { dg-do compile } */
/* { dg-options "-mfp16-format=none" } */

/* __fp16 type name is not recognized unless you explicitly enable it
   by selecting -mfp16-format=ieee or -mfp16-format=alternative.  */
__fp16 xx = 0.0;  /* { dg-error "unknown type name" } */
