/* { dg-do compile { target powerpc-*-eabi* } } */
/* { dg-options "-mspe=yes" } */

/* Test vectors that can interconvert without a cast.  */

__ev64_opaque__ opp;
int vint   __attribute__((vector_size (8)));
short vshort __attribute__((vector_size (8)));
float vfloat __attribute__((vector_size (8)));

int
main (void)
{
  __ev64_opaque__ george = { 1, 2 }; /* { dg-error "opaque vector types cannot be initialized" } */

  opp = vfloat;
  vshort = opp;
  vfloat = vshort; /* { dg-error "incompatible types in assignment" } */

  /* Just because this is a V2SI, it doesn't make it an opaque.  */
  vint = vshort; /* { dg-error "incompatible types in assignment" } */

  return 0;
}
