/* { dg-do compile { target powerpc-*-eabi* } } */
/* { dg-options "-mcpu=8540" } */

/* Test vectors that can interconvert without a cast.  */

typedef int __attribute__((mode(V2SI))) __ev64_opaque__;

__ev64_opaque__ opp;
int vint   __attribute__((mode(V2SI)));
int vshort __attribute__((mode(V4HI)));
int vfloat __attribute__((mode(V2SF)));

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
