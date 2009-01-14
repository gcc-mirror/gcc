/* { dg-do compile } */
/* { dg-options "-mcpu=8540 -mspe -mabi=spe -mfloat-gprs=single" } */
/* { dg-skip-if "not an SPE target" { ! powerpc_spe_nocache } { "*" } { "" } } */

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
  vfloat = vshort; /* { dg-error "incompatible types when assigning" } */

  /* Just because this is a V2SI, it doesn't make it an opaque.  */
  vint = vshort; /* { dg-message "note: use -flax-vector-conversions to permit conversions between vectors with differing element types or numbers of subparts" } */
  /* { dg-error "incompatible types when assigning" "" { target *-*-* } 22 } */

  return 0;
}
