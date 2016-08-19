/* Tests for _FloatN / _FloatNx types: test erroneous mixing with DFP.  */
/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-add-options float32 } */
/* { dg-add-options float32x } */
/* { dg-require-effective-target float32 } */
/* { dg-require-effective-target float32x } */

_Decimal32 d32;
_Float32 f32;
_Float32x f32x;
int i;

void
f (void)
{
  (void) (d32 + f32); /* { dg-error "mix operands" } */
  (void) (f32x * d32); /* { dg-error "mix operands" } */
  (void) (i ? d32 : f32); /* { dg-error "mix operands" } */
}
