/* { dg-options "-march=armv8-a" } */

void
f (__SVBool_t *x, __SVBool_t *y)
{
  *x = *y; /* { dg-error {this operation requires the SVE ISA extension} } */
  *x = *y;
}
