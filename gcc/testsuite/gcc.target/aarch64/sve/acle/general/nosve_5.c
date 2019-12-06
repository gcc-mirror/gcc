/* { dg-options "-march=armv8-a" } */

void
f (__SVInt8_t *x, __SVInt8_t *y)
{
  *x = *y; /* { dg-error {this operation requires the SVE ISA extension} } */
  *x = *y;
}
