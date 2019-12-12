/* PR c/60087 */
/* { dg-do compile } */
/* { dg-options "-Wsign-compare" } */

void
foo (unsigned int ui, int i)
{
  const unsigned char uc = 0;
  _Bool b;
  b = 0 != ~uc; /* { dg-warning "9:promoted bitwise complement of an unsigned value is always nonzero" } */
  b = 2 != ~uc; /* { dg-warning "9:comparison of promoted bitwise complement of an unsigned value with constant" } */
  b = uc == ~uc; /* { dg-warning "10:comparison of promoted bitwise complement of an unsigned value with unsigned" } */
  b = i == ui; /* { dg-warning "9:comparison of integer expressions of different signedness" } */
}
