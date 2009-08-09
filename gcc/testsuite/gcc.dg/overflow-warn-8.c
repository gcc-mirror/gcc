#include <limits.h>

void foo (int j)
{
  int i1 = (int)(double)1.0 + INT_MAX; /* { dg-warning "integer overflow" } */
  int i2 = (int)(double)1 + INT_MAX; /* { dg-warning "integer overflow" } */
  int i3 = 1 + INT_MAX; /* { dg-warning "integer overflow" } */
  int i4 = +1 + INT_MAX; /* { dg-warning "integer overflow" } */
  int i5 = (int)((double)1.0 + INT_MAX);
  int i6 = (double)1.0 + INT_MAX; /* { dg-warning "overflow in implicit constant" } */
  int i7 = 0 ? (int)(double)1.0 + INT_MAX : 1;
  int i8 = 1 ? 1 : (int)(double)1.0 + INT_MAX;
  int i9 = j ? (int)(double)1.0 + INT_MAX : 1; /* { dg-warning "integer overflow" } */
  unsigned int i10 = 0 ? (int)(double)1.0 + INT_MAX : 9U;
  unsigned int i11 = 1 ? 9U : (int)(double)1.0 + INT_MAX;
  unsigned int i12 = j ? (int)(double)1.0 + INT_MAX : 9U; /* { dg-warning "integer overflow" } */
  int i13 = 1 || (int)(double)1.0 + INT_MAX < 0;
  int i14 = 0 && (int)(double)1.0 + INT_MAX < 0;
  int i15 = 0 || (int)(double)1.0 + INT_MAX < 0; /* { dg-warning "integer overflow" } */
  int i16 = 1 && (int)(double)1.0 + INT_MAX < 0; /* { dg-warning "integer overflow" } */
  int i17 = j || (int)(double)1.0 + INT_MAX < 0; /* { dg-warning "integer overflow" } */
  int i18 = j && (int)(double)1.0 + INT_MAX < 0; /* { dg-warning "integer overflow" } */
}
