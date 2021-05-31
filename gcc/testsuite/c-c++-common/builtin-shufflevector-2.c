/* { dg-do compile } */

typedef long v4di __attribute__((vector_size(4 * sizeof (long))));
typedef int v4si __attribute__((vector_size(4 * sizeof (int))));
typedef int v8si __attribute__((vector_size(8 * sizeof (int))));

v4si res, a, b;
v4di resl, al, bl;
v8si res8, a8, b8;
void foo (void)
{
  res = __builtin_shufflevector (a, 0, 0, 1, 4, 5); /* { dg-error "must be vectors" } */
  res = __builtin_shufflevector (a, b, 0, 1, 4, 5, 6); /* { dg-error "power of two" } */
  res = __builtin_shufflevector (a, b, 0, 1, 4, 8); /* { dg-error "invalid" } */
  res = __builtin_shufflevector (a, b, 0, 1, -4, 5); /* { dg-error "invalid" } */
  res = __builtin_shufflevector (a, bl, 0, 1, 4, 5); /* { dg-error "same element type" } */
  resl = __builtin_shufflevector (a, b, 0, 1, 4, 5); /* { dg-error "" } incompatible types */
}
