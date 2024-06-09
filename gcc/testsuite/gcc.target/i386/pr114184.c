/* PR target/114184 */
/* { dg-do compile } */
/* { dg-options "-Og -mavx2" } */

typedef unsigned char V __attribute__((vector_size (32)));
typedef unsigned char W __attribute__((vector_size (16)));

_Complex long double
foo (void)
{
  _Complex long double d;
  *(V *)&d = (V) { 149, 136, 89, 42, 38, 240, 196, 194 };
  return d;
}

long double
bar (void)
{
  long double d;
  *(W *)&d = (W) { 149, 136, 89, 42, 38, 240, 196, 194 };
  return d;
}
