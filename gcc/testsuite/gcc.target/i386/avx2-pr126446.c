/* PR target/126446 */
/* { dg-do compile } */
/* { dg-options "-O1 -mavx2" } */

typedef signed char V __attribute__((vector_size (16)));

signed char
foo ()
{
  V b = {};
  long long c = ~2878966870562407444LL;
  b[c] = 1;
  return b[0];
}
