/* PR target/127498 */
/* { dg-do compile } */
/* { dg-options "-O2 -mgfni -mavx -mno-avx2" } */

typedef unsigned char V __attribute__ ((vector_size (32)));

V
foo (V x, int n)
{
  return (x >> n) | (x << (8 - n));
}

V
bar (V x, int n)
{
  return (x << n) | (x >> (8 - n));
}
