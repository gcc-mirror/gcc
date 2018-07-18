/* PR debug/86194 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O -g -mavx512bw" } */

typedef unsigned U __attribute__ ((vector_size (64)));
typedef unsigned long V __attribute__ ((vector_size (64)));
typedef unsigned __int128 W __attribute__ ((vector_size (64)));

U u;

W
bar (W w)
{
  U k = u;
  w <<= (W)(U) { 5, 3, 3, 0, 7, 3, 1, 3, k[7] };
  k += (U) { -(char)w[3] } != k;
  return (W)k + w;
}

void
foo (void)
{
  u = (U){ bar ((W)(V) { 0, ~0, 0, 0, 0, 0, ~0 })[0] };
}
