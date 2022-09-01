/* PR target/106707 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-Oz -g -fno-cprop-registers -fno-dce" } */

typedef unsigned __attribute__((__vector_size__ (8))) V;

unsigned __int128 ii;
unsigned x, y;

V v;

void
foo (long long a)
{
  long long l = a != x;
  int i = __builtin_add_overflow_p (y * ii, 0, 0);
  V u = ii < x | v, w = x <= u < i & y <= x / ii;
  v = __builtin_shufflevector (v, w, 1, 2) + (V) l;
}
