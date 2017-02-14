/* PR target/79498 */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -mno-avx512f -mcmodel=large -Wno-psabi" } */

typedef unsigned U __attribute__ ((vector_size (64)));
typedef unsigned __int128 V __attribute__ ((vector_size (64)));

static inline V
bar (U u, U x, V v)
{
  v = (V)(U) { 0, ~0 };
  v[x[0]] <<= u[-63];
  return v;
}

V
foo (U u)
{
  return bar (u, (U) {}, (V) {});
}
