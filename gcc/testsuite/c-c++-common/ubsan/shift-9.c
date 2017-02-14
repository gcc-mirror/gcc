/* PR sanitizer/77823 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-fsanitize=undefined -Wno-psabi -w" } */

typedef unsigned V __attribute__((vector_size(32)));
typedef unsigned __int128 W __attribute__((vector_size(32)));

V
foo (V v)
{
  return v << 30;
}

V
bar (V v, V w)
{
  return v << w;
}

W
baz (W v)
{
  return v << 30;
}

W
boo (W v, W w)
{
  return v << w;
}
