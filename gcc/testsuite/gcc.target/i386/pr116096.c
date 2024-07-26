/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -flive-range-shrinkage -fno-peephole2 -mstackrealign -Wno-psabi" } */

typedef char U __attribute__((vector_size (32)));
typedef unsigned V __attribute__((vector_size (32)));
typedef __int128 W __attribute__((vector_size (32)));
U g;

W baz ();

static inline U
bar (V x, W y)
{
  y = y | y << (W) x;
  return (U)y;
}

void
foo (W w)
{
  g = g <<
    bar ((V){baz ()[1], 3, 3, 5, 7},
	 (W){w[0], ~(int) 2623676210}) >>
    bar ((V){baz ()[1]},
	 (W){-w[0], ~(int) 2623676210});
}
