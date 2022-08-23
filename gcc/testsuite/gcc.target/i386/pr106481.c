/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -fno-dce -fno-forward-propagate -fno-rerun-cse-after-loop -Wno-psabi" } */

typedef int V __attribute__((vector_size (64)));
typedef __int128 W __attribute__((vector_size (64)));

W w;
V bar (void);

void
foo (V v, W)
{
  foo ((V){4, ~0}, (W) v);
  foo (v, w);
  bar ();
}

