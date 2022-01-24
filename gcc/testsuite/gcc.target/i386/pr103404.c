/* { dg-do compile } */
/* { dg-additional-options "-Og -fcse-follow-jumps -fno-dce -fno-early-inlining -fgcse -fharden-conditional-branches -frerun-cse-after-loop -fno-tree-ccp -mavx5124fmaps -std=c99 -w" } */

typedef unsigned __attribute__((__vector_size__ (4))) U;
typedef unsigned __attribute__((__vector_size__ (16))) V;
typedef unsigned __attribute__((__vector_size__ (64))) W;

int x, y;

V v;
W w;

inline
int bar (U a)
{
  a |= x;
  W k =
    __builtin_shufflevector (v, 5 / a,
			     2, 4, 0, 2, 4, 1, 0, 1,
			     1, 2, 1, 3, 0, 4, 4, 0);
  w = k;
  y = 0;
}

int
foo ()
{
  bar ((U){0xffffffff});
  for (unsigned i; i < sizeof (foo);)
    ;
}

