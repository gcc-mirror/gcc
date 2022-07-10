/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -fno-tree-forwprop --disable-tree-evrp" } */
typedef __int128 __attribute__((__vector_size__ (16))) V;

V
foo (V v)
{
  return (v & (V){15}) == v;
}
