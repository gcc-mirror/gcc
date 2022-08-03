/* { dg-options "-O -fnon-call-exceptions -fno-tree-dce -fno-tree-forwprop -w" } */

/* Verify there is no ICE.  */

typedef short __attribute__ ((__vector_size__ (64))) V;
V v, w;

inline V foo (V a, V b);

V
foo (V a, V b)
{
  b &= v < b;
  return (V){foo (b, w)[3], (V){}[3]};
}
