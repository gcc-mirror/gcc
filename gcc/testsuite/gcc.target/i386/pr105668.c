/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O -ftracer -fno-tree-fre" } */

typedef __int128 __attribute__((__vector_size__ (16))) V;

int i;

V
foo (_Complex float f)
{
  (void) __builtin_atanhf (i);
  V v = i != (V) { };
  i ^= f && 8;
  v %= 5;
  return v;
}
