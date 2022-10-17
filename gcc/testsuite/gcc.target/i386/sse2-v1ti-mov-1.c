/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -msse2" } */

typedef unsigned __int128 uv1ti __attribute__ ((__vector_size__ (16)));

uv1ti foo(__int128 x)
{
  return (uv1ti)x;
}

/* { dg-final { scan-assembler-not "%\[er\]sp" } } */
