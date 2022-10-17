/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -msse2" } */

typedef unsigned __int128 v1ti __attribute__ ((__vector_size__ (16)));

v1ti and(v1ti x, v1ti y)
{
  return x & y;
}

v1ti ior(v1ti x, v1ti y)
{
  return x | y;
}

v1ti xor(v1ti x, v1ti y)
{
  return x ^ y;
}

v1ti not(v1ti x)
{
  return ~x;
}

/* { dg-final { scan-assembler "pand" } } */
/* { dg-final { scan-assembler "por" } } */
/* { dg-final { scan-assembler-times "pxor" 2 } } */
