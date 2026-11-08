/* { dg-do compile } */
/* { dg-options "-O2 -mlasx" } */

typedef __int128 v2ti __attribute__ ((vector_size (32)));

v2ti
v2ti_xvldi (void)
{
  __int128 x = ((__int128)0xff00ff00000000LL << 64) | 0xff00ff00000000LL;
  return (v2ti){ x, x };
}
