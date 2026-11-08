/* { dg-do compile } */
/* { dg-options "-O2 -mlsx" } */

typedef long long v2di __attribute__ ((vector_size (16)));
typedef __int128 v1ti __attribute__ ((vector_size (16)));

v1ti
v1ti_vldi (void)
{
  return (v1ti)(v2di){ 0xff00ff00000000LL, 0xff00ff00000000LL };
}
