/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-final { scan-assembler {\tvdf\t%v[0-9]+,%v[0-9]+,%v[0-9]+,0} } } */
/* { dg-final { scan-assembler {\tvdg\t%v[0-9]+,%v[0-9]+,%v[0-9]+,0} } } */
/* { dg-final { scan-assembler {\tvdq\t%v[0-9]+,%v[0-9]+,%v[0-9]+,0} } } */

typedef int __attribute__ ((vector_size (16))) V4SI;
typedef long long __attribute__ ((vector_size (16))) V2DI;
typedef __int128 __attribute__ ((vector_size (16))) V1TI;

V4SI
vdf (V4SI x, V4SI y)
{
  return x / y;
}

V2DI
vdg (V2DI x, V2DI y)
{
  return x / y;
}

V1TI
vdq (V1TI x, V1TI y)
{
  return x / y;
}
