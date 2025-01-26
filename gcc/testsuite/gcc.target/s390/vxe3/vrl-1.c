/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-final { scan-assembler {\tvrlf\t%v[0-9]+,%v[0-9]+,%v[0-9]+,0} } } */
/* { dg-final { scan-assembler {\tvrlg\t%v[0-9]+,%v[0-9]+,%v[0-9]+,0} } } */
/* { dg-final { scan-assembler {\tvrlq\t%v[0-9]+,%v[0-9]+,%v[0-9]+,0} } } */

typedef unsigned int __attribute__ ((vector_size (16))) UV4SI;
typedef unsigned long long __attribute__ ((vector_size (16))) UV2DI;
typedef unsigned __int128 __attribute__ ((vector_size (16))) UV1TI;

UV4SI
vrlf (UV4SI x, UV4SI y)
{
  return x % y;
}

UV2DI
vrlg (UV2DI x, UV2DI y)
{
  return x % y;
}

UV1TI
vrlq (UV1TI x, UV1TI y)
{
  return x % y;
}
