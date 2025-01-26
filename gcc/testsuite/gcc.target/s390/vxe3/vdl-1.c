/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-final { scan-assembler {\tvdlf\t%v[0-9]+,%v[0-9]+,%v[0-9]+,0} } } */
/* { dg-final { scan-assembler {\tvdlg\t%v[0-9]+,%v[0-9]+,%v[0-9]+,0} } } */
/* { dg-final { scan-assembler {\tvdlq\t%v[0-9]+,%v[0-9]+,%v[0-9]+,0} } } */

typedef unsigned int __attribute__ ((vector_size (16))) UV4SI;
typedef unsigned long long __attribute__ ((vector_size (16))) UV2DI;
typedef unsigned __int128 __attribute__ ((vector_size (16))) UV1TI;

UV4SI
vdlf (UV4SI x, UV4SI y)
{
  return x / y;
}

UV2DI
vdlg (UV2DI x, UV2DI y)
{
  return x / y;
}

UV1TI
vdlq (UV1TI x, UV1TI y)
{
  return x / y;
}
