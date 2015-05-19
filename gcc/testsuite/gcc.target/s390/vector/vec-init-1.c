/* Check that the vec_init expander does its job.  */

/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=z13" } */





typedef __attribute__((vector_size(16))) signed int v4si;

extern v4si G;

v4si
f (signed int a)
{
  return G == a;
}
/* { dg-final { scan-assembler-times "vrepf" 1 } } */

v4si
g (signed int *a)
{
  return G == *a;
}
/* { dg-final { scan-assembler-times "vlrepf" 1 } } */

v4si
h ()
{
  return G == 1;
}
/* { dg-final { scan-assembler-times "vgmf\t%v.*,31,31" 1 } } */

v4si
i ()
{
  return G == -1;
}
/* { dg-final { scan-assembler-times "vone" 1 } } */

v4si
j ()
{
  return G == 0;
}
/* { dg-final { scan-assembler-times "vzero" 1 } } */

v4si
k ()
{
  return G == (v4si){ 0xff80, 0xff80, 0xff80, 0xff80 };
}
/* { dg-final { scan-assembler-times "vgmf\t%v.*,16,24" 1 } } */

v4si
l ()
{
  return G == (v4si){ 0xf000000f, 0xf000000f, 0xf000000f, 0xf000000f };
}
/* { dg-final { scan-assembler-times "vgmf\t%v.*,28,3" 1 } } */

v4si
m ()
{
  return G == (v4si){ 0x00ff00ff, 0x0000ffff, 0xffff0000, 0xff00ff00 };
}
/* { dg-final { scan-assembler-times "vgbm\t%v.*,21450" 1 } } */
