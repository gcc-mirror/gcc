/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -march=z13 --save-temps" } */

typedef __attribute__((vector_size(16))) double v2df;

v2df
adddbl (v2df a, v2df b)
{
  return a + b;
}
/* { dg-final { scan-assembler-times "vfadb" 1 } } */

v2df
subdbl (v2df a, v2df b)
{
  return a - b;
}
/* { dg-final { scan-assembler-times "vfsdb" 1 } } */

v2df
muldbl (v2df a, v2df b)
{
  return a * b;
}
/* { dg-final { scan-assembler-times "vfmdb" 1 } } */

v2df
divdbl (v2df a, v2df b)
{
  return a / b;
}
/* { dg-final { scan-assembler-times "vfd" 1 } } */

v2df
fmadbl (v2df a, v2df b, v2df c)
{
  return a * b + c;
}
/* { dg-final { scan-assembler-times "vfma" 1 } } */

v2df
fmsdbl (v2df a, v2df b, v2df c)
{
  return a * b - c;
}
/* { dg-final { scan-assembler-times "vfms" 1 } } */

