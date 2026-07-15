/* { dg-do compile } */
/* { dg-options "-Og -O3 -mcmodel=tiny" } */
/* { dg-require-effective-target aarch64_mcmodel_tiny } */

int
main (__fp16 x)
{
  __fp16 a = 6.5504e4;
  return (x <= a);
}
