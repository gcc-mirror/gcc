/* Tests to check if and instructions are emitted efficiently.  */
/* { dg-require-effective-target codedensity } */
/* { dg-options "-Os" } */

int check_bclr (int a)
{
  return a & (~0x40);
}

int check_bmskn (int a)
{
  return a & (-128);
}

/* { dg-final { scan-assembler "bclr_s" } } */
/* { dg-final { scan-assembler "bmskn" } } */
