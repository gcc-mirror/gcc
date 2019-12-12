/* Tests to check if or instructions are emitted efficiently.  */
/* { dg-require-effective-target codedensity } */
/* { dg-options "-Os" } */

int check_bset1 (int a)
{
  return a | 0x80000000;
}

int check_bset2(int a)
{
  return a | 0x2022;
}

/* { dg-final { scan-assembler-times "bset_s" 2 } } */
/* { dg-final { scan-assembler "or" } } */
