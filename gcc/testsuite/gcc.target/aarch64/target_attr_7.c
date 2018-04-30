/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=thunderx -march=armv8-a -dA" } */

/* Make sure that #pragma overrides command line option and
   target attribute overrides the pragma.  */

#pragma GCC target ("cpu=xgene1")

int
bar (int a)
{
  return a - 6;
}

__attribute__ ((target ("tune=cortex-a53")))
int
bam (int a)
{
  return a - bar (a);
}

/* { dg-final { scan-assembler-times "//.tune xgene1" 1 } } */
/* { dg-final { scan-assembler-times "//.tune cortex-a53" 1 } } */
/* { dg-final { scan-assembler-not "thunderx" } } */
