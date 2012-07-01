/* { dg-options "-O2" }  */
/* { dg-require-effective-target arm32 } */
/* { dg-final { scan-assembler-not "mov" } } */

void t0p(long long * p)
{
  *p -= 0x100000008;
}
