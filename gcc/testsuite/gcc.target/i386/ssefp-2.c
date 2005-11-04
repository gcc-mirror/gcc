/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -msse2 -march=k8 -mfpmath=sse" } */
/* { dg-final { scan-assembler "maxsd" } } */
/* { dg-final { scan-assembler "minsd" } } */
double x;
q()
{
  x=x<5?5:x;
}

double x;
q1()
{
  x=x>5?5:x;
}
