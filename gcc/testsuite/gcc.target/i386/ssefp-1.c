/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -march=k8 -mfpmath=sse" } */
/* { dg-require-effective-target sse2 } */
/* { dg-final { scan-assembler "maxsd" } } */
/* { dg-final { scan-assembler "minsd" } } */
double x;
t()
{
  x=x>5?x:5;
}

double x;
q()
{
  x=x<5?x:5;
}
