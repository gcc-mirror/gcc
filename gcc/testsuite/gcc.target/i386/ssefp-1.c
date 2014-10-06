/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -march=k8 -mfpmath=sse" } */
/* { dg-final { scan-assembler "maxsd" } } */
/* { dg-final { scan-assembler "minsd" } } */
double x;
void
t()
{
  x=x>5?x:5;
}

double x;
void
q()
{
  x=x<5?x:5;
}
