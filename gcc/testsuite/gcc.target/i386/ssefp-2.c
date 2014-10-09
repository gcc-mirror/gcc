/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -march=k8 -mfpmath=sse" } */
/* { dg-final { scan-assembler "maxsd" } } */
/* { dg-final { scan-assembler "minsd" } } */
double x;
void
q()
{
  x=x<5?5:x;
}

double x;
void
q1()
{
  x=x>5?5:x;
}
