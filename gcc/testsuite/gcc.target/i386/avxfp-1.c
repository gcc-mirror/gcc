/* { dg-do compile } */
/* { dg-options "-O2 -mavx -mfpmath=sse" } */
/* { dg-final { scan-assembler "vmaxsd" } } */
/* { dg-final { scan-assembler "vminsd" } } */
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
