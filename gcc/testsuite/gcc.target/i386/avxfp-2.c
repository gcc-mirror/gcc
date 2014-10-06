/* { dg-do compile } */
/* { dg-options "-O2 -mavx -mfpmath=sse" } */
/* { dg-final { scan-assembler "vmaxsd" } } */
/* { dg-final { scan-assembler "vminsd" } } */
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
