/* { dg-do compile } */
/* { dg-options "-O3" } */

/* Ensure we use the signed/unsigned extend vectorized add and sub
   instructions.  */
#define N 1024

int a[N];
long c[N];
long d[N];
unsigned int ua[N];
unsigned long uc[N];
unsigned long ud[N];

void
add ()
{
  for (int i = 0; i < N; i++)
    d[i] = a[i] + c[i];
}
/* { dg-final { scan-assembler-times "\[ \t\]saddw2\[ \t\]+" 1 } } */
/* { dg-final { scan-assembler-times "\[ \t\]saddw\[ \t\]+" 1 } } */

void
subtract ()
{
  for (int i = 0; i < N; i++)
    d[i] = c[i] - a[i];
}
/* { dg-final { scan-assembler-times "\[ \t\]ssubw2\[ \t\]+" 1 } } */
/* { dg-final { scan-assembler-times "\[ \t\]ssubw\[ \t\]+" 1 } } */

void
uadd ()
{
  for (int i = 0; i < N; i++)
    ud[i] = ua[i] + uc[i];
}
/* { dg-final { scan-assembler-times "\[ \t\]uaddw2\[ \t\]+" 1 } } */
/* { dg-final { scan-assembler-times "\[ \t\]uaddw\[ \t\]+" 1 } } */

void
usubtract ()
{
  for (int i = 0; i < N; i++)
    ud[i] = uc[i] - ua[i];
}
/* { dg-final { scan-assembler-times "\[ \t\]usubw2\[ \t\]+" 1 } } */
/* { dg-final { scan-assembler-times "\[ \t\]usubw\[ \t\]+" 1 } } */
