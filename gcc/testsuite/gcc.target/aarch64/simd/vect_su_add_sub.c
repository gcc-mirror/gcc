/* { dg-do compile } */
/* { dg-options "-O3" } */

typedef int __attribute__ ((mode (SI))) int32_t;
typedef int __attribute__ ((mode (DI))) int64_t;
typedef unsigned __attribute__ ((mode (SI))) size_t;
typedef unsigned __attribute__ ((mode (SI))) uint32_t;
typedef unsigned __attribute__ ((mode (DI))) uint64_t;

/* Ensure we use the signed/unsigned extend vectorized add and sub
   instructions.  */
#define N 1024

int32_t a[N];
int64_t c[N];
int64_t d[N];
uint32_t ua[N];
uint64_t uc[N];
uint64_t ud[N];

void
add ()
{
  for (size_t i = 0; i < N; i++)
    d[i] = a[i] + c[i];
}
/* { dg-final { scan-assembler-times "\[ \t\]saddw2\[ \t\]+" 1 } } */
/* { dg-final { scan-assembler-times "\[ \t\]saddw\[ \t\]+" 1 } } */

void
subtract ()
{
  for (size_t i = 0; i < N; i++)
    d[i] = c[i] - a[i];
}
/* { dg-final { scan-assembler-times "\[ \t\]ssubw2\[ \t\]+" 1 } } */
/* { dg-final { scan-assembler-times "\[ \t\]ssubw\[ \t\]+" 1 } } */

void
uadd ()
{
  for (size_t i = 0; i < N; i++)
    ud[i] = ua[i] + uc[i];
}
/* { dg-final { scan-assembler-times "\[ \t\]uaddw2\[ \t\]+" 1 } } */
/* { dg-final { scan-assembler-times "\[ \t\]uaddw\[ \t\]+" 1 } } */

void
usubtract ()
{
  for (size_t i = 0; i < N; i++)
    ud[i] = uc[i] - ua[i];
}
/* { dg-final { scan-assembler-times "\[ \t\]usubw2\[ \t\]+" 1 } } */
/* { dg-final { scan-assembler-times "\[ \t\]usubw\[ \t\]+" 1 } } */
