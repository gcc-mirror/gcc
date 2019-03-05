/* { dg-do compile } */
/* { dg-options "-O2" } */

#define vector __attribute__((vector_size(16) ))

vector unsigned long long
f1(vector unsigned long long b, vector unsigned int a)
{
  b[0] = a[0];
  return b;
}

unsigned long long
f2(vector unsigned int a)
{
  return a[0];
}

/* { dg-final { scan-assembler-times {fmov} 2 } } */
/* { dg-final { scan-assembler-not {umov} } } */
/* { dg-final { scan-assembler-not {uxtw} } } */
