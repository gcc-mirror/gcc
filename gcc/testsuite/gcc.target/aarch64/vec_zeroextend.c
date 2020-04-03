/* { dg-do compile } */
/* { dg-options "-O2" } */

#define vector __attribute__((vector_size(16) ))

#define lowull (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__ ? 1 : 0)
#define lowui (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__ ? 3 : 0)


vector unsigned long long
f1(vector unsigned long long b, vector unsigned int a)
{
  b[lowull] = a[lowui];
  return b;
}

unsigned long long
f2(vector unsigned int a)
{
  return a[lowui];
}

/* { dg-final { scan-assembler-times {fmov} 2 } } */
/* { dg-final { scan-assembler-not {umov} } } */
/* { dg-final { scan-assembler-not {uxtw} } } */
