/* { dg-do compile } */
/* { dg-options "-O1" } */

unsigned int test_0(unsigned int a)
{
  return (a & 0x000000FF) << 24 |
         (a & 0x0000FF00) << 8  |
         (a & 0x00FF0000) >> 8  |
         (a & 0xFF000000) >> 24;
}

unsigned int test_1(unsigned int a)
{
  union
  {
    unsigned int i;
    unsigned char a[4];
  } u, v;
  u.i = a;
  v.a[0] = u.a[3];
  v.a[1] = u.a[2];
  v.a[2] = u.a[1];
  v.a[3] = u.a[0];
  return v.i;
}

unsigned int test_2(unsigned int a)
{
  return __builtin_bswap32(a);
}

unsigned long long test_3(unsigned long long a)
{
  return __builtin_bswap64(a);
}

/* { dg-final { scan-assembler-times "call" 2 } } */
