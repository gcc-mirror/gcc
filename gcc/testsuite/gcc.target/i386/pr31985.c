/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2" } */

void test_c (unsigned int a, unsigned int b, unsigned int c, unsigned int d)
{
  volatile unsigned int x, y;
  unsigned long long __a = b | ((unsigned long long)a << 32);
  unsigned long long __b = d | ((unsigned long long)c << 32);
  unsigned long long __c = __a + __b;
  x = (unsigned int)(__c & 0xffffffff);
  y = (unsigned int)(__c >> 32);
}

/* { dg-final { scan-assembler-times "movl" 4 } } */
