/* { dg-do compile } */
/* { dg-options "-O2" } */

extern void foo(void);

void test_0(volatile unsigned int a[], unsigned int b)
{
  a[0] = __builtin_bswap32(a[0]);
  a[1] = a[1] >> 9;
  a[2] = __builtin_bswap32(a[2]);
  a[3] = a[3] << b;
  a[4] = __builtin_bswap32(a[4]);
  foo();
  a[5] = __builtin_bswap32(a[5]);
  a[6] = __builtin_stdc_rotate_left (a[6], 13);
  a[7] = __builtin_bswap32(a[7]);
  asm volatile ("# asm volatile");
  a[8] = __builtin_bswap32(a[8]);
  a[9] = (a[9] << 9) | (b >> 23);
  a[10] = __builtin_bswap32(a[10]);
}

void test_1(volatile unsigned long long a[])
{
  a[0] = __builtin_bswap64(a[0]);
  a[1] = __builtin_bswap64(a[1]);
}

/* { dg-final { scan-assembler-times "ssai\t8" 7 } } */
