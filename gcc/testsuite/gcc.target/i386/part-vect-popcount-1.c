/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vpopcntdq -mavx512bitalg -mavx512vl" } */
/* { dg-final { scan-assembler-times "vpopcntd\[^\n\r\]*xmm\[0-9\]" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vpopcntw\[^\n\r\]*xmm\[0-9\]" 3 { target ia32 } } } */
/* { dg-final { scan-assembler-times "vpopcntw\[^\n\r\]*xmm\[0-9\]" 2 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vpopcntb\[^\n\r\]*xmm\[0-9\]" 4 { target ia32 } } } */
/* { dg-final { scan-assembler-times "vpopcntb\[^\n\r\]*xmm\[0-9\]" 3 { target { ! ia32 } } } } */

void
foo1 (int* a, int* __restrict b)
{
  for (int i = 0; i != 2; i++)
    a[i] = __builtin_popcount (b[i]);
}

void
foo2 (unsigned short* a, unsigned short* __restrict b)
{
  for (int i = 0; i != 4; i++)
    a[i] = __builtin_popcount (b[i]);
}

void
foo3 (unsigned short* a, unsigned short* __restrict b)
{
  for (int i = 0; i != 2; i++)
    a[i] = __builtin_popcount (b[i]);
}

void
foo4 (unsigned char* a, unsigned char* __restrict b)
{
  for (int i = 0; i != 8; i++)
    a[i] = __builtin_popcount (b[i]);
}

void
foo5 (unsigned char* a, unsigned char* __restrict b)
{
  for (int i = 0; i != 4; i++)
    a[i] = __builtin_popcount (b[i]);
}

void
foo6 (unsigned char* a, unsigned char* __restrict b)
{
  for (int i = 0; i != 2; i++)
    a[i] = __builtin_popcount (b[i]);
}
