/* { dg-do run } */

#define NI __attribute__((__noinline__,__noclone__))
#define AI static __inline__ __attribute__((__always_inline__))

typedef __UINT8_TYPE__ uint8_t;
typedef __UINT16_TYPE__ uint16_t;
typedef __uint24 uint24_t;
typedef __UINT32_TYPE__ uint32_t;

typedef __INT32_TYPE__ int32_t;

#define ADD(W,B,N)                              \
  do                                            \
    {                                           \
      if (!c)                                   \
        __asm ("sbrc %T1%T2 $ subi %0,%n3"      \
               : "+d" (b)                       \
               : "r" (num), "n" (B), "n" (N));  \
      else                                      \
        if (num & ((uint##W##_t) 1 << B))       \
          b += N;                               \
    } while (0)

NI uint8_t Afun1 (uint32_t num)
{
  uint8_t b = 0;
  int c = 0;
  ADD (32, 31, 1);
  ADD (32, 29, 1);
  ADD (32, 13, 1);
  return b;
}
NI uint8_t Cfun1 (uint32_t num)
{
  uint8_t b = 0;
  int c = 1;
  ADD (32, 31, 1);
  ADD (32, 29, 1);
  ADD (32, 13, 1);
  return b;
}

NI uint8_t Afun2 (uint32_t num)
{
  uint8_t b = 0;
  int c = 0;
  ADD (32, 31, -1);
  ADD (32, 29, 1);
  ADD (32, 13, 1);
  return b;
}
NI uint8_t Cfun2 (uint32_t num)
{
  uint8_t b = 0;
  int c = 1;
  ADD (32, 31, -1);
  ADD (32, 29, 1);
  ADD (32, 13, 1);
  return b;
}

NI uint8_t Afun3 (uint32_t num)
{
  uint8_t b = 0;
  int c = 0;
  ADD (32, 13, 1);
  ADD (32, 29, 1);
  ADD (32, 31, 1);
  return b;
}
NI uint8_t Cfun3 (uint32_t num)
{
  uint8_t b = 0;
  int c = 1;
  ADD (32, 13, 1);
  ADD (32, 29, 1);
  ADD (32, 31, 1);
  return b;
}

NI uint8_t Afun4 (uint32_t num)
{
  uint8_t b = 0;
  int c = 0;
  ADD (32, 13, -1);
  ADD (32, 29, 1);
  ADD (32, 31, -1);
  return b;
}
NI uint8_t Cfun4 (uint32_t num)
{
  uint8_t b = 0;
  int c = 1;
  ADD (32, 13, -1);
  ADD (32, 29, 1);
  ADD (32, 31, -1);
  return b;
}

void test32_0 (uint32_t x)
{
  if (Afun1 (x) != Cfun1 (x)) __builtin_abort();
  if (Afun2 (x) != Cfun2 (x)) __builtin_abort();
  if (Afun3 (x) != Cfun3 (x)) __builtin_abort();
  if (Afun4 (x) != Cfun4 (x)) __builtin_abort();
}

void test32_1 (uint32_t x)
{
  test32_0 (x);
  test32_0 (~x);
}

void test32 (void)
{
  test32_1 (0);
  test32_1 (0x55555555);
  test32_1 (0xcccccccc);
  test32_1 (0x0f0f0f0f);
  test32_1 (0x00ff00ff);
  test32_1 (0x0000ffff);
}

/*********************************************************************/

NI uint8_t Afun5 (uint24_t num)
{
  uint8_t b = 0;
  int c = 0;
  ADD (24, 23, 1);
  ADD (24, 21, 1);
  ADD (24, 13, 1);
  return b;
}
NI uint8_t Cfun5 (uint24_t num)
{
  uint8_t b = 0;
  int c = 1;
  ADD (24, 23, 1);
  ADD (24, 21, 1);
  ADD (24, 13, 1);
  return b;
}

NI uint8_t Afun6 (uint24_t num)
{
  uint8_t b = 0;
  int c = 0;
  ADD (24, 23, -1);
  ADD (24, 21, -1);
  ADD (24, 13, 1);
  return b;
}
NI uint8_t Cfun6 (uint24_t num)
{
  uint8_t b = 0;
  int c = 1;
  ADD (24, 23, -1);
  ADD (24, 21, -1);
  ADD (24, 13, 1);
  return b;
}

NI uint8_t Afun7 (uint24_t num)
{
  uint8_t b = 0;
  int c = 0;
  ADD (24, 0, 1);
  ADD (24, 21, 1);
  ADD (24, 23, 1);
  return b;
}
NI uint8_t Cfun7 (uint24_t num)
{
  uint8_t b = 0;
  int c = 1;
  ADD (24, 0, 1);
  ADD (24, 21, 1);
  ADD (24, 23, 1);
  return b;
}

void test24_0 (uint24_t x)
{
  if (Afun5 (x) != Cfun5 (x)) __builtin_abort();
  if (Afun6 (x) != Cfun6 (x)) __builtin_abort();
  if (Afun7 (x) != Cfun7 (x)) __builtin_abort();
}

void test24_1 (uint24_t x)
{
  test24_0 (x);
  test24_0 (~x);
}

void test24 (void)
{
  test24_1 (0);
  test24_1 (0x555555);
  test24_1 (0xcccccc);
  test24_1 (07070707);
  test24_1 (0x0f0f0f);
  test24_1 (0x000fff);
}

/*********************************************************************/

NI uint8_t Afun15 (uint16_t num)
{
  uint8_t b = 0;
  int c = 0;
  ADD (16, 15, 1);
  ADD (16,  2, 1);
  ADD (16, 13, 1);
  return b;
}
NI uint8_t Cfun15 (uint16_t num)
{
  uint8_t b = 0;
  int c = 1;
  ADD (16, 15, 1);
  ADD (16,  2, 1);
  ADD (16, 13, 1);
  return b;
}

NI uint8_t Afun16 (uint16_t num)
{
  uint8_t b = 0;
  int c = 0;
  ADD (16, 15, -1);
  ADD (16,  2, 1);
  ADD (16, 13, 1);
  return b;
}
NI uint8_t Cfun16 (uint16_t num)
{
  uint8_t b = 0;
  int c = 1;
  ADD (16, 15, -1);
  ADD (16,  2, 1);
  ADD (16, 13, 1);
  return b;
}

NI uint8_t Afun17 (uint16_t num)
{
  uint8_t b = 0;
  int c = 0;
  ADD (16, 9, 1);
  ADD (16, 2, 1);
  ADD (16, 15, 1);
  return b;
}
NI uint8_t Cfun17 (uint16_t num)
{
  uint8_t b = 0;
  int c = 1;
  ADD (16, 9, 1);
  ADD (16, 2, 1);
  ADD (16, 15, 1);
  return b;
}

void test16_0 (uint16_t x)
{
  if (Afun15 (x) != Cfun15 (x)) __builtin_abort();
  if (Afun16 (x) != Cfun16 (x)) __builtin_abort();
  if (Afun17 (x) != Cfun17 (x)) __builtin_abort();
}

void test16_1 (uint16_t x)
{
  test16_0 (x);
  test16_0 (~x);
}

void test16 (void)
{
  test16_1 (0);
  test16_1 (0x5555);
  test16_1 (0xcccc);
  test16_1 (0x0f0f);
  test16_1 (0x00ff);
}

int main (void)
{
  test32 ();
  test24 ();
  test16 ();
  return 0;
}
