/* { dg-do run } */

#define NI __attribute__((__noipa__))
#define AI static __inline__ __attribute__((__always_inline__))

typedef __UINT8_TYPE__ uint8_t;
typedef __UINT16_TYPE__ uint16_t;
typedef __uint24 uint24_t;
typedef __UINT32_TYPE__ uint32_t;

AI uint16_t Aswap16 (uint16_t data)
{
  uint16_t d = 0;
  if (data & (1u << 15)) d |= 1u << 6;
  if (data & (1u << 14)) d |= 1u << 13;
  if (data & (1u << 13)) d |= 1u << 14;
  if (data & (1u << 12)) d |= 1u << 15;
  return d;
}

AI uint32_t Aswap32 (uint32_t data)
{
  uint32_t d = 0;
  if (data & (1ul << 31)) d |= 1ul << 6;
  if (data & (1ul << 30)) d |= 1ul << 13;
  if (data & (1ul << 13)) d |= 1ul << 30;
  if (data & (1ul << 12)) d |= 1ul << 31;
  return d;
}

AI uint24_t Aswap24 (uint24_t data)
{
  uint24_t d = 0;
  if (data & (1ul << 23)) d |= 1ul << 6;
  if (data & (1ul << 22)) d |= 1ul << 13;
  if (data & (1ul << 13)) d |= 1ul << 22;
  if (data & (1ul << 12)) d |= 1ul << 23;
  return d;
}

AI uint8_t Aswap8 (uint8_t data)
{
  uint8_t d = 0;
  if (data & (1 << 7)) d |= 1 << 2;
  if (data & (1 << 6)) d |= 1 << 3;
  if (data & (1 << 3)) d |= 1 << 6;
  if (data & (1 << 2)) d |= 1 << 7;
  return d;
}

NI uint8_t Nswap8 (uint8_t data) { return Aswap8 (data); }
NI uint16_t Nswap16 (uint16_t data) { return Aswap16 (data); }
NI uint24_t Nswap24 (uint24_t data) { return Aswap24 (data); }
NI uint32_t Nswap32 (uint32_t data) { return Aswap32 (data); }

void test8 (void)
{
  if (Nswap8 (0xaa) != Aswap8 (0xaa)) __builtin_abort();
  if (Nswap8 (0xcc) != Aswap8 (0xcc)) __builtin_abort();
  if (Nswap8 (0xf0) != Aswap8 (0xf0)) __builtin_abort();
}

void test16 (void)
{
  if (Nswap16 (0xaaaa) != Aswap16 (0xaaaa)) __builtin_abort();
  if (Nswap16 (0xcccc) != Aswap16 (0xcccc)) __builtin_abort();
  if (Nswap16 (0xf0f0) != Aswap16 (0xf0f0)) __builtin_abort();
  if (Nswap16 (0xff00) != Aswap16 (0xff00)) __builtin_abort();
}

void test24 (void)
{
  if (Nswap24 (0xaaaaaa) != Aswap24 (0xaaaaaa)) __builtin_abort();
  if (Nswap24 (0xcccccc) != Aswap24 (0xcccccc)) __builtin_abort();
  if (Nswap24 (0xf0f0f0) != Aswap24 (0xf0f0f0)) __builtin_abort();
  if (Nswap24 (0xfff000) != Aswap24 (0xfff000)) __builtin_abort();
}

void test32 (void)
{
  if (Nswap32 (0xaaaaaaaa) != Aswap32 (0xaaaaaaaa)) __builtin_abort();
  if (Nswap32 (0xcccccccc) != Aswap32 (0xcccccccc)) __builtin_abort();
  if (Nswap32 (0xf0f0f0f0) != Aswap32 (0xf0f0f0f0)) __builtin_abort();
  if (Nswap32 (0xff00ff00) != Aswap32 (0xff00ff00)) __builtin_abort();
  if (Nswap32 (0xffff0000) != Aswap32 (0xffff0000)) __builtin_abort();
}

int main (void)
{
  test8 ();
  test16 ();
  test24 ();
  test32 ();
  return 0;
}
