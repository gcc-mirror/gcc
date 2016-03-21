/* { dg-do run } */
/* { dg-options "-O2 -fselective-scheduling -mtune=arm1136j-s" } */

typedef unsigned char uint8_t;
typedef unsigned short int uint16_t;
typedef unsigned int uint32_t;
typedef unsigned long long int uint64_t;
typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
u64 __attribute__((noinline, noclone))
foo(u8 u8_0, u16 u16_0, u32 u32_0, u64 u64_0, u8 u8_1, u16 u16_1, u32 u32_1, u64 u64_1, u8 u8_2, u16 u16_2, u32 u32_2, u64 u64_2, u8 u8_3, u16 u16_3, u32 u32_3, u64 u64_3)
{
  u8 *p8_2 = &u8_2;
  u16 *p16_2 = &u16_2;
  u8 *p8_3 = &u8_3;
  u64 *p64_3 = &u64_3;
  p8_2 = &u8_3;
  *p8_3 -= *p64_3;
  *p8_2 = (u64)*p8_2 % ((u64)*p8_2 | 3);
  u8_2 = (u64)u8_2 / ((u64)*p16_2 | 1);
  u16_0 = (u64)u16_0 % ((u64)*p8_2 | 3);
  return u8_0 + u16_0 + u32_0 + u64_0 + u8_1 + u16_1 + u32_1 + u64_1 + u8_2 + u16_2 + u32_2 + u64_2 + u8_3 + u16_3 + u32_3 + u64_3;
}
int main()
{
  u64 x = 0;
  x += foo(3llu, 6llu, 15llu, 28llu, 5llu, 11llu, 20llu, 44llu, 7llu, 10llu, 20llu, 55llu, 0llu, 9llu, 17llu, 48llu);
  __builtin_printf("%02x%02x%02x%02x%02x%02x%02x%02x\n", (unsigned)((x >> 0) & 0xff), (unsigned)((x >> 8) & 0xff), (unsigned)((x >> 16) & 0xff), (unsigned)((x >> 24) & 0xff), (unsigned)((x >> 32) & 0xff), (unsigned)((x >> 40) & 0xff), (unsigned)((x >> 48) & 0xff), (unsigned)((x >> 56) & 0xff));
  if (x != 0x1f3)
    __builtin_abort();
  return 0;
}
