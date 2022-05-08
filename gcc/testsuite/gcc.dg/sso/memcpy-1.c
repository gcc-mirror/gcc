/* { dg-do run } */

typedef unsigned char uint8_t;
typedef unsigned int uint32_t;

#define __big_endian_attr__ scalar_storage_order("big-endian")
#define __little_endian_attr__ scalar_storage_order("little-endian")

typedef union
{
  uint32_t val;
  uint8_t v[4];
} __attribute__((__big_endian_attr__)) upal_u32be_t;

typedef union
{
  uint32_t val;
  uint8_t v[4];
} __attribute__((__little_endian_attr__)) upal_u32le_t;

static inline uint32_t native_to_big_endian(uint32_t t)
{
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
  return t;
#else
  return __builtin_bswap32(t);
#endif
}
static inline uint32_t native_to_little_endian(uint32_t t)
{
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
  return __builtin_bswap32(t);
#else
  return t;
#endif
}
#define test(p, p1, i) do { if (p[i] != p1[i]) __builtin_abort (); } while (0)

#define tests(p, p1) do { test(p, p1, 0); test(p, p1, 1); \
                          test(p, p1, 2); test(p, p1, 3); } while (0)

int main(void)
{
  const uint32_t u = 0x12345678;

  upal_u32be_t tempb;
  __builtin_memcpy (&tempb, &u, sizeof(uint32_t));
  uint32_t bu = tempb.val;
  uint32_t b1u = native_to_big_endian(u);
  tests (((uint8_t*)&bu), ((uint8_t*)&b1u));

  upal_u32le_t templ;
  __builtin_memcpy (&templ, &u, sizeof(uint32_t));
  uint32_t lu = templ.val;
  uint32_t l1u = native_to_little_endian(u);
  tests (((uint8_t*)&lu), ((uint8_t*)&l1u));

  return 0;
}
