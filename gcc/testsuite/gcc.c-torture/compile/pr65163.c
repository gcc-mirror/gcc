/* PR target/65163  */

typedef unsigned int uint32_t;
typedef unsigned short uint16_t;
union unaligned_32 { uint32_t l; } __attribute__((packed));
union unaligned_16 { uint16_t l; } __attribute__((packed));

int
test_00 (unsigned char* buf, int bits_per_component)
{
  (((union unaligned_32*)(buf))->l) =
    __builtin_bswap32 (bits_per_component == 10 ? 1 : 0);
  return 0;
}

int
test_01 (unsigned char* buf, int bits_per_component)
{
  (((union unaligned_16*)(buf))->l) =
    __builtin_bswap16 (bits_per_component == 10 ? 1 : 0);
  return 0;
}
