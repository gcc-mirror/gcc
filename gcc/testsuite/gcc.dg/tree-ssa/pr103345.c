/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-bswap-details" } */

typedef unsigned int uint32_t;
typedef unsigned char uint8_t;

uint32_t load_le_32_or(const uint8_t *ptr)
{
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  return ((uint32_t)ptr[0]) |
         ((uint32_t)ptr[1] << 8) |
         ((uint32_t)ptr[2] << 16) |
         ((uint32_t)ptr[3] << 24);
#else
  return ((uint32_t)ptr[3]) |
         ((uint32_t)ptr[2] << 8) |
         ((uint32_t)ptr[1] << 16) |
         ((uint32_t)ptr[0] << 24);
#endif
}

uint32_t load_le_32_add(const uint8_t *ptr)
{
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  return ((uint32_t)ptr[0]) +
         ((uint32_t)ptr[1] << 8) +
         ((uint32_t)ptr[2] << 16) +
         ((uint32_t)ptr[3] << 24);
#else
  return ((uint32_t)ptr[3]) +
         ((uint32_t)ptr[2] << 8) +
         ((uint32_t)ptr[1] << 16) +
         ((uint32_t)ptr[0] << 24);
#endif
}

uint32_t load_le_32_xor(const uint8_t *ptr)
{
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  return ((uint32_t)ptr[0]) ^
         ((uint32_t)ptr[1] << 8) ^
         ((uint32_t)ptr[2] << 16) ^
         ((uint32_t)ptr[3] << 24);
#else
  return ((uint32_t)ptr[3]) ^
         ((uint32_t)ptr[2] << 8) ^
         ((uint32_t)ptr[1] << 16) ^
         ((uint32_t)ptr[0] << 24);
#endif
}

/* { dg-final { scan-tree-dump-times "32 bit load in target endianness found" 3 "bswap" } } */

