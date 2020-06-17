/* { dg-do compile } */
/* { dg-require-effective-target bswap } */
/* { dg-require-effective-target stdint_types } */
/* { dg-options "-O2 -fdump-tree-bswap" } */
/* { dg-additional-options "-march=z900" { target s390*-*-* } } */

#include <stdint.h>

extern unsigned char data[4];

struct uint32_st {
  unsigned char u0, u1, u2, u3;
};

uint32_t read_le32_1 (void)
{
  return data[0] | (data[1] << 8) | (data[2] << 16) | (data[3] << 24);
}

uint32_t read_le32_2 (struct uint32_st data)
{
  return data.u0 | (data.u1 << 8) | (data.u2 << 16) | (data.u3 << 24);
}

uint32_t read_le32_3 (unsigned char *data)
{
  return *data | (*(data + 1) << 8) | (*(data + 2) << 16)
	 | (*(data + 3) << 24);
}

uint32_t read_be32_1 (void)
{
  return data[3] | (data[2] << 8) | (data[1] << 16) | (data[0] << 24);
}

uint32_t read_be32_2 (struct uint32_st data)
{
  return data.u3 | (data.u2 << 8) | (data.u1 << 16) | (data.u0 << 24);
}

uint32_t read_be32_3 (unsigned char *data)
{
  return *(data + 3) | (*(data + 2) << 8) | (*(data + 1) << 16)
	 | (*data << 24);
}

static inline unsigned short
get_unaligned_16_le (unsigned char *p)
{
  return p[0] | (p[1] << 8);
}
unsigned int
get_unaligned_32_le (unsigned char *p)
{
  return get_unaligned_16_le (p) | (get_unaligned_16_le (p + 2) << 16);
}

static inline unsigned short
get_unaligned_16_be (unsigned char *p)
{
  return p[1] | (p[0] << 8);
}
unsigned int
get_unaligned_32_be (unsigned char *p)
{
  return get_unaligned_16_be (p + 2) | (get_unaligned_16_be (p) << 16);
}

/* { dg-final { scan-tree-dump-times "32 bit load in target endianness found at" 4 "bswap" } } */
/* { dg-final { scan-tree-dump-times "32 bit bswap implementation found at" 4 "bswap" } } */
