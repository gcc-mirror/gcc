/* { dg-do compile } */
/* { dg-require-effective-target bswap16 } */
/* { dg-require-effective-target stdint_types } */
/* { dg-options "-O2 -fdump-tree-bswap" } */
/* { dg-additional-options "-march=z900" { target s390-*-* } } */

#include <stdint.h>

unsigned char data[2];

struct uint16_st {
  unsigned char u0, u1;
};

uint32_t read_le16_1 (void)
{
  return data[0] | (data[1] << 8);
}

uint32_t read_le16_2 (struct uint16_st data)
{
  return data.u0 | (data.u1 << 8);
}

uint32_t read_le16_3 (unsigned char *data)
{
  return *data | (*(data + 1) << 8);
}

uint32_t read_be16_1 (void)
{
  return data[1] | (data[0] << 8);
}

uint32_t read_be16_2 (struct uint16_st data)
{
  return data.u1 | (data.u0 << 8);
}

uint32_t read_be16_3 (unsigned char *data)
{
  return *(data + 1) | (*data << 8);
}

typedef int HItype __attribute__ ((mode (HI)));

/* Test that detection of significant sign extension works correctly. This
   checks that unknown byte markers are set correctly in cast of cast.  */

HItype
swap16 (HItype in)
{
  return (HItype) (((in >> 0) & 0xFF) << 8)
		| (((in >> 8) & 0xFF) << 0);
}

/* { dg-final { scan-tree-dump-times "16 bit load in target endianness found at" 3 "bswap" } } */
/* { dg-final { scan-tree-dump-times "16 bit bswap implementation found at" 4 "bswap" } } */
