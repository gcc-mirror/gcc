/* { dg-do compile } */
/* { dg-require-effective-target bswap } */
/* { dg-require-effective-target stdint_types } */
/* { dg-options "-O2 -fdump-tree-bswap" } */
/* { dg-additional-options "-mzarch" { target s390*-*-* } } */

#include <stdint.h>

unsigned char data[8];

struct uint64_st {
  unsigned char u0, u1, u2, u3, u4, u5, u6, u7;
};

uint64_t read_le64_1 (void)
{
  return (uint64_t) data[0] | ((uint64_t) data[1] << 8)
	 | ((uint64_t) data[2] << 16) | ((uint64_t) data[3] << 24)
	 | ((uint64_t) data[4] << 32) | ((uint64_t) data[5] << 40)
	 | ((uint64_t) data[6] << 48) | ((uint64_t) data[7] << 56);
}

uint64_t read_le64_2 (struct uint64_st data)
{
  return (uint64_t) data.u0 | ((uint64_t) data.u1 << 8)
	 | ((uint64_t) data.u2 << 16) | ((uint64_t) data.u3 << 24)
	 | ((uint64_t) data.u4 << 32) | ((uint64_t) data.u5 << 40)
	 | ((uint64_t) data.u6 << 48) | ((uint64_t) data.u7 << 56);
}

uint64_t read_le64_3 (unsigned char *data)
{
  return (uint64_t) *data | ((uint64_t) *(data + 1) << 8)
	 | ((uint64_t) *(data + 2) << 16) | ((uint64_t) *(data + 3) << 24)
	 | ((uint64_t) *(data + 4) << 32) | ((uint64_t) *(data + 5) << 40)
	 | ((uint64_t) *(data + 6) << 48) | ((uint64_t) *(data + 7) << 56);
}

uint64_t read_be64_1 (void)
{
  return (uint64_t) data[7] | ((uint64_t) data[6] << 8)
	 | ((uint64_t) data[5] << 16) | ((uint64_t) data[4] << 24)
	 | ((uint64_t) data[3] << 32) | ((uint64_t) data[2] << 40)
	 | ((uint64_t) data[1] << 48) | ((uint64_t) data[0] << 56);
}

uint64_t read_be64_2 (struct uint64_st data)
{
  return (uint64_t) data.u7 | ((uint64_t) data.u6 << 8)
	 | ((uint64_t) data.u5 << 16) | ((uint64_t) data.u4 << 24)
	 | ((uint64_t) data.u3 << 32) | ((uint64_t) data.u2 << 40)
	 | ((uint64_t) data.u1 << 48) | ((uint64_t) data.u0 << 56);
}

uint64_t read_be64_3 (unsigned char *data)
{
  return (uint64_t) *(data + 7) | ((uint64_t) *(data + 6) << 8)
	 | ((uint64_t) *(data + 5) << 16) | ((uint64_t) *(data + 4) << 24)
	 | ((uint64_t) *(data + 3) << 32) | ((uint64_t) *(data + 2) << 40)
	 | ((uint64_t) *(data + 1) << 48) | ((uint64_t) *data << 56);
}

/* { dg-final { scan-tree-dump-times "64 bit load in target endianness found at" 3 "bswap" } } */
/* { dg-final { scan-tree-dump-times "64 bit bswap implementation found at" 3 "bswap" } } */
