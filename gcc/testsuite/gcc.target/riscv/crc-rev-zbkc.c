/* { dg-do compile { target rv64 } } */
/* { dg-options "-O2 -march=rv64gc_zbkc -mabi=lp64" } */

#define CRC64(BITS, TYPE, POLY)					\
  unsigned long							\
  crc##BITS (unsigned long crc, TYPE data)			\
  {								\
    return __builtin_rev_crc64_data##BITS (crc, data, POLY);	\
  }

#define POLY 0x42f0e1eba9ea3693UL

CRC64 (8, unsigned char, POLY)
CRC64 (16, unsigned short, POLY)
CRC64 (32, unsigned int, POLY)
CRC64 (64, unsigned long, POLY - 1)

/* Only the 32- and 64-bit updates use the carry-less multiply sequence.  */
/* { dg-final { scan-assembler-times {\mclmul\M} 2 } } */
/* { dg-final { scan-assembler-times {\mclmulh\M} 2 } } */
