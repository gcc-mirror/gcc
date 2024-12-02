/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details -w" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-flto" } } */

/* We don't detect, it's optimized to branch-less CRC.  */
#define CRC32_POLY_LE 0xedb88320
typedef unsigned int u32;
u32 calc_crc(unsigned char *buf, int len)
{
  u32 reg;
  u32 tmp;
  int j, k;

  reg = 0xffffffff;

  for (j = 0; j < len; j++) {
      reg ^= buf[j];

      for (k = 0; k < 8; k++) {
	  tmp = reg & 0x01;

	  reg >>= 1;

	  if (tmp)
	    reg ^= CRC32_POLY_LE;
	}
    }

  return ~reg;
}
