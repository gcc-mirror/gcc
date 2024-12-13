/* { dg-do run } */
/* { dg-options "-fdump-tree-crc-details" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-g" "-Os" "-funroll-loops" "-fpeel-loops"  "-flto" } } */
/* { dg-require-effective-target int32plus } */

//Test from roms/u-boot-sam460ex/drivers/mtd/ubi/crc32.c
#include <stdint.h>
#include <stdlib.h>

typedef uint8_t u8;
typedef uint32_t u32;
#define CRCPOLY_LE 0xedb88320

__attribute__ ((noinline,optimize(0)))
u32 crc32_le_O0 (u32 crc, unsigned char const *p, size_t len)
{
  int i;
  while (len--) {
      crc ^= *p++;
      for (i = 0; i < 8; i++)
	crc = (crc >> 1) ^ ((crc & 1) ? CRCPOLY_LE : 0);
    }
  return crc;
}

u32 crc32_le (u32 crc, unsigned char const *p, size_t len)
{
  int i;
  while (len--) {
      crc ^= *p++;
      for (i = 0; i < 8; i++)
	crc = (crc >> 1) ^ ((crc & 1) ? CRCPOLY_LE : 0);
    }
  return crc;
}


int main ()
{
  u32 crc = 0x0D800D80;
  unsigned char st[2] = {'H','i'};
  for (unsigned char i = 0; i < 255; i++)
    {
      st[0] = i;
      u32 res1 = crc32_le_O0 (crc, st, 2);
      u32 res2 = crc32_le (crc, st, 2);
      if (res1 != res2)
	__builtin_abort ();
      crc = res1;
    }
  __builtin_exit (0);
}
/* { dg-final { scan-tree-dump "function maybe contains CRC calculation." "crc" } } */
/* { dg-final { scan-tree-dump "Loop iteration number is 7" "crc"} } */
/* { dg-final { scan-tree-dump "Bit reversed" "crc"} } */
/* { dg-final { scan-tree-dump "calculates CRC!" "crc"} } */
/* { dg-final { scan-tree-dump-times "Couldn't generate faster CRC code." 0 "crc"} } */
