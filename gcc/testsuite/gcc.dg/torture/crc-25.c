/* { dg-do run } */
/* { dg-options "-fdump-tree-crc-details" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-g" "-Os" "-funroll-loops" "-fpeel-loops" "-flto" } } */
/* { dg-require-effective-target int32plus } */

//Test from roms/u-boot-sam460ex/drivers/net/mpc512x_fec.c(fsl_mcdmafec.c)

#include <stdint.h>
#include <stdlib.h>

typedef uint8_t u8;
typedef uint32_t u32;

__attribute__ ((noinline,optimize(0)))
u32 mpc512x_fec_set_hwaddr_O0 (unsigned char *mac)
{
  u8 currByte;                    /* byte for which to compute the CRC */
  int byte;                       /* loop - counter */
  int bit;                        /* loop - counter */
  u32 crc = 0xffffffff;           /* initial value */

  for (byte = 0; byte < 6; byte++) {
      currByte = mac[byte];
      for (bit = 0; bit < 8; bit++) {
	  if ((currByte & 0x01) ^ (crc & 0x01)) {
	      crc >>= 1;
	      crc = crc ^ 0xedb88320;
	    } else {
	      crc >>= 1;
	    }
	  currByte >>= 1;
	}
    }

  crc = crc >> 26;
  return crc;
}

u32 mpc512x_fec_set_hwaddr (unsigned char *mac)
{
  u8 currByte;                    /* byte for which to compute the CRC */
  int byte;                       /* loop - counter */
  int bit;                        /* loop - counter */
  u32 crc = 0xffffffff;           /* initial value */

  for (byte = 0; byte < 6; byte++) {
      currByte = mac[byte];
      for (bit = 0; bit < 8; bit++) {
	  if ((currByte & 0x01) ^ (crc & 0x01)) {
	      crc >>= 1;
	      crc = crc ^ 0xedb88320;
	    } else {
	      crc >>= 1;
	    }
	  currByte >>= 1;
	}
    }

  crc = crc >> 26;
  return crc;
}

int main ()
{
  unsigned char st[6] = "Hello";
  for (unsigned char i = 0; i < 255; i++)
    {
      st[0] = i;
      u32 res1 = mpc512x_fec_set_hwaddr_O0 (st);
      u32 res2 = mpc512x_fec_set_hwaddr (st);
      if (res1 != res2)
	__builtin_abort ();
    }
  __builtin_exit (0);
}
/* { dg-final { scan-tree-dump "function maybe contains CRC calculation." "crc" } } */
/* { dg-final { scan-tree-dump "Loop iteration number is 7" "crc"} } */
/* { dg-final { scan-tree-dump "Bit reversed" "crc"} } */
/* { dg-final { scan-tree-dump "calculates CRC!" "crc"} } */
/* { dg-final { scan-tree-dump-times "Couldn't generate faster CRC code." 0 "crc"} } */
