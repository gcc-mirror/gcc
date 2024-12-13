/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-flto" } } */

/* We don't detect this case, because there are many conditional branches.
    Even if we detected it won't be verified,
    because we would get more than two execution paths (states).  */

#include <stdint.h>

typedef uint8_t byte;
byte Compute_CRC8_Simple_OneByte_ShiftReg (byte byteVal)
{
  const byte generator = 0x1D;
  byte crc = 0; /* init crc register with 0 */
  byte b = byteVal;
  for (int i = 7; i >= 0; i--)
    {
      /* check if MSB is set */
      if ((crc & 0x80) != 0)
	{   /* MSB set, shift it out of the register */
	  crc = (byte) (crc << 1);
	  /* shift in next bit of input stream:
	   * If it's 1, set LSB of crc to 1.
	   * If it's 0, set LSB of crc to 0. */
	  crc = ((byte) (b & (1 << i)) != 0) ? (byte) (crc | 0x01)
					     : (byte) (crc & 0xFE);
	  /* Perform the 'division' by XORing the crc register with the generator polynomial */
	  crc = (byte) (crc ^ generator);
	}
      else
	{   /* MSB not set, shift it out and shift in next bit of input stream. Same as above, just no division */
	  crc = (byte) (crc << 1);
	  crc = ((byte) (b & (1 << i)) != 0) ? (byte) (crc | 0x01)
					     : (byte) (crc & 0xFE);
	}
    }
  return crc;
}
