/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-crc -march=rv64gc_zbc" } */
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
/* { dg-final { scan-tree-dump "Compute_CRC8_Simple_OneByte_ShiftReg function maybe calculates CRC and returns it." "crc"} } */
/* { dg-final { scan-tree-dump "Return size is 8" "crc"} } */
/* { dg-final { scan-tree-dump "Loop iteration number is 7" "crc"} } */
/* { dg-final { scan-tree-dump "Bit forward" "crc"} } */


