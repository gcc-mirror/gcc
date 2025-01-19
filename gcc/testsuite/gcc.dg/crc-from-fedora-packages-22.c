/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details -O2" } */

// File - crrcsim-LoggerReader_byte.ii
// We don't support this case.

int crcReg;
enum { crcPoly = 49 };
void crcByteSchritt(unsigned char data)
{
  for (int i=0; i<8; i++)
    {
      crcReg <<= 1;


      if ((data & 0x80) != 0)
	crcReg |= 1;

      data <<= 1;

      if ((crcReg & 0x100) != 0)
	crcReg = crcReg ^ crcPoly;
    }

  crcReg &= 0xFF;
}
