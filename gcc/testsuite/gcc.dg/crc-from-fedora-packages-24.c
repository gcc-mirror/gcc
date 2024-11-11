/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details -O2" } */
/* { dg-require-effective-target int32plus } */

// File - utils.ii
/* We don't support the case when CRC variable's size is different from the
   calculated CRC (I.e. in this case CRC 16 is calculated.)  */

void crc_byte(const char data, unsigned int *crc16)
{
  int k;
  unsigned c,d ;

  c = data << 8 ;
  d = c;

  for (k = 0; k < 8; k++) {
      *crc16 = (c & 0x8000) ^ *crc16;

      if (*crc16 & 0x8000) {
	  *crc16 = *crc16 << 1;
	  *crc16 = *crc16 ^ 0x8005;
	} else
	*crc16 = *crc16 << 1;

      d = d << 1;
      c = d;
    }
}
/* { dg-final { scan-tree-dump "maybe contains CRC calculation." "crc" } } */
