/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details -O2" } */

// File - callerid

unsigned short calc_crc(unsigned short crc, unsigned char data)
{
  unsigned int i, j, org, dst;
  org = data;
  dst = 0;

  for (i = 0; i < 8; i++) {
    org <<= 1;
    dst >>= 1;
    if (org & 0x100)
      dst |= 0x80;
  }
  data = (unsigned char) dst;
  crc ^= (unsigned int) data << (16 - 8);
  for (j = 0; j < 8; j++) {
    if (crc & 0x8000U)
      crc = (crc << 1) ^ 0x1021U ;
    else
      crc <<= 1 ;
  }
  return crc;
}

/* { dg-final { scan-tree-dump "calculates CRC!" "crc" } } */
