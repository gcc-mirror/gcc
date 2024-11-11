/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details -O2" } */
/* { dg-require-effective-target int32plus } */

// File - 3way.cpp. Modified
typedef unsigned int word32;
word32 ProcessAndXorBlock ()
{
  word32 a0, a1, a2;
  word32 START_D = 0xb1b1;
  word32 rc = START_D;
  unsigned int m_rounds = 32;
  for(unsigned i=0; i<m_rounds; i++)
  {
    //...
    rc <<= 1;
    if (rc&0x10000) rc ^= 0x11011;
  }
  return rc;
}

/* { dg-final { scan-tree-dump "Polynomial's all bits are zeros or the size of the polynomial is uncertain." "crc" } } */
/* { dg-final { scan-tree-dump "maybe contains CRC calculation." "crc" } } */
