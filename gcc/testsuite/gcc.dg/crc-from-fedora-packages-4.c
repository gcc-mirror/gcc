/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details -O2" } */

// crc.ii
typedef unsigned int u32;

void GenerateCRC32Table (u32 polynomial, u32 *table)
{
  for (u32 i = 0; i <= 255; i++)
    {
      u32 crc = i;
      for (u32 j = 0; j < 8; j++)
	{
	  crc = (crc >> 1) ^ ((crc & 1) ? 0x23428765 : 0);
	}
      table[i] = crc;
    }
}


/* { dg-final { scan-tree-dump "calculates CRC!" "crc" } } */
