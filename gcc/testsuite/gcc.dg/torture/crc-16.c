/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-flto" } } */

/* A test from busybox - we don't verify, as unsigned is used for the
  "expected" variable, but 16-bit CRC is calculated.  We verify only those cases
  when CRC variable's size and calculated CRC are equal. In the algorithm we don't
  check whether "expected" variable's only low half is used.  */
int receive(/*int read_fd, */int file_fd)
{
  /* Initialization is not the same as in Busybox.  */
  unsigned blockLength = 13;
  unsigned char blockBuf[1024] = "sgdfsgdfsgdfs";
  int cksum_or_crc = 0x4561;

  unsigned expected;
  int i, j;
 /* ... */
  expected = 0;
      for (i = 0; i < blockLength; i++)
	{
	  expected = expected ^ blockBuf[i] << 8;
	  for (j = 0; j < 8; j++)
	    {
	      if (expected & 0x8000)
		expected = (expected << 1) ^ 0x1021;
	      else
		expected = (expected << 1);
	    }
	}
      expected &= 0xffff;

  if (cksum_or_crc != expected) {
      /* ... */
      return 1; // was - goto timout
    }

  /* ... */
  return -1;
}
