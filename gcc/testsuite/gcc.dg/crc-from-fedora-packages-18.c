/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details -O2" } */
/* { dg-require-effective-target int32plus } */

// File - cryptobonrrxternd.i
// We don't support this case.

unsigned char PGP_input[2*100];
unsigned long crc24(int len) {

  if (sizeof (long) < 4)
    __builtin_exit (0);

  long crc;
  crc = 0xB704CE;
  int i,j;
  j = 0;
  while (len--) {
      crc ^= (PGP_input[j]) << 16;
      j++;
      for (i = 0; i < 8; i++) {
	  crc <<= 1;
	  if (crc & 0x1000000){
	      crc ^= 0x1864CFB;
	    }
	}
    }
  return crc & 0xFFFFFF;
}

/* { dg-final { scan-tree-dump "Polynomial's value is \\\{\[0, \]*0\\\}" "crc" } } */
/* { dg-final { scan-tree-dump "maybe contains CRC calculation." "crc" } } */
