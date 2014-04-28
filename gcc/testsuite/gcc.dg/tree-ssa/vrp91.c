/* { dg-do compile } */
/* { dg-options "-S -O2 -fdump-tree-vrp2" } */

unsigned short data;
void foo ()
{
  unsigned char  x16;
  unsigned int i;
  for (i = 0; i < 8; i++)
    {
      x16 = data & 1;
      data >>= 1;
      if (x16 == 1)
	{
	  data ^= 0x4;
	}
      data >>= 1;
    }
}

/* { dg-final { scan-tree-dump "\\\[0, 7\\\]" "vrp2" } } */
/* { dg-final { cleanup-tree-dump "vrp2" } } */
