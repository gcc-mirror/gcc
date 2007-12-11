/* { dg-do compile } */

extern int ReadBlobByte (void);

void ReadRLEImage (unsigned char *p)
{
  unsigned char background_color[4] = { 0, 1, 2, 3 };
  long j;

  unsigned long number_planes = ReadBlobByte();

  for (j = 0; j < (long) number_planes; j++)
    *p++ = background_color[j];
}

/* { dg-final { cleanup-tree-dump "vect" } } */
