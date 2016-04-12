/* { dg-do compile } */
/* { dg-options "-O1 -floop-nest-optimize" } */

unsigned int z4, pz;
int nn[2];

static unsigned int
xq (unsigned int dj)
{
  return dj > 1 ? z4 : z4 + dj;
}

void
la (void)
{
  int hd, dl;
  unsigned int hn = 0;

  for (hd = 0; hd < 2; ++hd)
    {
      for (dl = 0; dl < 2; ++dl)
	nn[dl] = 0;
      --hn;
      pz = xq (hn);
    }
}
