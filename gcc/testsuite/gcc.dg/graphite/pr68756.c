/* { dg-do compile } */
/* { dg-options "-O1 -floop-nest-optimize -fdump-tree-graphite-details --param graphite-allow-codegen-errors=1" } */

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

/* { dg-final { scan-tree-dump-times "code generation error" 1 "graphite" } } */
