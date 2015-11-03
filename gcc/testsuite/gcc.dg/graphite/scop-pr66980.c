void foo(unsigned char *in, unsigned char *out, int w, int h)
{
  unsigned int i, j;
  for (i = 0; i < 3*w*h; i++)
    for (j = 0; j < 3*w*h; j++)
      out[i * w + j] = in[(i * w + j)*3] + in[(i * w + j)*3 + 1] + in[(i * w + j)*3 + 2];
}

/* Requires delinearization to be able to represent "i*w".  */
/* { dg-final { scan-tree-dump-times "number of SCoPs: 1" 1 "graphite" { xfail *-*-* } } } */
