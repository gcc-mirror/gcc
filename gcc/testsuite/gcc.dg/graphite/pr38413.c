/* { dg-options "-O2 -fgraphite-identity -Wno-old-style-definition" } */

static int qsz;

void specqsort(base, n, size, compar)
     int n, size, compar;
     char *base;
{
  register char c, *i, *j, *lo, *hi;
  qsz = size;
  for (i = base, hi = base + qsz; i < hi; ) 
    {
      *i++ = c;
    }
}
