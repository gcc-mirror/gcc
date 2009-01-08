/* { dg-options "-O2 -floop-block" } */

int test()
{
  int offset, len;
  register char *mid;
  register char *midend;
  register char *bigend;
  long unsigned int curlen;
  if (offset + len > curlen) {
    while (midend > mid)
      *--bigend = *--midend;
  }
}
