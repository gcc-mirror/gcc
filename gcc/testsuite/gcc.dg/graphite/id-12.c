void
foo (unsigned short x[])
{
  int i;
  unsigned short *p = &x[2];
  if (*p)
    x += 2;
  for (i = 2; i < 9; i++, ++x)
    *x >>= 8;
}
