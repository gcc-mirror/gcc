void
test (void)
{
  short *p, q[3];
  int x;

  p = q;
  for (x = 0; x < 3; x++)
    *p++ = 0;
}
