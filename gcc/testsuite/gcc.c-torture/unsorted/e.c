foo (short a, int *p, short *s)
{
  int i;
  for (i = 10;  i >= 0; i--)
    {
      a = (short) bar ();
      p[i] = a;
      s[i] = a;
    }
}
