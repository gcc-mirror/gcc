typedef unsigned uint;

inline
g (uint *s, uint *d, uint c)
{
  while (c != 0)
    {
      *--d = *--s;
      c--;
    }
}

f (uint *p1, uint c, uint *p2)
{
  while (c > 0 && *p1 == 0)
    {
      p1++;
      c--;
    }
  if (c == 0)
    return 1;
  g (p2, p1, c);
}
