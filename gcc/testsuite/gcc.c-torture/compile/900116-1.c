struct st {char a, b, c, d; }

zloop (struct st *s, int *p, int *q)
{
  int i;
  struct st ss;

  for (i = 0;  i < 100;  i++)
    {
      ss = s[i];
      p[i] = ss.c;
      q[i] = ss.b;
    }
}
