void
pr98447_original (unsigned long *p, int r, int i)
{
  int b = 63, n = r % 16;

  while (i >= 0)
    {
      if (n > b)
        {
          p[i--] = b + 1 >= 64 ? 0UL : 1UL << (b + 1); /* { dg-bogus "-Wanalyzer-shift-count-overflow" } */
          b += 64;
        }
      b -= n;
    }
}

void
pr98477_comment_4 (unsigned long *p, int r, int i)
{
  int b = 64, n = r % 64;

  while (i >= 0 && b >= 0)
    {
      if (b <= n)
        p[i--] = 1UL << b; /* { dg-bogus "-Wanalyzer-shift-count-overflow" } */
      b -= n;
    }
}
