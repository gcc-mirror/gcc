/* MIN of two vector counts with a value at zero.  */
/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

void
fclz (unsigned *a, unsigned *b, int *r, int *s, int n)
{
  for (int i = 0; i < n; i++)
    {
      int ca = __builtin_clzg (a[i], 32);
      int cb = __builtin_clzg (b[i], 32);
      r[i] = ca < cb ? ca : cb;
      s[i] = ca + cb;
    }
}

void
fctz (unsigned *a, unsigned *b, int *r, int *s, int n)
{
  for (int i = 0; i < n; i++)
    {
      int ca = __builtin_ctzg (a[i], 32);
      int cb = __builtin_ctzg (b[i], 32);
      r[i] = ca < cb ? ca : cb;
      s[i] = ca + cb;
    }
}
