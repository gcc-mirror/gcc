static inline int
mod (int a, int n)
{
  return a >= n ? a % n : a;
}
void dpara(int);
void opticurve (int m)
{
  int i;
  for (i = 0; i < m; i++)
    {
        dpara(mod (i - 1, m));
    }
}
