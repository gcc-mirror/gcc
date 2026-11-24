/* { dg-do run } */

unsigned g[4];
struct T { unsigned u; };

unsigned f0(unsigned *p, int n, unsigned k)
{
  if (n <= 0)
    return k;
  return *p ^ f0(&g[3], n - 1, 0);
}

unsigned f1(int n, unsigned k, struct T t)
{
  return f0(&g[n & 3], n, k) >> (t.u & 7);
}

int main()
{
  unsigned h = 1;
  for (int n = -1; n <= 7; n++)
    h = h ^ f0(&g[1], n, 3) ^ f1(n, 85, (struct T){(unsigned)n - 2});
  if (h != 2)
    __builtin_abort ();
  return 0;
}
