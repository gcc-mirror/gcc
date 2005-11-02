extern void abort (void);

#define N	(1 << (sizeof(int) * __CHAR_BIT__ - 2))

int f(int n)
{
  if (-N <= n && n <= N-1)
    return 1;
  return 0;
}

int main ()
{
  if (f (N))
    abort ();
  return 0;
}
