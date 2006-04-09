void __attribute__((noinline))
foo (int *p, int d1, int d2, int d3,
     short count, int s1, int s2, int s3, int s4, int s5)
{
  int n = count;
  while (n--)
    {
      *p++ = s1;
      *p++ = s2;
      *p++ = s3;
      *p++ = s4;
      *p++ = s5;
    }
}

int main()
{
  int x[10], i;

  foo (x, 0, 0, 0, 2, 100, 200, 300, 400, 500);
  for (i = 0; i < 10; i++)
    if (x[i] != (i % 5 + 1) * 100)
      abort ();
  exit (0);
}
