/*  We used to simplify these incorrectly.  */
__attribute__((noipa))
long long
foo (unsigned int x)
{
  int y = x;
  y = ~y;
  return ((long long) x) & y;
}

__attribute__((noipa))
long long
foo_v (volatile unsigned int x)
{
  volatile int y = x;
  y = ~y;
  return ((long long) x) & y;
}

__attribute__((noipa))
long long
bar (unsigned int x)
{
  int y = x;
  y = ~y;
  return ((long long) x) ^ y;
}

__attribute__((noipa))
long long
bar_v (volatile unsigned int x)
{
  volatile int y = x;
  y = ~y;
  return ((long long) x) ^ y;
}

__attribute__((noipa))
long long
baz (unsigned int x)
{
  int y = x;
  y = ~y;
  return y ^ ((long long) x);
}

__attribute__((noipa))
long long
baz_v (volatile unsigned int x)
{
  volatile int y = x;
  y = ~y;
  return y ^ ((long long) x);
}


int main()
{
  for(int t = -1; t <= 1; t++)
    {
      if (foo(t) != foo_v(t))
        __builtin_abort ();
      if (bar(t) != bar_v(t))
        __builtin_abort ();
      if (baz(t) != baz_v(t))
        __builtin_abort ();
    }
}
