/* { dg-do run } */
/* { dg-options "-O3 -floop-unroll-and-jam --param unroll-jam-min-percent=0" } */
int x[1024], y[1024];

void __attribute__((noipa)) foo ()
{
  for (int i = 0; i < 1024; ++i)
    {
      x[i] = 0;
      for (int j = 0; j < 1024; ++j)
        if (!y[j])
          x[i] = 1;
    }
}

int main()
{
  y[1023] = 1;
  foo ();
  if (x[1] != 1)
    __builtin_abort ();
  return 0;
}
