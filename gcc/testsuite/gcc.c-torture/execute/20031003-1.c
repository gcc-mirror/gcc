/* PR optimization/9325  */

extern void abort (void);

int f1()
{
  return (int)2147483648.0f;
}

int f2()
{
  return (int)(float)(2147483647);
}

int main()
{
  if (f1() != 2147483647)
    abort ();
  if (f2() != 2147483647)
    abort ();
  return 0;
}

