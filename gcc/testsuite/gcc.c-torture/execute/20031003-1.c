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

int f3()
{
  float a = 2147483648.0f;
  return (int)a;
}

int f4()
{
  int a = 2147483647;
  float b = (float)a;
  return (int)b;
}

int main()
{
  if (f1() != 2147483647)
    abort ();
  if (f2() != 2147483647)
    abort ();
#ifdef __OPTIMIZE__
  if (f3() != 2147483647)
    abort ();
  if (f4() != 2147483647)
    abort ();
#endif
  return 0;
}

