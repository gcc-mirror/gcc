/* { dg-require-effective-target trampolines } */

int f0(int (*fn)(int *), int *p)
{
  return (*fn) (p);
}

int f1(void)
{
  int i = 0;

  int f2(int *p)
  {
    i = 1;
    return *p + 1;
  }

  return f0(f2, &i);
}

int main()
{
  if (f1() != 2)
    abort ();

  return 0;
}
