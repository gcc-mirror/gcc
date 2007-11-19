int test1 (int b, int c)
{
  char x;
  if (b)
    return x / c;
  else
    return 1;
}
int test2 (int b, int c)
{
  int x;
  if (b)
    return x * c;
  else
    return 1;
}
int test3 (int b, int c)
{
  int x;
  if (b)
    return x % c;
  else
    return 1;
}
int test4 (int b, int c)
{
  char x;
  if (b)
    return x == c;
  else
    return 1;
}

extern void abort (void);
int main()
{
  if (test1(1, 1000) != 0)
    abort ();
  if (test2(1, 0) != 0)
    abort ();
  if (test3(1, 1) != 0)
    abort ();
  if (test4(1, 1000) != 0)
    abort ();
  return 0;
}

