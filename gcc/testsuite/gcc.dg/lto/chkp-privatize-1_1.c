static int __attribute__((noinline))
f2 (int i)
{
  return 2 * i;
}

int __attribute__((noinline))
f1 (int i)
{
  return f2 (i) + 10;
}
