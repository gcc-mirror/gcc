static int __attribute__((noinline)) ATTRIBUTE
bar (int x)
{
  return x + 3;
}

int __attribute__((noinline)) ATTRIBUTE
foo (int y)
{
  return y + bar (y);
}

int ATTRIBUTE
main (void)
{
  return !(foo (5) == 13);
}
