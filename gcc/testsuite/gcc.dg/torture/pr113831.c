/* { dg-do run } */

int a[3];
int __attribute__((noipa))
foo(int i, int x)
{
  int tem = 0;
  a[2] = x;
  if (i < 1)
    ++i;
  else
    {
      ++i;
      tem = a[i];
    }
  tem += a[i];
  return tem;
}

int
main()
{
  if (foo (0, 7) != 0)
    __builtin_abort();
  return 0;
}
