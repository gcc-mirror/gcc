/* { dg-do run } */

int __attribute__((noipa,noinline))
foo (int i)
{
  int a[2];
  a[1] = 1;
  int j = a[1];
  int *p = &a[0];
  p[i] = 0;
  return a[j];
}

int
main()
{
  if (foo (1) != 0)
    __builtin_abort ();
  return 0;
}
