extern void abort (void);

int foo (int size)
{
  int a[size];
  a[size - 10] = 42;
  return a[size - 10] + size;
}

main()
{
  int x = foo (20);
  if (x != 62)
    abort ();
  return 0;
}
