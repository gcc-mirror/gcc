void abort (void);
void exit (int);

int * foo (int *x, int b)
{

  *(x++) = 55;
  if (b)
    *(x++) = b;

  return x;
}

int
main(void)
{
  int a[5];

  __builtin_memset (a, 1, sizeof (a));

  if (foo(a, 0) - a != 1 || a[0] != 55 || a[1] != a[4])
    abort();

  __builtin_memset (a, 1, sizeof (a));

  if (foo(a, 2) - a != 2 || a[0] != 55 || a[1] != 2)
    abort();

  exit (0);
}
