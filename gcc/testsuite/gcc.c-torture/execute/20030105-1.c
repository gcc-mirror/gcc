int __attribute__ ((noinline))
foo ()
{
  const int a[8] = { 0, 1, 2, 3, 4, 5, 6, 7 };
  int i, sum;

  sum = 0;
  for (i = 0; i < sizeof (a) / sizeof (*a); i++)
    sum += a[i];

  return sum;
}

int
main ()
{
  if (foo () != 28)
    abort ();
  exit (0);
}
