void abort (void);
void exit (int);

int a3[3];

void f(int *a)
{
  int i;

  for (i=3; --i;)
    a[i] = 42 / i;
}

int
main ()
{
  f(a3);

  if (a3[1] != 42 || a3[2] != 21)
    abort ();

  exit (0);
}
