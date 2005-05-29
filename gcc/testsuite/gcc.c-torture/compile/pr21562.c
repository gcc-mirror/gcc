/* { dg-options "-O3 -fno-inline" } */
struct foo { int a, b, c; };
void abort(void);
void exit(int);

void
brother (int a, int b, int c)
{
  if (a)
    abort ();
}

void
sister (struct foo f, int b, int c)
{
  brother ((f.b == b), b, c);
}

int
main ()
{
  struct foo f = { 7, 8, 9 };
  sister (f, 1, 2);
  exit (0);
}
