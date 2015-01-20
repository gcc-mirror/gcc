/* { dg-options "-O1" } */

extern void fn1 (void); 

int a[2], b;

static void
foo (int p)
{
  b = 1 ^ a[(b ^ 1) & 1];
  b = 1 ^ a[b & 1];
  if (p)
    __builtin_abort ();
}

int
main ()
{
  foo (0);
  b = 0;
  foo (0);

  if (b != 1)
    __builtin_abort ();

  return 0;
}
