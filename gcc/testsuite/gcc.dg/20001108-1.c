/* { dg-do run } */
/* { dg-options "-O2 -fomit-frame-pointer" } */

void die (long) __attribute__ ((noreturn));

void die (long e)
{
  abort ();
  for (;;);
}

long foo (double i)
{
  if (i != 2.0)
    abort ();
  return 26;
}

long bar (long i, double x)
{
  if (x < 0) die (1);
  return foo (x);
}

main()
{
  if (bar (0, 2.0) != 26)
    abort ();
  exit (0);
}
