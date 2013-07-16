/* { dg-do run } */
/* { dg-options "-O2 -mtune=athlon64" } */

extern void abort (void);

long double
__attribute__((noinline, noclone))
test (float num)
{
  unsigned int i;

  if (num < 0.0)
    num = 0.0;

  __builtin_memcpy (&i, &num, sizeof(unsigned int));

  return (long double)(unsigned long long) i;
}

int
main ()
{
  long double x;

  x = test (0.0);

  if (x != 0.0)
    abort ();

  return 0;
}
