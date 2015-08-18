extern void abort (void) __attribute__ ((noreturn));

double __attribute__ ((noinline, noclone))
foo (unsigned int x)
{
  return (double) (float) (x | 0xffff0000);
}

int
main ()
{
  if (foo (1) != 0x1.fffep31)
    abort ();
  return 0;
}
