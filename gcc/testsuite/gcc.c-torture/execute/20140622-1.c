unsigned p;

long __attribute__((noinline, noclone))
test (unsigned a)
{
  return (long)(p + a) - (long)p;
}

int
main ()
{
  p = (unsigned) -2;
  if (test (0) != 0)
    __builtin_abort ();
  if (test (1) != 1)
    __builtin_abort ();
  if (test (2) != -(long)(unsigned)-2)
    __builtin_abort ();
  p = (unsigned) -1;
  if (test (0) != 0)
    __builtin_abort ();
  if (test (1) != -(long)(unsigned)-1)
    __builtin_abort ();
  if (test (2) != -(long)(unsigned)-2)
    __builtin_abort ();
  return 0;
}
