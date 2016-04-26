/* { dg-require-effective-target int32plus } */
int __attribute__ ((noinline))
foo (void)
{
  return 123;
}

int __attribute__ ((noinline))
bar (void)
{
  int c = 1;
  c |= 4294967295 ^ (foo () | 4073709551608);
  return c;
}

int
main ()
{
  if (bar () != 0x83fd4005)
    __builtin_abort ();
}
