long
f (long x)
{
  return x / (-0x7fffffffL - 1L);
}

main ()
{
  if (f (-1L) != 0L || f (0x7fffffffL) != 0L || f (-0x7fffffffL - 1L) != 1l)
    abort ();
  exit (0);
}
