long val = 0x5e000000;

long
f1 (void)
{
  return 0x132;
}

long
f2 (void)
{
  return 0x5e000000;
}

void
f3 (long b)
{
  val = b;
}

void
f4 ()
{
  long v = f1 ();
  long o = f2 ();
  v = (v & 0x00ffffff) | (o & 0xff000000);
  f3 (v);
}

main ()
{
  f4 ();
  if (val != 0x5e000132)
    abort ();
  exit (0);
}
