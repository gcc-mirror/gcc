extern void abort (void);

int
foo (int x)
{
  if ((int) (x & 0x80ffffff) != (int) (0x8000fffe))
    abort ();

  return 0;
}

int
main ()
{
  return foo (0x8000fffe);
}
