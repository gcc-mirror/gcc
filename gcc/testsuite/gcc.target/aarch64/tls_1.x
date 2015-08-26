void abort (void);

__thread int t0 = 0x10;
__thread int t1 = 0x10;

int
main (int argc, char **argv)
{
  if (t0 != t1)
    abort ();

  return  0;
}

