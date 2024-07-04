void abort (void);
void exit (int);

int
test (arg)
     int arg;
{
  if (arg > 0 || arg == 0)
    return 0;
  return -1;
}

int
main (void)
{
  if (test (0) != 0)
    abort ();
  if (test (-1) != -1)
    abort ();
  exit (0);
}
