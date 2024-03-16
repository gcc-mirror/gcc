void abort (void);
void exit (int);

char a[10] = "deadbeef";

char
acc_a (long i)
{
  return a[i-2000000000L];
}

int
main (void)
{
  if (acc_a (2000000000L) != 'd')
    abort ();
  exit (0);
}
