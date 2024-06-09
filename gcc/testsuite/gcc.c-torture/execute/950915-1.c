void abort (void);
void exit (int);

long int a = 100000;
long int b = 21475;

long
f ()
{
  return ((long long) a * (long long) b) >> 16;
}

int
main (void)
{
  if (f () < 0)
    abort ();
  exit (0);
}
