void abort (void);
void exit (int);

int v = 3;

void
f (void)
{
  int v = 4;
  {
    extern int v;
    if (v != 3)
      abort ();
  }
}

int
main (void)
{
  f ();
  exit (0);
}
