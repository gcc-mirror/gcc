void abort (void);
void exit (int);

void
g (int i)
{
}

void
f (int i)
{
  g (0);
  while ( ({ i--; }) )
    g (0);
}

int
main (void)
{
  f (10);
  exit (0);
}
