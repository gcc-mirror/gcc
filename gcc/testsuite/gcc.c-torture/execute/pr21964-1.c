void abort (void);
void exit (int);

void
foo (int n, int m)
{
  if (m == 0)
    exit (0);
  else if (n != 0)
    abort ();
  else
    foo (n++, m - 1);
}
 
int
main (void)
{
  foo (0, 4);
}
