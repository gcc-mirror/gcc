/* { dg-options "-fgnu89-inline" } */

extern void abort (void);
extern void exit (int);

double d;

__inline__ double foo (void)
{
  return d;
}

__inline__ int bar (void)
{
  foo();
  return 0;
}

int main (void)
{
  if (bar ())
    abort ();
  exit (0);
}
