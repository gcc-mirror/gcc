extern double sqrt (double);
extern void abort (void);
int once;

double foo (void)
{
  if (once++)
    abort ();
  return 0.0 / 0.0;
}

double x;
int main (void)
{
  x = sqrt (foo ());
  return 0;
}
