void abort (void);
void exit (int);

volatile double a, *p;

int main ()
{
  double c, d;
  volatile double b;

  d = 1.0;
  p = &b;
  do
  {
    c = d;
    d = c * 0.5;
    b = 1 + d;
  } while (b != 1.0);

  a = 1.0 + c;
  if (a == 1.0)
    abort();

  exit (0);
}
