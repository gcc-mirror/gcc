void abort (void);
void exit (int);

double f (double a) {}
double (* const a[]) (double) = {&f};

int
main (void)
{
  double (*p) ();
  p = &f;
  if (p != a[0])
    abort ();
  exit (0);
}
