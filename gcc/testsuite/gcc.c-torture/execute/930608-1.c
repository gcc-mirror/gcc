double f (double a) {}
double (* const a[]) (double) = {&f};

main ()
{
  double (*p) ();
  p = &f;
  if (p != a[0])
    abort ();
  exit (0);
}
