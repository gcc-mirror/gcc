void abort (void);
void exit (int);

int a = 1;
int b = -1;

int c = 1;
int d = 0;

int
main (void)
{
  double e;
  double f;
  double g;

  f = c;
  g = d;
  e = (a < b) ? f : g;
  if (e)
    abort ();
  exit(0);
}

