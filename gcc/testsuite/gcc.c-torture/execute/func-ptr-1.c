void abort (void);
void exit (int);

static double f (float a);
static double (*fp) (float a);

int
main (void)
{
  fp = f;
  if (fp ((float) 1) != 1.0)
    abort ();
  exit (0);
}

static double
f (float a)
{
  return a;
}
