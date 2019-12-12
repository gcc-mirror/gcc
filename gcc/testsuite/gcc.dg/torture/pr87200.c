/* { dg-do compile } */

unsigned long long int ry;

int
gl (void)
{
  long long int my = 0;
  unsigned long long int *oi = (unsigned long long int *) &my;
  int s9;

  s9 = !!gl () ? ry : 0;
  if (s9 != 0)
    oi = &ry;
  else
    {
      my = ry;
      *oi += my;
    }

  return *oi;
}

