/* { dg-do compile } */
/* { dg-options "-O2 -march=z196" } */

extern long useme (long, ...);

void
foo (void)
{
  long secs = useme (41);
  long utc_secs = useme (42);
  long h, m;

  utc_secs = useme (42);
  h = secs / 3600;
  m = secs / 60;
  if (utc_secs >= 86400)
    {
      m = 59;
      h--;
      if (h < 0)
	h = 23;
    }
  useme (h, m);
}
