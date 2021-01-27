/* { dg-do compile } */

double a[1024];

int bar();
void foo (int n)
{
  double x = 0, y = 0;
  int i = 1023;
  do
    {
      x += a[i] + a[i+1];
      y += a[i] / a[i+1];
      if (bar ())
        break;
    }
  while (--i);
  /* We want to avoid vectorizing the LC PHI and insert vector CTORs
     inside of the loop where it is only needed here.  */
  a[0] = x;
  a[1] = y;
}

/* { dg-final { scan-tree-dump-not "vectorizing SLP node starting from: ._\[0-9\]+ = PHI" "slp1" } } */
