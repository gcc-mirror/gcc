/* { dg-do compile } */

_Bool a[2048];
int b[2048];

void
foo ()
{
  int i;
  for (i = 0; i < 2048; i += 4)
    {
      a[i] = b[i] <= 10;
      a[i + 3] = b[i + 1] <= 10;
      a[i + 2] = b[i + 2] <= 10;
      a[i + 1] = b[i + 3] <= 10;
    }
}

