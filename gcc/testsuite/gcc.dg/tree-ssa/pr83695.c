/* { dg-do compile } */
/* { dg-options "-O3" } */

int a[3][3][3], b, d;
short c;
unsigned char e;

static void f ()
{
  for (c = 0; c < 2; c++)
      for (e = 0; e < 3; e++)
        for (b = 0; b < 3; b++)
          a[b][e][b] = 0;
  while (1)
    ;
}

int main ()
{
  if (d)
    f ();
  return 0;
}
