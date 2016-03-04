/* { dg-do compile } */
/* { dg-options "-O3" } */

unsigned int a[64], b[64], c[64], d[64], e[64];

void
foo ()
{
  int i;
  for (i = 0; i < 64; i++)
    {
      d[i] = a[i];
      e[i] = ((b[i] < e[i]) != !c[i]) && !a[i];
    }
}
