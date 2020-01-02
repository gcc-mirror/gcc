/* { dg-do compile } */

void
f (int *x, short *y, int z)
{
  for (int i = 0; i < 0x82; ++i)
    x[-i] += x[z * i];
}
