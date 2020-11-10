/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

long a[6];
void d(int c)
{
  for (; c; c++)
    for (int b = 0; b < 8; b++)
      ((char *)&a[c])[b] = c;
}
