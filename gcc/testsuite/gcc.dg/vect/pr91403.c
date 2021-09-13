/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

extern int a[][1000000];
int b;
void c()
{
  for (int d = 2; d <= 9; d++)
    for (int e = 32; e <= 41; e++)
      b += a[d][5];
}
