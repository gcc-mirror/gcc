/* { dg-do compile } */

int a[100][70304];
int b[100];
void c()
{
  for (int d = 2; d < 4; d++)
    for (int e = 2; e <= 50; e++)
      for (int f = 32; f <= 38; f++)
	b[d + f] -= a[e][5];
}
