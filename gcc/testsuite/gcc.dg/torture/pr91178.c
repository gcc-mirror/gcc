/* { dg-do compile } */

int a;
extern int f[10][91125];
int b[50];
void c()
{
  for (int d = 6; d <= a; d++)
    for (int e = 16; e <= 24; e++)
      b[e] -= f[d][d];
}
