/* { dg-do compile } */

int a, b[3], c[3][5];

void
fn1 ()
{
  int e;
  for (a = 2; a >= 0; a--)
    for (e = 0; e < 4; e++)
      c[a][e] = b[a];
}
