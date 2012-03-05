/* { dg-do compile } */

int a, b, c, h;

int i[5][5];

void
fn1 ()
{
  int l = 0;

  for (a = 0; a <= 3; a++)
    for (b = 1; b >= 0; b -= 1)
      l |= i[0][b];
  c = l;
}
