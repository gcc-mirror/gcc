/* { dg-do run } */
/* { dg-options "-O3 -fno-tree-dce" } */

int a, b, c;
int main()
{
  int e = -1;
  short f = -1;
  for (; c < 1; c++)
    while (f >= e)
      f++;
  for (; a < 2; a++) {
    short g = ~(~b | ~f);
    int h = -g;
    int i = (3 / ~h) / ~b;
    b = i;
  }
  return 0;
}
