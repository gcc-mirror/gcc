/* { dg-do compile } */

int a, b, c, d;
short fn1(int p1, int p2) { return p1; }

int fn2(int p1) {}

int main()
{
  for (; c; c++)
    a |= fn1(1, a) | fn2(b |= d);
  return 0;
}
