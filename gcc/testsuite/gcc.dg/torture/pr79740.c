/* { dg-do compile } */

int a;
short b;
short fn1(unsigned short p1) { return p1 << a; }

int main()
{
  short c;
  int d = 4;
  for (; b;)
    {
      c = d + 1;
      fn1(c);
      d = 0;
    }
  d++;
  return 0;
}
