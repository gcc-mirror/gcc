#define B 95

foo (a, b, p)
     unsigned a, b;
     int *p;
{
  p[1] = a % B;
  p[0] = a / B;
}

bar (a, b, p)
     unsigned a, b;
     int *p;
{
  p[0] = a / B;
  p[1] = a % B;
}

