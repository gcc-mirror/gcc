/* { dg-do run } */

unsigned char a;
int b = 1, c, d;
int __attribute__((noipa))
f()
{
  char e;
  c = b - c;
  a = ~(c || a);
  e = -(b ^ a);
  d = e && b;
  a = ~(b & a);
  if (a < 2)
    return 1;
  return 0;
}

int main()
{
  if (f())
    __builtin_abort();
}

