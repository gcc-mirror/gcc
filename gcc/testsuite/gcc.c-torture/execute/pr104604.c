/* PR tree-optimization/104604 */

unsigned char g;

__attribute__((noipa))
unsigned char
foo (_Complex unsigned c)
{
  unsigned char v = g;
  _Complex unsigned t = 3;
  t /= c;
  return v + t;
}

__attribute__((noipa))
unsigned char
bar (_Complex unsigned c)
{
  unsigned char v = g;
  _Complex unsigned t = 42;
  t /= c;
  return v + t;
}

int
main ()
{
  unsigned char x = foo (7);
  if (x)
    __builtin_abort ();
  if (bar (7) != 6)
    __builtin_abort ();
  return 0;
}
