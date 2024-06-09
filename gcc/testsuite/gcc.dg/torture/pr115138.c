/* { dg-do run } */

int foo (int) {}
int bar (int) {}

typedef int (*pred)(int);

int x, y;
pred A () { if (x) return foo; else return bar; }
pred B () { if (y) return foo; else return bar; }
int __attribute__((noipa)) baz()
{
  pred a = A();
  pred b = B();
  if (a != b)
    return 42;
  return 0;
}

int main()
{
  if (baz () != 0)
    __builtin_abort ();
  y = 1;
  if (baz () != 42)
    __builtin_abort ();
  return 0;
}
