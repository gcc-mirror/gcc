// Test that we properly evaluate the object parameter when accessing static
// members.

struct A {
  static void f () {}
  static int i;
};

int A::i;

int c = 0;

A g ()
{
  ++c;
  return A();
}

int main ()
{
  g().f();
  g().i = 42;
  return (c != 2);
}
