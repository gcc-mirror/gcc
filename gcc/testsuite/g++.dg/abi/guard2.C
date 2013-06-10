// PR c++/41611
// Test that the guard gets its own COMDAT group.
// { dg-final { scan-assembler "_ZGVZN1A1fEvE1i,comdat" { target *-*-linux* *-*-gnu* } } }

struct A {
  static int f()
  {
    static int &i = *new int();
    return i;
  }
};

int main()
{
  return A::f();
}
