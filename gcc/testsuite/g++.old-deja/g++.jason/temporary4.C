// Bug: g++ initializes both B::i and B::j before destroying any temps.

extern "C" int printf (const char *, ...);

int c = 0;
int d = 0;
int r = 0;

struct A {
  A() { if (c != d) r = 1; ++c; }
  A(const A&);  // declare so g++ returns A on the stack
  ~A() { ++d; }
  operator int () { return 0; }
};

A foo ()
{
  return A();
}

struct B {
  int i;
  int j;
  B(): i(foo()), j(foo()) { }
};

int main()
{
  B b;
  return r;
}
