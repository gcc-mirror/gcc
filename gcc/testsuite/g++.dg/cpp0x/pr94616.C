// PR c++/94616
// { dg-do compile { target c++11 } }

struct Bar {
  Bar(int n) { if (n > 0) throw 2; }
  ~Bar() {}
};

struct Foo {
  Bar b1 = 0;
  Bar b2 = 1;
  Foo() {}
  ~Foo() {}
};

int
main()
{
  try {
    Foo f;
  } catch(int) {
  }
}
