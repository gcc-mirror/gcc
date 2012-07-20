// PR c++/54026
// { dg-final { scan-assembler-not "rodata" } }

void non_const(int *);

template <typename T>
struct Foo {
  T x;
  mutable int y;
  void func() const { non_const(&y); }
};

struct Bar {
  int x;
  mutable int y;
  void func() const { non_const(&y); }
};

const Foo<int> foo = { 1, 2 };
const Bar bar = { 3, 4 };
