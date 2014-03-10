// Origin PR c++/48574
// { dg-do compile { target c++11 } }

struct A
{
  virtual int foo();
};

void baz (int);

template <typename T>
void
bar(T x)
{
  A &b = *x;
  baz (b.foo ());
}

void
foo()
{
  A a;
  bar(&a);
}
