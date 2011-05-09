// Origin PR c++/48574
// { dg-options "-std=c++0x" }
// { dg-do compile }

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
