// Origin PR c++/48574
// { dg-do compile }

struct A
{
  virtual void foo();
};

template <typename T>
void
bar(T x)
{
  A &b = *x;
  b.foo ();
}

void
foo()
{
  A a;
  bar(&a);
}
