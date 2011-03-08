// Origin PR c++/47957
// { dg-do compile }

struct S
{
  int m;

  S()
    : m(0)
  {
  }
};

struct Base
{
  typedef S T;
};

template<class T>
struct Derived : public Base
{
  int
  foo()
  {
    T a; // This is Base::T, not the template parameter.
    return a.m;
  }
};

int
main()
{
  Derived<char> d;
  return d.foo();
}
