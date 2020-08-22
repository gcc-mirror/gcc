// P0634R3
// { dg-do compile { target c++20 } }

template<class T>
void f(int i)
{
  T::x * i; // { dg-error "dependent-name" }
}

struct Foo {
  typedef int x;
};

struct Bar {
  static int const x = 5;
};

int
main ()
{
  f<Bar>(1);
  f<Foo>(1);
}
