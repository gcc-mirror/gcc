// PR c++/79457
// { dg-do compile { target c++11 } }

template <typename T>
class Foo
{
private:
  T& goo;
  template <typename R>
  using S = decltype (goo[R ()]);

public:
  Foo (T& goo) : goo {goo} {}

  template <typename R>
  S<R> boo () {}
};

int
main ()
{
  int bar[] = {1, 2, 3};
  Foo<decltype (bar)> foo {bar};
}
