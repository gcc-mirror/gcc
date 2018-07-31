// c++/84306
// { dg-do link { target c++11 } }

struct foo {
  foo() = default;

  foo(foo const&);

  template<typename T>
  explicit foo(T&&) { }
};

int
main()
{
  foo f1;
  foo f2{f1};
}
