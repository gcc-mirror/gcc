// PR c++/34870

template <typename T>
struct Foo
{
  friend void func(const Foo &) {}
};

void check(const Foo<int> & x)
{
  func(x);
}
