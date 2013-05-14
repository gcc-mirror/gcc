// PR c++/53721
// { dg-require-effective-target c++11 }

struct A
{
  void f() {};
  auto g() -> decltype(this->f())
  {
  }
};
