// PR c++/40371

struct A
{
  typedef void (&F)();
  template<int> operator F();
};

void foo()
{
  A()(); // { dg-error "no match" }
}
