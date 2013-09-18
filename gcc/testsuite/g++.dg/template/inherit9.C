// PR c++/58273

class A {};
class B
{
  int goo(A);
};
template<typename E>
class D : public B
{
  void foo(A t)
  {
    int const i(B::goo(t));
  }
};
