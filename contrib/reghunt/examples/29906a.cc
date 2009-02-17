struct A{
  typedef int T;
  virtual ~A();
};
struct B:public A{
  using A::T;
};
