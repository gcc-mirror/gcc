// PR c++/60108
// { dg-require-effective-target c++11 }

template<int> struct A
{
  virtual ~A();
};

template<typename> struct B : A<0>, A<1>
{
  ~B() = default;
};

struct C : B<bool>
{
  C() {}
};
