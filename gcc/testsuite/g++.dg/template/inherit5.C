struct A
{
  template<int> void foo();
};

template<int N> struct B : A
{
  B() { foo<N>(); }
};

B<0> b;
