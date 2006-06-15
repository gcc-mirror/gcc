// PR c++/27665

template<int> struct A
{
    struct B
    {
      struct C {};
    };
};

template<int N> void foo()
{
  class A<N>::B::C X;
}

void bar()
{
  foo<0>();
}
