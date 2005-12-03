// PR c++/24173
// { dg-options "--param ggc-min-expand=0 --param ggc-min-heapsize=0" }

template <int> struct A;

void foo(A<0>);

template<int> struct A
{
  friend void foo(A<0>);
};

void bar()
{
  foo(A<0>());
}
