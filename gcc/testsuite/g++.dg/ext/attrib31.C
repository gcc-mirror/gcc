// PR c++/35097

template<int> struct A;

template<> struct A<0>
{
  typedef int X __attribute((aligned(4)));
};

template<typename T> void foo(const A<0>::X&, T);

void bar()
{
  foo(A<0>::X(), 0);
}
