// PR c++/25663

template<int> struct A
{
  A(int);
};

void foo()
{
  A<0>(A<0>(0));
}
