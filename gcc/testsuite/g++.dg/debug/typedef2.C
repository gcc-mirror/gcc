// PR c++/17695

template<typename T> struct A
{
  T t;
  A();
};

struct B
{
  B() { typedef int C; A<C> a; }
} b;
