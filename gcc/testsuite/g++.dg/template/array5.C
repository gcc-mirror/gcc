// PR c++/15427

template<class T>
struct A
{
  T foo;
};

template<class T>
struct B
{
  A<int> _squares[2];
};

