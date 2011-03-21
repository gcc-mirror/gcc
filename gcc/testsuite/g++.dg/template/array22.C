// PR c++/48029

template <class T> struct A { };
template <class T, class U> struct B
{
  struct N { };
  typedef U u;
};

typedef B<int, A<int>(*)[2]> btype;
A<int> v1[2];
btype v2;


