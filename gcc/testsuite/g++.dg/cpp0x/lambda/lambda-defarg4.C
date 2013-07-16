// PR c++/54764
// { dg-require-effective-target c++11 }

template<class T = void>
struct c
{
  int (*f)(int) = [](int i){return i + i;};
};
