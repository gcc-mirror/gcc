// { dg-options -std=c++17 }

template <class T>
struct A
{
  int i;
};

template <class T>
A(T);			       // { dg-error "must have trailing return type" }
