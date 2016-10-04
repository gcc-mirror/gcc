// { dg-options -std=c++1z }

template <class T>
struct A
{
  int i;
};

template <class T>
A(T);			       // { dg-error "must have trailing return type" }
