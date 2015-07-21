// PR c++/34485

struct A
{
  template<T::X> struct X; // { dg-error "'T' has not been declared" }
};
