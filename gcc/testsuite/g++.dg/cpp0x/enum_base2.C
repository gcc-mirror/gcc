// PR c++/60187
// { dg-require-effective-target c++11 }

template<typename... T> struct A
{
  enum E : T {};		// { dg-error "parameter pack" }
};

A<int> a;
