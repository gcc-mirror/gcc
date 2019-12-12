// { dg-do compile { target c++17 } }

template <class T>
struct A
{
  A(T);
};

A a = 42;
A *ap = &a;			// { dg-error "placeholder" }
