// PR c++/84447
// { dg-do compile { target c++11 } }

struct A
{
  template<typename T> A(T, T = 0) = delete;
};

struct B : A
{
  using A::A;			// { dg-error "deleted" }
};

B b(0);				// { dg-error "deleted" }
