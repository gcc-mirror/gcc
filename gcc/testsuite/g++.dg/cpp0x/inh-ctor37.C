// PR c++/94751
// { dg-do compile }
// { dg-options "-w" }

struct A {
  A(float);
};

template<typename T> 
struct B : A {
  using A::A;

  struct C {
    C(int);
  };

  C c{ "foo" }; // { dg-error "invalid conversion" }
};

struct S { S(B<A> *); };

S
fn ()
{
  return S(new B<A>(10.5));
}
