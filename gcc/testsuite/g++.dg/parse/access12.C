// Example 2 of [class.access.general]
// { dg-do compile }

class A {
  typedef int I;    // private member
  I f();
  friend I g(I);
  static I x;
  template<int> struct Q;
  template<int> friend struct R;
protected:
    struct B { };
};

A::I A::f() { return 0; }
A::I g(A::I p = A::x);
A::I g(A::I p) { return 0; }
A::I A::x = 0;
// FIXME: We reject these two declarations because access checking of A::I
// is not performed in the scope of the class being declared.
// template<A::I> struct A::Q { };
// template<A::I> struct R { };

struct D: A::B, A { };
