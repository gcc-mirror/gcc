// { dg-do compile }

// Origin: pepeaty@yahoo.com

// PR c++/10230: ICE while determining if refered non-static member
// is from a base type of the current class.

class A {
public:
  class B {
  public:
    int a;			// { dg-error "object missing" }
  };
};

class C {
public:
  void f(void) { sizeof(A::B::a); } // { dg-error "this location" }
};
