// PR c++/98103
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fsanitize=vptr -fno-sanitize-recover=vptr" }
// Modified constexpr-dynamic17.C.

struct V {
  virtual void f();
};

struct A : V { };

struct B : V {
  constexpr B(V*, A*);
};

struct D : B, A {
  constexpr D() : B((A*)this, this) { }
};

constexpr B::B(V* v, A* a)
{
  dynamic_cast<B*>(a); // { dg-error "uninitialized" }
}

constexpr D d;
