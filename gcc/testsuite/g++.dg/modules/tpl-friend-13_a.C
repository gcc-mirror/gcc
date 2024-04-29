// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi M }

export module M;
export template <typename> struct A {
  friend struct S;
  template <typename> friend struct T;
};

export template <typename> struct B {
  friend void f();
  template <typename> friend void g();
};
