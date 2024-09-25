// PR c++/115231
// { dg-additional-options "-fmodules-ts -Wno-global-module" }
// { dg-module-cmi M }

module;

template <typename T>
struct A {
  template <typename U> A(U);
};

template <typename T> A(T) -> A<T>;

export module M;

// Exporting a GMF entity should make the deduction guides reachable.
export using ::A;


export template <typename T>
struct B {
  template <typename U> B(U);
};

// Not exported, but should still be reachable by [temp.deduct.guide] p1.
B(int) -> B<double>;


// Class-scope deduction guides should be reachable as well, even if
// the class body was not exported.
export template <typename T> struct C;

template <typename T>
struct C {
  template <typename U>
  struct I {
    template <typename V> I(V);
  };

  I(int) -> I<int>;

  template <typename P>
  I(const P*) -> I<P>;
};
