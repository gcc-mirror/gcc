// { dg-additional-options "-fmodules-ts" }
export module foo;
// { dg-module-cmi foo }

template<class T>
struct A {
  friend typename T::type;
  friend void f(A) { }
private:
  static constexpr int value = 42;
};
