// { dg-do run }
// { dg-options "-std=c++1z -fconcepts" }

#include <cassert>
#include <type_traits>

template<typename T>
  concept bool C() { return __is_class(T); }

template<typename T>
  concept bool Type() { return true; }

struct S { };

int called;

// Basic terse notation
void f(auto x) { called = 1; }
void g(C x) { called = 2; }

// Overloading generic functions
void h(auto x) { called = 1; }
void h(C x) { called = 2; }

void p(auto x);
void p(C x);

struct S1 {
  void f1(auto x) { called = 1; }
  void f2(C x) { called = 2; }

  void f3(auto x) { called = 1; }
  void f3(C x) { called = 2; }
};

template<C T>
  struct S2 {
    void f1(auto x) { called = 1; }
    void f2(C x) { called = 2; }

    void f3(auto x) { called = 1; }
    void f3(C x) { called = 2; }

    void h1(auto x);
    void h2(C x);

    void h3(auto x);
    void h3(C x);

    template<C U>
      void g1(T t, U u) { called = 1; }

    template<C U>
      void g2(T t, U u);
  };


void ptr(C*) { called = 1; }
void ptr(const C*) { called = 2; }

void ref(C&) { called = 1; }
void ref(const C&) { called = 2; }

void
fwd_lvalue_ref(Type&& x) {
  using T = decltype(x);
  static_assert(std::is_lvalue_reference<T>::value, "not an lvlaue reference");
}

void
fwd_const_lvalue_ref(Type&& x) {
  using T = decltype(x);
  static_assert(std::is_lvalue_reference<T>::value, "not an lvalue reference");
  using U = typename std::remove_reference<T>::type;
  static_assert(std::is_const<U>::value, "not const-qualified");
}

void fwd_rvalue_ref(Type&& x) {
  using T = decltype(x);
  static_assert(std::is_rvalue_reference<T>::value, "not an rvalue reference");
}

// Make sure we can use nested names speicifers for concept names.
namespace N {
  template<typename T>
    concept bool C() { return true; }
} // namesspace N

void foo(N::C x) { }

int main() {
  S s;
  const S cs;

  f(0); assert(called == 1);
  g(s); assert(called == 2);

  h(0); assert(called == 1);
  h(s); assert(called == 2);

  S1 s1;
  s1.f1(0); assert(called == 1);
  s1.f2(s); assert(called == 2);

  s1.f3(0); assert(called == 1);
  s1.f3(s); assert(called == 2);

  S2<S> s2;
  s2.f1(0); assert(called == 1);
  s2.f2(s); assert(called == 2);

  s2.f3(0); assert(called == 1);
  s2.f3(s); assert(called == 2);

  s2.h1(0); assert(called == 1);
  s2.h2(s); assert(called == 2);

  s2.h3(0); assert(called == 1);
  s2.h3(s); assert(called == 2);

  s2.g1(s, s); assert(called == 1);
  s2.g2(s, s); assert(called == 2);

  ptr(&s); assert(called == 1);
  ptr(&cs); assert(called == 2);

  ref(s); assert(called == 1);
  ref(cs); assert(called == 2);

  // Check forwarding problems
  fwd_lvalue_ref(s);
  fwd_const_lvalue_ref(cs);
  fwd_rvalue_ref(S());

  foo(0);
}

// Test that decl/def matching works.

void p(auto x) { called = 1; }
void p(C x) { called = 2; }

template<C T>
  void S2<T>::h1(auto x) { called = 1; }

template<C T>
  void S2<T>::h2(C x) { called = 2; }

template<C T>
  void S2<T>::h3(auto x) { called = 1; }

template<C T>
  void S2<T>::h3(C x) { called = 2; }

template<C T>
  template<C U>
    void S2<T>::g2(T t, U u) { called = 2; }
