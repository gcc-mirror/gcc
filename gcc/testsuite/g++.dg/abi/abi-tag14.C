// { dg-options "-Wabi-tag" }

inline namespace __cxx11 __attribute ((abi_tag ("cxx11"))) {
  struct A {};
};

// { dg-final { scan-assembler "_Z1aB5cxx11" } }
A a;				// { dg-warning "\"cxx11\"" }

// { dg-final { scan-assembler "_Z1fB5cxx11v" } }
A f() {}			// { dg-warning "\"cxx11\"" }

namespace {
  A a2;
  A f2() {}
  struct B: A {};
}

// { dg-final { scan-assembler "_Z1fPN7__cxx111AE" } }
A f(A*) {}

// { dg-final { scan-assembler "_Z1gIN7__cxx111AEET_v" } }
template <class T> T g() { }
template <> A g<A>() { }

// { dg-final { scan-assembler "_Z1vIN7__cxx111AEE" { target c++14 } } }
#if __cplusplus >= 201402L
template <class T> T v = T();
void *p = &v<A>;
#endif
