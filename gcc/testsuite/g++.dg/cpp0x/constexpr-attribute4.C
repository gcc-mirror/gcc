// Verify we correctly handle the non-dependent attribute expression which
// which we used to reject due to double folding.
// { dg-do compile { target { c++11 } } }

struct A {
  constexpr int f() const { return __alignof__(int); };
};

template<class...>
void f() {
  int a __attribute__((aligned(A{}.f())));
}

template void f();
