// PR c++/115403
// { dg-do compile { target x86_64-*-* } }

template <typename> __attribute__((always_inline)) inline void AssertEqual() {}
void TestAllF16FromF32() { AssertEqual<float>(); }
#pragma GCC target "sse4.1"
