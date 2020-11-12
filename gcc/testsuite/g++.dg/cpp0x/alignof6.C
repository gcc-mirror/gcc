// PR c++/88115
// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wno-pedantic" }

// Verify the non-standard extension alignof(expr) behaves like
// alignof(type) to yield the ABI alignment of the type, and that
// __alignof__(expr) behaves like __alignof__(type) to yield the
// preferred alignment of the type.

static_assert(alignof(double{}) == alignof(double), "");
static_assert(__alignof__(double{}) == __alignof__(double), "");

template <class T>
void f() {
  static_assert(alignof(T{}) == alignof(T), "");
  static_assert(__alignof__(T{}) == __alignof__(T), "");
}

template void f<double>();
