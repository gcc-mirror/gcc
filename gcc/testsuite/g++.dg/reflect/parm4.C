// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test invoking functions with a reflection argument.

using info = decltype(^^int);

struct S {
  int mfn0 (info) { return 0; }
  constexpr int mfn1 (info) { return 1; }
  consteval int mfn2 (info) { return 2; }
  int mfn3 (int, info) { return 0; }
  info mfn4 () { return ^^int; }    // { dg-error "consteval-only value" }
};

void
g (S s)
{
  int i0 = s.mfn0 (^^int);  // { dg-error "consteval-only value" }
  constexpr int i1 = s.mfn1 (^^int);
  constexpr int i2 = s.mfn2 (^^int);
  int i3 = s.mfn3 (42, ^^int);  // { dg-error "consteval-only value" }
  info i4 = s.mfn4 ();
}

template<typename T>
int fn (T) { return 4; }
const int a = fn (^^int); // { dg-error "consteval-only value" }
int b = fn (^^int); // { dg-error "consteval-only value" }

template<typename T>
T fn2 () { return ^^void; } // { dg-error "consteval-only" }
const info i = fn2<info>();
