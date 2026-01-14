// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test invoking functions with a reflection argument.

using info = decltype(^^int);

struct S {
  int mfn0 (info) { return 0; }  // { dg-error "function of consteval-only type must be declared .consteval." }
  constexpr int mfn1 (info) { return 1; }  // { dg-error "function of consteval-only type must be declared .consteval." }
  consteval int mfn2 (info) { return 2; }
  int mfn3 (int, info) { return 0; }  // { dg-error "function of consteval-only type must be declared .consteval." }
  info mfn4 () { return ^^int; }  // { dg-error "consteval-only expressions|function of consteval-only type must be declared .consteval." }
};

void
g (S s)
{
  int i0 = s.mfn0 (^^int);  // { dg-error "consteval-only expressions" }
  constexpr int i1 = s.mfn1 (^^int);
  constexpr int i2 = s.mfn2 (^^int);
  int i3 = s.mfn3 (42, ^^int);  // { dg-error "consteval-only expressions" }
  info i4 = s.mfn4 ();  // { dg-error "consteval-only variable" }
}

template<typename T>
int fn (T) { return 4; } // { dg-error "function of consteval-only type must be declared .consteval." }
const int a = fn (^^int); // { dg-error "consteval-only expressions" }
int b = fn (^^int); // { dg-error "consteval-only expressions" }

template<typename T>
T fn2 () { return ^^void; } // { dg-error "consteval-only" }
const info i = fn2<info>(); // { dg-error "consteval-only variable" }
