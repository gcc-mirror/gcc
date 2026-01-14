// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test reflections and pack indexing.

struct S {
  int x;
  static int sx;
  using type = int;
};

template<typename... Ts>
void
f ()
{
  constexpr auto r1 = ^^Ts...[0]::sx;
  constexpr auto r2 = ^^typename Ts...[0]::type;

  // NTTPs and pack-index-expressions cannot appear as operands
  // of the reflection operator.
  constexpr auto e = ^^Ts...[0]; // { dg-error "cannot be applied" }
}


void
g ()
{
  f<S>();
}
