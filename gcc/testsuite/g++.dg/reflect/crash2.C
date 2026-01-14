// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

struct S {
  template<typename T>
  static void tfn () { }
  template<typename T>
  static constexpr int var = 1;
};

template<typename T>
void
f ()
{
  // Parsed as "S::tfn < S".
  [: ^^T :]::tfn<[: ^^T :]>();	// { dg-error "expected primary-expression|expected a reflection of an expression instead of type .S." }
// { dg-warning "expected .template. keyword" "" { target *-*-* } .-1 }
  int i = [: ^^T :]::var<int>;	// { dg-error "missing|expected" }
}

void
g ()
{
  f<S>();
}
