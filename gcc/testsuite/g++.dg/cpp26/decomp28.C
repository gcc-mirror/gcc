// PR c++/120039
// { dg-do compile { target c++11 } }
// { dg-options "" }

struct S {
  int a; long long b; short c;
  explicit operator bool () const noexcept { return true; }
};

template <int N>
void
foo ()
{
  S s = S { 1, 2, 3 };
  if (auto [sx, sy, sz] : s)	// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    ;				// { dg-error "expected initializer before ':' token" "" { target *-*-* } .-1 }
}				// { dg-error "expected '\\\)' before ':' token" "" { target *-*-* } .-2 }

int
main ()
{
  foo <0> ();
}
