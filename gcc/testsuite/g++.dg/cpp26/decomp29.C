// PR c++/122559
// { dg-do compile { target c++11 } }
// { dg-options "" }

enum class A : bool { B, C };

template <typename T>
A type (T &&x)
{
  if (auto [value = x ()])	// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    return A::C;		// { dg-error "expected '\\\]' before '=' token" "" { target *-*-* } .-1 }
  return A::B;			// { dg-error "expected initializer before '\\\)' token" "" { target *-*-* } .-2 }
}

int
main ()
{
  auto _ = type (A::B);
}
