// PR c++/115586
// { dg-do compile { target c++11 } }
// { dg-options "" }

struct S { int a, b, c; };

void
foo ()
{
  S s[4] = {};
  for (auto [_, _, a] : s)	// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    ++_;			// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
				// { dg-error "reference to '_' is ambiguous" "" { target *-*-* } .-1 }
  for (auto _ : s)
    ++_.b;
  for (auto [a, _, b] : s)	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
    ++_;
  for (auto [_, _, a] : s)	// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    {				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
      ++_;			// { dg-error "reference to '_' is ambiguous" }
    }
  for (auto _ : s)
    {
      ++_.b;
      int _ = 2;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
      ++_;			// { dg-error "reference to '_' is ambiguous" }
    }
  for (auto [a, _, b] : s)	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
    {
      ++_;
      int _ = ++b;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
      ++_;			// { dg-error "reference to '_' is ambiguous" }
    }
}

void
bar ()
{
  S s[4] = {};
  auto [_, c, _] = s[0];	// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
  ++_;				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
				// { dg-error "reference to '_' is ambiguous" "" { target *-*-* } .-1 }
  for (auto [a, _, _] : s)	// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    ++_;			// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
				// { dg-error "reference to '_' is ambiguous" "" { target *-*-* } .-1 }
  for (auto _ : s)
    ++_.c;
  for (auto [a, b, _] : s)	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
    ++_;
  for (auto [a, _, _] : s)	// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    {				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
      ++_;			// { dg-error "reference to '_' is ambiguous" }
      ++a;
    }
  for (auto _ : s)
    int _ = 5;			// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
  for (auto [a, b, _] : s)	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
    {
      ++_;
      int _ = a + b;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
      ++_;			// { dg-error "reference to '_' is ambiguous" }
    }
}
