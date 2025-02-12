// PR c++/115586
// { dg-do compile { target c++11 } }
// { dg-options "-Wunused-variable -Wunused-but-set-variable -Wunused-parameter -Wshadow" }

struct S { int a, b, c; };

void
foo ()
{
  S s[4] = {};
  for (auto [_, _, a] : s)	// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    ++a;			// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  for (auto _ : s)
    ++_.b;
  for (auto [a, _, b] : s)	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
    ++a;
  for (auto [_, _, a] : s)	// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    int _ = ++a;		// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  for (auto _ : s)
    {
      int _ = 2;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    }
  for (auto [a, _, b] : s)	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
    int _ = ++b;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
}

void
bar ()
{
  S s[4] = {};
  auto [_, c, _] = s[0];	// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
  ++c;				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  for (auto [a, _, _] : s)	// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    ++a;			// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  for (auto _ : s)
    ++_.c;
  for (auto [a, b, _] : s)	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
    ++b;
  for (auto [a, _, _] : s)	// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    {				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
      int _ = ++a;
    }
  for (auto _ : s)
    int _ = 5;			// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
  for (auto [a, b, _] : s)	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
    {
      int _ = a + b;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    }
}
