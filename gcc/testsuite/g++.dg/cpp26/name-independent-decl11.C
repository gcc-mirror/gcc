// P2169R4 - A nice placeholder with no name
// { dg-do compile { target c++11 } }
// { dg-options "" }

template <typename T>
void
foo ()
{
  {
    auto [i, ..._, k] = T {};	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
				// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } .-1 }
    ++_...[0];			// { dg-warning "pack indexing only available with" "" { target c++23_down } }
  }
  {
    auto [_, ..._, k] = T {};	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
				// { dg-warning "name-independent declarations only available with" "" { target c++23_down } .-1 }
				// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } .-2 }
    ++_;			// { dg-error "reference to '_' is ambiguous" }
  }
  {
    auto [i, ..._, _] = T {};	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
				// { dg-warning "name-independent declarations only available with" "" { target c++23_down } .-1 }
				// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } .-2 }
    ++_;			// { dg-error "reference to '_' is ambiguous" }
  }
  {
    auto [i, ..._, j] = T {};	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
				// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } .-1 }
    auto [k, ..._, l] = T {};	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
				// { dg-warning "name-independent declarations only available with" "" { target c++23_down } .-1 }
				// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } .-2 }
    ++_...[0];			// { dg-error "reference to '_' is ambiguous" }
  }
  {
    static auto [i, ..._, j] = T {};// { dg-warning "structured bindings only available with" "" { target c++14_down } }
				// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } .-1 }
				// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-2 }
    ++_...[0];			// { dg-warning "pack indexing only available with" "" { target c++23_down } }
  }
  {
    static auto [_, ..._, j] = T {};// { dg-error "redeclaration of 'auto _'" }
				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
				// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } .-2 }
				// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-3 }
  }
  {
    static auto [i, ..._, _] = T {};// { dg-error "conflicting declaration 'auto _'" }
				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
				// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } .-2 }
				// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-3 }
  }
}

template <typename T>
void
bar ()
{
  T s[4] = {};
  for (auto [..._, _, a] : s)	// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
				// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } .-2 }
    ++_;			// { dg-error "reference to '_' is ambiguous" }
  for (auto [a, ..._, b] : s)	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
				// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } .-1 }
    ++_...[0];			// { dg-warning "pack indexing only available with" "" { target c++23_down } }
  for (auto [_, ..._, a] : s)	// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    {				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
				// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } .-2 }
      ++_;			// { dg-error "reference to '_' is ambiguous" }
    }
  for (auto [a, ..._, b] : s)	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
    {				// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } .-1 }
      ++_...[0];		// { dg-warning "pack indexing only available with" "" { target c++23_down } }
      int _ = ++b;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
      ++_;			// { dg-error "reference to '_' is ambiguous" }
    }
}

template <typename T>
void
baz ()
{
  T s[4] = {};
  for (auto [a, ..._, _] : s)	// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
				// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } .-2 }
    ++_;			// { dg-error "reference to '_' is ambiguous" }
  for (auto [a, b, ..._] : s)	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
				// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } .-1 }
    ++_...[0];			// { dg-warning "pack indexing only available with" "" { target c++23_down } }
  for (auto [a, ..._, _] : s)	// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    {				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
				// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } .-2 }
      ++_;			// { dg-error "reference to '_' is ambiguous" }
      ++a;
    }
  for (auto [a, b, ..._] : s)	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
    {				// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } .-1 }
      ++_...[0];		// { dg-warning "pack indexing only available with" "" { target c++23_down } }
      int _ = a + b;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
      ++_;			// { dg-error "reference to '_' is ambiguous" }
    }
}

struct A { int a, b, c, d, e; };

namespace std {
  template<typename T> struct tuple_size;
  template<int, typename> struct tuple_element;
}

struct B {
  int a[5];
  template <int I> int &get () { return a[I]; }
};

template<> struct std::tuple_size<B> { static const int value = 5; };
template<int I> struct std::tuple_element<I,B> { using type = int; };

void
qux ()
{
  foo <A> ();
  bar <A> ();
  baz <A> ();
  foo <B> ();
  bar <B> ();
  baz <B> ();
}
