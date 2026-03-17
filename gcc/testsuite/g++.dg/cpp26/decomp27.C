// PR c++/124456
// { dg-do compile { target c++14 } }
// { dg-options "" }

struct A { int i; };

template <typename... T>
A
foo (T...)
{
  return A ();
}

template <typename... T>
int
bar (T...)
{
  return 0;
}

template <typename... T>
auto
baz (T... t)
{
  return [=] (auto... a) {
    auto [... k] = foo (t..., a...);	// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
    return bar (k...); };		// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
}

auto x = baz (1);
auto y = x (1, 2, 3);
