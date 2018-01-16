// PR c++/83217
// { dg-do compile { target c++11 } }
// { dg-options "" }

template <typename T>
struct S
{
  T a;
};

void
foo (S<int> *b)
{
  auto & [c] = *b;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
}

void
bar (S<char> *d)
{
  auto [e] = *d;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
}
