// PR c++/95328
// { dg-do compile { target c++11 } }
// { dg-options "" }

template <typename T>
struct S
{
  int a, b;
};

template <typename T>
void
foo ()
{
  auto [a, b] = S<int>();	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
}

int
main ()
{
  foo<int> ();
}
