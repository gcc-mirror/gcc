// PR c++/85952
// { dg-do compile { target c++11 } }
// { dg-options "-Wunused-but-set-variable" }

int
foo ()
{
  int a[2] = {1, 2};	// { dg-bogus "set but not used" } */
  auto [x, y] = a;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return x + y;
}

struct S { int d, e; };

int
bar ()
{
  S a = {1, 2};
  auto [x, y] = a;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return x + y;
}

int
baz ()
{
  S a = {1, 2};
  auto & [x, y] = a;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return x + y;
}

int
qux ()
{
  int a[2] = {1, 2};
  auto & [x, y] = a;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return x + y;
}
