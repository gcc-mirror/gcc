// PR c++/87582
// { dg-do run { target c++11 } }
// { dg-options "-Wreturn-local-addr" }

struct S { int s, t; };
S v {1, 2};
int a[3] = {1, 2, 3};

int &
f1 ()
{
  auto& [s, t] = v;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return s;		// { dg-bogus "reference to local variable '.' returned" }
}

int &
f2 ()
{
  S v {1, 2};
  auto& [s, t] = v;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return s;		// { dg-warning "reference to local variable 'v' returned" }
}

int &
f3 ()
{
  auto& [s, t, u] = a;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return s;		// { dg-bogus "reference to local variable '.' returned" }
}

int &
f4 ()
{
  int a[3] = {1, 2, 3};
  auto& [s, t, u] = a;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return s;		// { dg-warning "reference to local variable 'a' returned" }
}

int &
f5 ()
{
  auto [s, t] = v;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return s;		// { dg-warning "reference to local variable 's' returned" }
}

int &
f6 ()
{
  S v {1, 2};
  auto [s, t] = v;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return s;		// { dg-warning "reference to local variable 's' returned" }
}

int &
f7 ()
{
  auto [s, t, u] = a;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return s;		// { dg-warning "reference to local variable 's' returned" }
}

int &
f8 ()
{
  int a[3] = {1, 2, 3};
  auto [s, t, u] = a;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return s;		// { dg-warning "reference to local variable 's' returned" }
}

int *
f9 ()
{
  auto& [s, t] = v;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return &s;		// { dg-bogus "address of local variable '.' returned" }
}

int *
f10 ()
{
  S v {1, 2};
  auto& [s, t] = v;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return &s;		// { dg-warning "address of local variable 'v' returned" }
}

int *
f11 ()
{
  auto& [s, t, u] = a;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return &s;		// { dg-bogus "address of local variable '.' returned" }
}

int *
f12 ()
{
  int a[3] = {1, 2, 3};
  auto& [s, t, u] = a;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return &s;		// { dg-warning "address of local variable 'a' returned" }
}

int *
f13 ()
{
  auto [s, t] = v;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return &s;		// { dg-warning "address of local variable 's' returned" }
}

int *
f14 ()
{
  S v {1, 2};
  auto [s, t] = v;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return &s;		// { dg-warning "address of local variable 's' returned" }
}

int *
f15 ()
{
  auto [s, t, u] = a;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return &s;		// { dg-warning "address of local variable 's' returned" }
}

int *
f16 ()
{
  int a[3] = {1, 2, 3};
  auto [s, t, u] = a;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return &s;		// { dg-warning "address of local variable 's' returned" }
}

int
main ()
{
  if (&f1 () != &v.s || &f3 () != &a[0] || f9 () != &v.s || f11 () != &a[0])
    __builtin_abort ();
}
