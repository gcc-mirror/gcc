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
  S v {1, 2};		// { dg-warning "reference to local variable 'v' returned" }
  auto& [s, t] = v;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return s;
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
  int a[3] = {1, 2, 3};	// { dg-warning "reference to local variable 'a' returned" }
  auto& [s, t, u] = a;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return s;
}

int &
f5 ()
{
  auto [s, t] = v;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return s;		// { dg-warning "reference to local variable 's' returned" "" { target *-*-* } .-1 }
}

int &
f6 ()
{
  S v {1, 2};
  auto [s, t] = v;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return s;		// { dg-warning "reference to local variable 's' returned" "" { target *-*-* } .-1 }
}

int &
f7 ()
{
  auto [s, t, u] = a;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return s;		// { dg-warning "reference to local variable 's' returned" "" { target *-*-* } .-1 }
}

int &
f8 ()
{
  int a[3] = {1, 2, 3};
  auto [s, t, u] = a;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return s;		// { dg-warning "reference to local variable 's' returned" "" { target *-*-* } .-1 }
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
  S v {1, 2};		// { dg-warning "address of local variable 'v' returned" }
  auto& [s, t] = v;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return &s;
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
  int a[3] = {1, 2, 3};	// { dg-warning "address of local variable 'a' returned" }
  auto& [s, t, u] = a;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return &s;
}

int *
f13 ()
{
  auto [s, t] = v;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return &s;		// { dg-warning "address of local variable 's' returned" "" { target *-*-* } .-1 }
}

int *
f14 ()
{
  S v {1, 2};
  auto [s, t] = v;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return &s;		// { dg-warning "address of local variable 's' returned" "" { target *-*-* } .-1 }
}

int *
f15 ()
{
  auto [s, t, u] = a;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return &s;		// { dg-warning "address of local variable 's' returned" "" { target *-*-* } .-1 }
}

int *
f16 ()
{
  int a[3] = {1, 2, 3};
  auto [s, t, u] = a;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return &s;		// { dg-warning "address of local variable 's' returned" "" { target *-*-* } .-1 }
}

int
main ()
{
  if (&f1 () != &v.s || &f3 () != &a[0] || f9 () != &v.s || f11 () != &a[0])
    __builtin_abort ();
}
