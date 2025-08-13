// PR c++/120776
// { dg-do compile { target c++11 } }
// { dg-options "" }

extern int b[];

void
foo (int n)
{
  int a[n];
  a[0] = 42;
  auto [x] = a;					// { dg-warning "structured bindings only available with" "" { target c++14_down } }
						// { dg-error "cannot decompose variable length array 'int \\\[n\\\]'" "" { target *-*-* } .-1 }
  auto [y] = b;					// { dg-warning "structured bindings only available with" "" { target c++14_down } }
						// { dg-error "deduced type 'int \\\[\\\]' for '<structured bindings>' is incomplete" "" { target *-*-* } .-1 }
}
