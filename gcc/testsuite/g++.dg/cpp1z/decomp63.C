// PR c++/118719
// { dg-do compile { target c++11 } }
// { dg-options "" }

int
main ()
{
  int a[] = { 42 };
  auto [x] = a;					// { dg-warning "structured bindings only available with" "" { target c++14_down } }
						// { dg-message "declared here" "" { target c++17_down } .-1 }
  [=] () { int b = x; (void) b; };		// { dg-warning "captured structured bindings are a C\\\+\\\+20 extension" "" { target c++17_down } }
  [&] () { int b = x; (void) b; };		// { dg-warning "captured structured bindings are a C\\\+\\\+20 extension" "" { target c++17_down } }
  [x] () { int b = x; (void) b; };		// { dg-warning "captured structured bindings are a C\\\+\\\+20 extension" "" { target c++17_down } }
  [&x] () { int b = x; (void) b; };		// { dg-warning "captured structured bindings are a C\\\+\\\+20 extension" "" { target c++17_down } }
  [x = x] () { int b = x; (void) b; };		// { dg-warning "lambda capture initializers only available with" "" { target c++11_only } }
  [y = x] () { int b = y; (void) b; };		// { dg-warning "lambda capture initializers only available with" "" { target c++11_only } }
  [y = x * 2] () { int b = y; (void) b; };	// { dg-warning "lambda capture initializers only available with" "" { target c++11_only } }
}
