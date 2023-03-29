// P1169R4 - static operator()
// { dg-do compile { target c++11 } }
// { dg-options "" }

void
foo ()
{
  int u = 0;
  auto a = [](int x, int y) mutable mutable { return x + y; };		// { dg-error "duplicate 'mutable' specifier" }
  auto b = [](int x, int y) static static { return x + y; };		// { dg-error "duplicate 'static' specifier" }
									// { dg-warning "'static' only valid in lambda with" "" { target c++20_down } .-1 }
  auto c = [](int x, int y) static mutable { return x + y; };		// { dg-error "'mutable' specifier conflicts with 'static'" }
									// { dg-warning "'static' only valid in lambda with" "" { target c++20_down } .-1 }
  auto d = [](int x, int y) mutable static { return x + y; };		// { dg-error "'static' specifier conflicts with 'mutable'" }
									// { dg-warning "'static' only valid in lambda with" "" { target c++20_down } .-1 }
  auto e = [=](int x, int y) static { return x + y; };			// { dg-error "lambda specifier with lambda capture" }
									// { dg-warning "'static' only valid in lambda with" "" { target c++20_down } .-1 }
  auto f = [&](int x, int y) static { return x + y; };			// { dg-error "lambda specifier with lambda capture" }
									// { dg-warning "'static' only valid in lambda with" "" { target c++20_down } .-1 }
  auto g = [u](int x, int y) static { return x + y; };			// { dg-error "lambda specifier with lambda capture" }
									// { dg-warning "'static' only valid in lambda with" "" { target c++20_down } .-1 }
}
