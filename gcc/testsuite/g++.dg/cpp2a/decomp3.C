// P1381R1
// { dg-do compile { target c++11 } }
// { dg-options "" }

struct Foo { int a : 1; int b; };

int main() {
  auto[a, b] = Foo();			// { dg-warning "structured bindings only available with" "" { target c++14_down } }

  auto f1 = [&] { return a; };		// { dg-error "cannot bind bit-field" }
  auto f2 = [&a = a] { return a; };	// { dg-error "cannot bind bit-field" }
					// { dg-warning "lambda capture initializers only available with" "" { target c++11_only } .-1 }
  auto f3 = [&a] { return a; };		// { dg-error "cannot bind bit-field" }

  auto g1 = [&] { return b; };
  auto g2 = [&b = b] { return b; };	// { dg-warning "lambda capture initializers only available with" "" { target c++11_only } }
  auto g3 = [&b] { return b; };

  auto h1 = [=] { return a; };
  auto h2 = [a = a] { return a; };	// { dg-warning "lambda capture initializers only available with" "" { target c++11_only } }
  auto h3 = [a] { return a; };

  auto i1 = [=] { return b; };
  auto i2 = [b = b] { return b; };	// { dg-warning "lambda capture initializers only available with" "" { target c++11_only } }
  auto i3 = [b] { return b; };
}
