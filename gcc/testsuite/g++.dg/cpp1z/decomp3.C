// { dg-do compile { target c++11 } }
// { dg-options "" }

struct A { int a, b; float c; };
A &bar ();
struct B { int d; };
B baz ();

void
test (A &b, B c)
{
  int && [ d ] = c;			// { dg-error "decomposition declaration cannot be declared with type 'int'" }
					// { dg-warning "decomposition declaration only available with -std=c..1z or -std=gnu..1z" "" { target c++14_down } .-1 }
  char & [ e, f, ff ] { b };		// { dg-error "decomposition declaration cannot be declared with type 'char'" }
					// { dg-warning "decomposition declaration only available with -std=c..1z or -std=gnu..1z" "" { target c++14_down } .-1 }
  auto&[g,h,i]=b;			// { dg-warning "decomposition declaration only available with -std=c..1z or -std=gnu..1z" "" { target c++14_down } }
  decltype (auto) [ j ] = c;		// { dg-error "decomposition declaration cannot be declared with type 'decltype.auto.'" "" { target c++14 } }
					// { dg-warning "decomposition declaration only available with -std=c..1z or -std=gnu..1z" "" { target c++14_down } .-1 }
					// { dg-error "expected primary-expression before 'decltype'" "" { target c++11_down } .-2 }
  auto & & && & [ m, n, o ] = b;	// { dg-error "multiple ref-qualifiers" }
					// { dg-warning "decomposition declaration only available with -std=c..1z or -std=gnu..1z" "" { target c++14_down } .-1 }
  constexpr auto [ p ] = c;		// { dg-error "decomposition declaration cannot be declared 'constexpr'" }
					// { dg-warning "decomposition declaration only available with -std=c..1z or -std=gnu..1z" "" { target c++14_down } .-1 }
  friend auto [ q ] = c;		// { dg-error "'friend' used outside of class" }
					// { dg-warning "decomposition declaration only available with -std=c..1z or -std=gnu..1z" "" { target c++14_down } .-1 }
  typedef auto [ r ] = c;		// { dg-error "decomposition declaration cannot be declared 'typedef'" }
					// { dg-warning "decomposition declaration only available with -std=c..1z or -std=gnu..1z" "" { target c++14_down } .-1 }
  inline auto [ s ] = c;		// { dg-error "decomposition declaration cannot be declared 'inline'" }
					// { dg-warning "decomposition declaration only available with -std=c..1z or -std=gnu..1z" "" { target c++14_down } .-1 }
  __restrict auto [ t ] = c;		// { dg-error "invalid use of 'restrict'" }
					// { dg-warning "decomposition declaration only available with -std=c..1z or -std=gnu..1z" "" { target c++14_down } .-1 }
  long long auto [ u ] = c;		// { dg-error "'long long' invalid for 'decomposition'" }
					// { dg-warning "decomposition declaration only available with -std=c..1z or -std=gnu..1z" "" { target c++14_down } .-1 }
  virtual auto [ v ] = c;		// { dg-error "'virtual' outside class declaration" }
					// { dg-warning "decomposition declaration only available with -std=c..1z or -std=gnu..1z" "" { target c++14_down } .-1 }
  explicit auto [ w ] = c;		// { dg-error "'explicit' outside class declaration" }
					// { dg-warning "decomposition declaration only available with -std=c..1z or -std=gnu..1z" "" { target c++14_down } .-1 }
  static auto [ x ] = c;		// { dg-error "decomposition declaration cannot be declared 'static'" }
					// { dg-warning "decomposition declaration only available with -std=c..1z or -std=gnu..1z" "" { target c++14_down } .-1 }
  extern auto [ y ] { c };		// { dg-error "decomposition declaration cannot be declared 'extern'" }
					// { dg-warning "decomposition declaration only available with -std=c..1z or -std=gnu..1z" "" { target c++14_down } .-1 }
}

void
test2 (auto & [ p ] = bar ())		// { dg-error "'p' was not declared in this scope" }
{
}

int arr[4];

void
test3 (A &b, B c)
{
  auto [ d, e, f ] = arr;		// { dg-error "only 3 names provided while 'int .4.' decomposes into 4 elements" }
					// { dg-warning "decomposition declaration only available with -std=c..1z or -std=gnu..1z" "" { target c++14_down } .-1 }
  auto & [ g, h, i, j, k ] = arr;	// { dg-error "5 names provided while 'int .4.' decomposes into 4 elements" }
					// { dg-warning "decomposition declaration only available with -std=c..1z or -std=gnu..1z" "" { target c++14_down } .-1 }
  auto [ l, m ] = b;			// { dg-error "only 2 names provided while 'A' decomposes into 3 elements" }
					// { dg-warning "decomposition declaration only available with -std=c..1z or -std=gnu..1z" "" { target c++14_down } .-1 }
  auto & [ n, o, p, q ] = b;		// { dg-error "4 names provided while 'A' decomposes into 3 elements" }
					// { dg-warning "decomposition declaration only available with -std=c..1z or -std=gnu..1z" "" { target c++14_down } .-1 }
  auto [] { c };			// { dg-error "empty decomposition declaration" }
					// { dg-warning "decomposition declaration only available with -std=c..1z or -std=gnu..1z" "" { target c++14_down } .-1 }
  auto [ r, s ] = c;			// { dg-error "2 names provided while 'B' decomposes into 1 elements" }
					// { dg-warning "decomposition declaration only available with -std=c..1z or -std=gnu..1z" "" { target c++14_down } .-1 }
}
