// LWG2296 - addressof should be constexpr
// { dg-do compile { target c++11 } }

template <typename T>
constexpr inline T *
addressof (T &x) noexcept
{
  return __builtin_addressof (x);
}

auto a = __builtin_addressof (1);		// { dg-error "31:lvalue required as unary" }
auto b = addressof (1);				// { dg-error "21:cannot bind non-const lvalue reference of type" }

struct S { int s : 5; int t; void foo (); } s;

auto c = __builtin_addressof (s);
auto d = addressof (s);
auto e = __builtin_addressof (s.s);		// { dg-error "33:attempt to take address of bit-field" }
auto f = addressof (s.s);			// { dg-error "23:cannot bind bit-field" }
auto g = __builtin_addressof (S{});		// { dg-error "31:taking address of rvalue" }
auto h = addressof (S{});			// { dg-error "21:cannot bind non-const lvalue reference of type" }
auto i = __builtin_addressof (S::t);		// { dg-error "34:invalid use of non-static data member" }
auto j = __builtin_addressof (S::foo);		// { dg-error "34:invalid use of non-static member function" }

void
foo (bool b)
{
  lab:;
  char c;
  long long int d;
  auto k = __builtin_addressof (lab);		// { dg-error "33:.lab. was not declared in this scope" }
  auto l = __builtin_addressof (b ? c : d);	// { dg-error "35:lvalue required as unary" }
}
