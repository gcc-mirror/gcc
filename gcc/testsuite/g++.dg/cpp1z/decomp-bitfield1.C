// Test of bit-fields.
// { dg-do compile { target c++17 } }

struct A { long i: 2; } a;

template <class,class> struct same_type;
template <class T> struct same_type<T,T> {};

void f()
{
  auto [ x ] = a;

  same_type<decltype(x),long>{};
  same_type<decltype(x+x),int>{};

  long &r = x;			// { dg-error "bit" }
  &x;				// { dg-error "bit" }
  sizeof(x);			// { dg-error "bit" }
}
