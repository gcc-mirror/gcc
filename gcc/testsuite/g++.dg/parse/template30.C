// PR c++/104319
// { dg-do compile }
// { dg-options "" }

template<typename T> struct A {};
template<typename T> int z;	// { dg-warning "variable templates only available with" "" { target c++11_down } }
template<int N> int w;		// { dg-warning "variable templates only available with" "" { target c++11_down } }

void
foo ()
{
  z<int>=0;			// { dg-error "'>=' should be '> =' to terminate a template argument list" }
}				// { dg-error "expected ';' before numeric constant" "" { target *-*-* } .-1 }

int
bar ()
{
  return z<int>>0;		// { dg-error "spurious '>>', use '>' to terminate a template argument list" "" { target c++98_only } }
}				// { dg-error "expected ';' before numeric constant" "" { target c++98_only } .-1 }

int
baz ()
{
  return z<int>>=0;		// { dg-error "'>>=' should be '> >=' to terminate a template argument list" }
}				// { dg-error "expected ';' before numeric constant" "" { target *-*-* } .-1 }

int
qux ()
{
  return z<A<int>>=0;		// { dg-error "'>>=' should be '>> =' to terminate a template argument list" "" { target c++11 } }
}				// { dg-error "'>>=' should be '> > =' to terminate a template argument list" "" { target c++98_only } .-1 }
				// { dg-error "template argument 1 is invalid" "" { target *-*-* } .-2 }

void
quux ()
{
  w<5>=0>=6>=8> = 5;
}

#if __cplusplus >= 201103L
struct B { constexpr bool operator >= (int) { return true; } };

void
corge ()
{
  w<B()>=5> = 5;
}
#endif
