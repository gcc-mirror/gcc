// { dg-do assemble  }
// Origin: "Artem Hodyush" <artem@duma.gov.ru>

struct B { int m; };

template< class T >
void
q( T& t ) {
  t.T::m=1;
}

void f() {
  B b;
  b.B::m=1;
  q( b );
}
