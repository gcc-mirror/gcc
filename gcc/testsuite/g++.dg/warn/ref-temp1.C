// PR c++/986
// { dg-options "-Wall -Wextra" }

struct X { X (int); };

struct Y {
  Y ();
  const X &x;			// note the ampersand
};

Y::Y () : x(1) {}		// { dg-warning "temporary" }
