// { dg-do compile }
// Origin:

// PR c++/6716
// ICE in complex class structure when components are incomplete

template <class T> struct X {
  T t;				// { dg-error "incomplete" }
};

template <class T> struct Y {
  X<T> x;			// { dg-message "instantiated" }
};

template <class T> struct Z {	// { dg-error "declaration" }
  Y<Z<T> > y;			// { dg-message "instantiated" }
};

struct ZZ : Z<int>
{
};
