// { dg-do compile }
// Origin:

// PR c++/6716
// ICE in complex class structure when components are incomplete

template <class T> struct X {
  T t;				// { dg-error "incomplete" }
};

template <class T> struct Y {	// { dg-error "instantiated" }
  X<T> x;
};

template <class T> struct Z {	// { dg-error "instantiated|declaration" }
  Y<Z<T> > y;
};

struct ZZ : Z<int>
{				// { dg-error "instantiated" }
};
