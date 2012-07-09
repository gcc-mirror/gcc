// PR c++/11987

template <int dim> struct X {
  struct I { I(); };
};

template <int dim> struct Y : X<dim> {
  typedef typename X<dim>::I I;
};

// note: I is nested type in X, not Y!
template <int dim>
Y<dim>::I::I () {}		// { dg-error "dependent typedef" "typedef" }
// { dg-error "no type|dependent type" "no type" { target *-*-* } 13 }

template struct Y<1>;
