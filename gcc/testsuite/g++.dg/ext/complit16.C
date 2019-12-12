// { dg-options "" }

template <int = (struct A{int i;}){42}.i> struct B{}; // { dg-error "" }

B<0> b;
