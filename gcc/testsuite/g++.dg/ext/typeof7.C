// PR c++/13635
// { dg-options "" }

template<int n> class X {template<class Y> typeof(Y::y) foo();};
X<0> x;

