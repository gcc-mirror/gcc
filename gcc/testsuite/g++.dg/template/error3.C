// PR 12762

template <typename> struct A { A() {}};
typedef A<int> Ac;
Ac<double> a; // { dg-error "template" }
