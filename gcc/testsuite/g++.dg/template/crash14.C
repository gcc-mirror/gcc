template <int T> class foo { public: foo() { } class Z { };};
template <int I[2]> void dep7(foo<I[0]> *) { } // { dg-error "" }

