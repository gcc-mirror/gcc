// PR c++/18124

template <template <int> class class> class A {}; // { dg-error "" }
