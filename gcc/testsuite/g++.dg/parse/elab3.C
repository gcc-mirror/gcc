// PR c++/87781
// { dg-do compile }
// { dg-options "" }

template<class> class A;
class template A<int> *p; // { dg-error ".template. must follow|invalid" }
