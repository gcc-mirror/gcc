// PR c++/13950

template <class T> struct Base {};
template <class T> struct Derived: public Base<T> {
  typename Derived::template Base<double>* p1; // { dg-error "" }
};
