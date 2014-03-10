// PR c++/47687
// { dg-do compile { target c++11 } }

template <class T> struct A { };

auto inl = []{ return []{}; }();
typedef decltype(inl) inlt;

A<inlt> a;
