// PR c++/47687
// { dg-options -std=c++0x }

template <class T> struct A { };

auto inl = []{ return []{}; }();
typedef decltype(inl) inlt;

A<inlt> a;
