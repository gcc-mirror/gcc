namespace N {
template<class T> struct A { template<class U> void f(); };
template<> template<class U> void A<int>::f() { }
}
