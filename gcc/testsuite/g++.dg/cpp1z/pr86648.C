// { dg-do compile { target c++17 } }

template <typename> class A;
template <class T> struct B { static A a{T::a}; };
void foo () { B<int> a; }
