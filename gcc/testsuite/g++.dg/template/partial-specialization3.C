// DR 2179

template <class T1, class T2> class A;
template <class T> struct A<T, void> { void f(); };
template <class T> void g(T) { A<char, void>().f(); }   // #1
template<typename T> struct A<char, T> {};		// { dg-error "" }
A<char, void> f;   // #2
