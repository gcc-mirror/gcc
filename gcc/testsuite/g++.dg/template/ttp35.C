// PR c++/108179

template <class T, T X, template <T> class F>
struct Foo {};

template <class T0, class T1, T1 X, template <T1> class F>
void f(Foo<T1, X, F>) {}
