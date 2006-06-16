// PR c++/27689

void f (...);
template <template <typename> class F, typename T> void f (F<T>);
template <typename> struct foo { struct bar {}; };
void g (foo<int>::bar x) { f(x); }

