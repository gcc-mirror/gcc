// { dg-options "" }

int foo;

template <class T> struct Base {};

template <class T>
struct Derived : public Base<typeof(foo)> {};
