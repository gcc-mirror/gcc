namespace NS {
    struct C {};
    void foo();
}

template <class T> struct X {};

template <class T> struct A {
    A() { foo (X<T>()); }
    void foo(X<T>);
};
template struct A<NS::C>;
