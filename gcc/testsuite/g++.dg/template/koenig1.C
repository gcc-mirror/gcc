namespace NS {
    struct C {};
    void foo(C);
}

template <class T> void bar() { T t; foo (t); }

template void bar<NS::C> ();
