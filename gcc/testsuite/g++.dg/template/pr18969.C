// PR c++/18969

template <typename T>
struct A
{
    int f1 () { return; } // { dg-error "return-statement" }
    void f2 () { return 5; } // { dg-error "return-statement" }
    T *f3 () { return; } // { dg-error "return-statement" }
    typename T::f &f4 () { return; } // { dg-error "return-statement" }

    T f5 () { return; } // { dg-bogus "return-statement" }
    void f6 () { return (T)true; } // { dg-bogus "return-statement" }
    typename T::f f7 () { return; } // { dg-bogus "return-statement" }
};
