// { dg-lto-do link }
// { dg-lto-options { { -g -flto } } }
// { dg-extra-ld-options "-r -nostdlib" }

template<typename T> struct Identity { typedef T type; };
struct S {
    typedef void (S::*FP)();
    FP fp;
};
void g();
void f() {
    typedef Identity<S>::type Dummy;
    S s;
    g();
}

