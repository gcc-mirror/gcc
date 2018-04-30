// PR c++/60063
// { dg-options -Wunused-local-typedefs }

template <class> struct S;

void foo (int i) {
    typedef __attribute__ ((used)) S<int> X;
}

template <class T>
void bar (T i) {
    typedef __attribute__ ((used)) S<T> Y;
}
