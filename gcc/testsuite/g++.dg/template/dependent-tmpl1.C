// PR c++/93085
// { dg-do compile { target c++11 } }

template<class T>
struct G {
    template<T> static int foo();    // #1
    template<int> static int foo();  // #2
    int x = foo<42>();  // OK
};
