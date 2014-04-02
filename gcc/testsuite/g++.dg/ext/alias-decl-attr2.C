// { dg-do compile { target c++11 } }

template<typename T>
struct X {
    using layout_type __attribute ((aligned(__alignof(double)))) =
        char[sizeof(T)];
    layout_type data;
};

template<typename T>
struct Y {
    using layout_type  __attribute ((aligned(__alignof(T)))) =
        char[sizeof(T)];
    layout_type data;
};

template<typename T>
struct Z {
    using layout_type __attribute ((aligned(__alignof(T)))) =
        char[sizeof(T)];
    struct Z2 {
        layout_type data;
    } in;
};

template<typename T>
struct A;

template <typename T>
struct A<T*> {
    using layout_type __attribute ((aligned(__alignof(T)))) =
        char[sizeof(T)];
  layout_type data;
};

template<bool> struct StaticAssert;
template<> struct StaticAssert<true> {};

StaticAssert<__alignof(X<double>) == __alignof(double)> d1;
StaticAssert<__alignof(Y<double>) == __alignof(double)> d2;
StaticAssert<__alignof(Z<double>) == __alignof(double)> d3;
StaticAssert<__alignof(A<double*>) == __alignof(double)> d4;
